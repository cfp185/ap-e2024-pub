module SPC
  ( -- * SPC startup
    SPC,
    startSPC,

    -- * Job functions
    Job (..),
    JobId,
    JobStatus (..),
    JobDoneReason (..),
    jobAdd,
    jobStatus,
    jobWait,
    jobCancel,

    -- * Worker functions
    WorkerName,
    Worker (..),
    workerAdd,
    workerStop,
  )
where

import Control.Concurrent
  ( forkIO,
    killThread,
    threadDelay,
    ThreadId,
  )
import Control.Exception (SomeException, try)
import Control.Monad (ap, forever, liftM, void, forM_)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- | Retrieve Unix time using a monotonic clock.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle = filter (\(k, _) -> k /= needle)

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

-- | How a job finished.
data JobDoneReason
  = -- | Normal termination.
    Done
  | -- | The job was killed because it ran for too long.
    DoneTimeout
  | -- | The job was explicitly cancelled, or the worker
    -- it was running on was stopped.
    DoneCancelled
  | -- | The job crashed due to an exception.
    DoneCrashed
  deriving (Eq, Ord, Show)

-- | The status of a job.
data JobStatus
  = -- | The job is done and this is why.
    JobDone JobDoneReason
  | -- | The job is still running.
    JobRunning
  | -- | The job is enqueued, but is waiting for an idle worker.
    JobPending
  | -- | A job with this ID is not known to this SPC instance.
    JobUnknown
  deriving (Eq, Ord, Show)

-- | A worker decides its own human-readable name. This is useful for
-- debugging.
type WorkerName = String

-- | Messages sent to workers.
data WorkerMsg
  = WorkerAssignJob JobId Job
  | WorkerCancelJob
  | WorkerTerminate
  | WorkerJobDone
  | WorkerJobCrashed
  | WorkerTimedOut


-- Messages sent to SPC.
data SPCMsg
  = MsgJobAdd Job (ReplyChan JobId)
  | MsgJobCancel JobId
  | MsgJobStatus JobId (ReplyChan JobStatus)
  | MsgJobWait JobId (ReplyChan JobDoneReason)
  | MsgTick
  | MsgWorkerAdd WorkerName (ReplyChan (Either String Worker))
  -- Messages from workers
  | MsgWorkerIdle WorkerName
  | MsgWorkerJobDone WorkerName JobId JobDoneReason
  | MsgWorkerTerminated WorkerName

-- | A handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | A handle to a worker.
data Worker = Worker
  { workerName :: WorkerName,
    workerServer :: Server WorkerMsg
  }

-- Add a Show instance for Worker
instance Show Worker where
  show (Worker name _) = "Worker {workerName = " ++ show name ++ "}"

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobsPending :: [(JobId, Job)],
    spcJobsRunning :: [(JobId, Job)],
    spcJobsDone :: [(JobId, JobDoneReason)],
    spcJobCounter :: JobId,
    spcWorkersIdle :: [(WorkerName, Worker)],
    spcWorkersBusy :: [(WorkerName, (Worker, JobId, Seconds))],
    spcWaiting :: [(JobId, ReplyChan JobDoneReason)]
  }

-- | The monad in which the main SPC thread runs. This is a state
-- monad with support for IO.
newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

instance Functor SPCM where
  fmap = liftM

instance Applicative SPCM where
  pure x = SPCM $ \state -> pure (x, state)
  (<*>) = ap

instance Monad SPCM where
  SPCM m >>= f = SPCM $ \state -> do
    (x, state') <- m state
    let SPCM f' = f x
    f' state'

-- | Retrieve the state.
get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

-- | Modify the state.
-- NOTE: we decided to do this manually ourselves, since it enhanced a better understanding.
-- modify :: (SPCState -> SPCState) -> SPCM ()
-- modify f = do
  -- state <- get
  -- put $ f state

-- | Lift an 'IO' action into 'SPCM'.
io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

-- | Run the SPCM monad.
runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

schedule :: SPCM ()
schedule = do
  state <- get
  let pendingJobs = spcJobsPending state
  let idleWorkers = spcWorkersIdle state
  if null pendingJobs || null idleWorkers
    then return ()
    else do
      case (pendingJobs, idleWorkers) of
        ((jobId, job):restJobs, (wName, worker):restWorkers) -> do
          io $ sendTo (workerServer worker) $ WorkerAssignJob jobId job
          now <- io getSeconds
          let deadline = now + fromIntegral (jobMaxSeconds job)
          put $ state
            { spcJobsPending = restJobs,
              spcJobsRunning = (jobId, job) : spcJobsRunning state,
              spcWorkersIdle = restWorkers,
              spcWorkersBusy = (wName, (worker, jobId, deadline)) : spcWorkersBusy state
            }
          schedule
        _ -> return ()  -- This handles any unexpected empty lists


jobDone :: JobId -> JobDoneReason -> SPCM ()
jobDone jobId reason = do
  state <- get
  let runningJobs = spcJobsRunning state
  let pendingJobs = spcJobsPending state
  -- Remove from running or pending jobs
  let newRunningJobs = removeAssoc jobId runningJobs
  let newPendingJobs = removeAssoc jobId pendingJobs
  -- Add to done jobs
  let doneJobs = (jobId, reason) : spcJobsDone state
  -- Notify waiting workers
  let (waiting_for_job, not_waiting_for_job) =
        partition ((== jobId) . fst) (spcWaiting state)
  forM_ waiting_for_job $ \(_, rsvp) ->
    io $ reply rsvp reason
  put $ state { spcJobsRunning = newRunningJobs,
                spcJobsPending = newPendingJobs,
                spcJobsDone = doneJobs,
                spcWaiting = not_waiting_for_job
              }

workerIsIdle :: WorkerName -> SPCM ()
workerIsIdle wName = do
  state <- get
  let busyWorkers = spcWorkersBusy state
  let idleWorkers = spcWorkersIdle state
  case lookup wName [(wName', (worker, jobId, seconds)) | (wName', (worker, jobId, seconds)) <- busyWorkers] of
    Just (worker, _, _) -> do
      let newBusyWorkers = removeAssoc wName busyWorkers
      put $ state { spcWorkersBusy = newBusyWorkers,
                    spcWorkersIdle = (wName, worker) : idleWorkers }
    Nothing -> pure ()

workerIsGone :: WorkerName -> SPCM ()
workerIsGone wName = do
  state <- get
  let busyWorkers = spcWorkersBusy state
  let idleWorkers = spcWorkersIdle state
  let newBusyWorkers = removeAssoc wName busyWorkers
  let newIdleWorkers = removeAssoc wName idleWorkers
  put $ state { spcWorkersBusy = newBusyWorkers, spcWorkersIdle = newIdleWorkers }

checkTimeouts :: SPCM ()
checkTimeouts = do
  state <- get
  time <- io getSeconds
  let timedOut (_, (_, _, maxTime)) = time >= maxTime
      timedOutList = filter timedOut $ spcWorkersBusy state
  forM_ timedOutList $ \(_, (worker, jobId, _)) -> do
    io $ sendTo (workerServer worker) WorkerTimedOut
    jobDone jobId DoneTimeout

workerExists :: WorkerName -> SPCM Bool
workerExists workername = do
  state <- get
  let idleWorkers = spcWorkersIdle state
  let busyWorkers = spcWorkersBusy state
  pure $ workername `elem` (map fst idleWorkers ++ map fst busyWorkers)

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  checkTimeouts
  schedule
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending = (JobId jobid, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
      schedule
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case  ( lookup jobid $ spcJobsPending state,
                                lookup jobid $ spcJobsRunning state,
                                lookup jobid $ spcJobsDone state
                              ) of
        (Just _, _, _) -> JobPending
        (_, Just _, _) -> JobRunning
        (_, _, Just r) -> JobDone r
        _ -> JobUnknown
    MsgJobWait jobid rsvp -> do
      state <- get
      case lookup jobid $ spcJobsDone state of
        Just reason ->
          io $ reply rsvp reason
        Nothing -> do
          put $ state {spcWaiting = (jobid, rsvp) : spcWaiting state}
    MsgWorkerAdd wName rsvp -> do
      exists <- workerExists wName
      if exists
        then io $ reply rsvp $ Left "Worker with that name already exists."
        else do
          wServer <- io $ spawn $ workerLoop c wName
          let worker = Worker wName wServer
          state <- get
          put $ state { spcWorkersIdle = (wName, worker) : spcWorkersIdle state }
          io $ reply rsvp $ Right worker
          schedule
    MsgWorkerIdle wName -> do
      workerIsIdle wName
      schedule
    MsgWorkerJobDone wName jobId reason -> do
      jobDone jobId reason
      workerIsIdle wName
      schedule
    MsgWorkerTerminated wName -> do
      workerIsGone wName
    MsgJobCancel jobId -> do
      state <- get
      -- Check if the job is pending
      let pendingJobs = spcJobsPending state
      if jobId `elem` map fst pendingJobs
        then do
          -- Remove from pending jobs and mark as cancelled
          let newPendingJobs = removeAssoc jobId pendingJobs
          put $ state { spcJobsPending = newPendingJobs }
          jobDone jobId DoneCancelled
        else do
          -- Check if the job is running
          let runningJobs = spcJobsRunning state
          case lookup jobId runningJobs of
            Just _ -> do
              -- Find the worker running this job
              let busyWorkers = spcWorkersBusy state
              case [ (wName, worker) | (wName, (worker, jobId', _)) <- busyWorkers, jobId' == jobId ] of
                ((_, worker):_) -> do
                  -- Send cancellation message to the worker
                  io $ sendTo (workerServer worker) WorkerCancelJob
                [] -> return ()
              jobDone jobId DoneCancelled
            Nothing -> return ()
    _ -> pure ()

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = [],
            spcJobsRunning = [],
            spcJobsDone = [],
            spcWorkersIdle = [],
            spcWorkersBusy = [],
            spcWaiting = []
          }
  c <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  void $ spawn $ timer c
  pure $ SPC c
  where
    timer c _ = forever $ do
      threadDelay 1000000 -- 1 second
      sendTo c MsgTick

-- | Add a job for scheduling.
jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

-- | Asynchronously query the job status.
jobStatus :: SPC -> JobId -> IO JobStatus
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

-- | Synchronously block until job is done and return the reason.
jobWait :: SPC -> JobId -> IO JobDoneReason
jobWait (SPC c) jobid =
  requestReply c $ MsgJobWait jobid

-- | Asynchronously cancel a job.
jobCancel :: SPC -> JobId -> IO ()
jobCancel (SPC c) jobid =
  sendTo c $ MsgJobCancel jobid

-- | Add a new worker with this name. Fails with 'Left' if a worker
-- with that name already exists.
workerAdd :: SPC -> WorkerName -> IO (Either String Worker)
workerAdd (SPC c) wName =
  requestReply c $ MsgWorkerAdd wName

-- | Shut down a running worker. No effect if the worker is already
-- terminated.
workerStop :: Worker -> IO ()
workerStop (Worker _ server) =
  sendTo server WorkerTerminate

workerLoop :: Chan SPCMsg -> WorkerName -> Chan WorkerMsg -> IO ()
workerLoop spcChan wName workerChan = workerIdleLoop
  where
    workerIdleLoop :: IO ()
    workerIdleLoop = do
      send spcChan $ MsgWorkerIdle wName
      msg <- receive workerChan
      case msg of
        WorkerAssignJob jobId job -> do
          jobThreadId <- forkIO $ do
            result <- try (jobAction job) :: IO (Either SomeException ())
            case result of
              Left _ -> do
                send spcChan $ MsgWorkerJobDone wName jobId DoneCrashed
                send workerChan WorkerJobCrashed
              Right _ -> send spcChan $ MsgWorkerJobDone wName jobId Done
            send spcChan $ MsgWorkerIdle wName
            workerLoop spcChan wName workerChan
          workerBusyLoop jobId jobThreadId
        WorkerTerminate -> do
          send spcChan $ MsgWorkerTerminated wName
        _ -> workerIdleLoop

    workerBusyLoop :: JobId -> ThreadId -> IO ()
    workerBusyLoop jobId jobThreadId = do
      msg <- receive workerChan
      case msg of
        WorkerCancelJob -> do
          killThread jobThreadId
          send spcChan $ MsgWorkerJobDone wName jobId DoneCancelled
          workerIdleLoop
        WorkerJobDone -> do
          send spcChan $ MsgWorkerJobDone wName jobId Done
          workerIdleLoop
        WorkerJobCrashed -> do
          send spcChan $ MsgWorkerJobDone wName jobId DoneCrashed
          workerIdleLoop
        WorkerTerminate -> do
          killThread jobThreadId
          send spcChan $ MsgJobCancel jobId
          send spcChan $ MsgWorkerTerminated wName
        WorkerTimedOut -> do
          killThread jobThreadId
          send spcChan $ MsgWorkerJobDone wName jobId DoneTimeout
          workerIdleLoop
        _ -> workerBusyLoop jobId jobThreadId