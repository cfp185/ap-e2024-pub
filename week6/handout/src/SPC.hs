module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    pingSPC,
    Job(..),
    JobId,
    jobAdd,
    JobDoneReason,
    JobStatus,
  )
where

import Control.Concurrent
  ( ThreadId,
    forkIO,
    killThread,
    threadDelay,
  )
import Control.Exception (SomeException, catch)
import Control.Monad (ap, forM_, forever, liftM, void)
import Data.List (partition)
import GenServer
import System.Clock.Seconds (Clock (Monotonic), Seconds, getTime)

-- First some general utility functions.

-- | Retrieve Unix time using a monotonic clock. You cannot use this
-- to measure the actual world time, but you can use it to measure
-- elapsed time.
getSeconds :: IO Seconds
getSeconds = getTime Monotonic

-- | Remove mapping from association list.
removeAssoc :: (Eq k) => k -> [(k, v)] -> [(k, v)]
removeAssoc needle ((k, v) : kvs) =
  if k == needle
    then kvs
    else (k, v) : removeAssoc needle kvs
removeAssoc _ [] = []

-- Then the definition of the glorious SPC.

-- Messages sent to SPC.
data SPCMsg
    = MsgPing (ReplyChan Int)
    | MsgJobAdd Job (ReplyChan JobId)
    | MsgJobStatus JobId (ReplyChan JobStatus)


-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcJobCounter :: JobId,
    spcJobsPending :: [(JobId,Job)]
  }

-- | A job that is to be enqueued in the glorious SPC.
data Job = Job
  { -- | The IO action that comprises the actual action of the job.
    jobAction :: IO (),
    -- | The maximum allowed runtime of the job, counting from when
    -- the job begins executing (not when it is enqueued).
    jobMaxSeconds :: Int
  }

data JobDoneReason
  = Done -- Normal termination.
  | DoneTimeout -- The job was killed because it ran for too long.
  | DoneCancelled -- The job was explicitly cancelled.
  | DoneCrashed -- The job crashed due to an exception.
  deriving (Eq, Ord, Show)

data JobStatus
  = JobDone JobDoneReason -- The job is done and this is why.
  | JobRunning -- The job is still running.
  | JobPending -- The job is enqueued, but is waiting for an idle worker.
  deriving (Eq, Ord, Show)

newtype SPCM a = SPCM (SPCState -> IO (a, SPCState))

-- | A unique identifier of a job that has been enqueued.
newtype JobId = JobId Int
  deriving (Eq, Ord, Show)

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

get :: SPCM SPCState
get = SPCM $ \state -> pure (state, state)

-- | Overwrite the state.
put :: SPCState -> SPCM ()
put state = SPCM $ \_ -> pure ((), state)

io :: IO a -> SPCM a
io m = SPCM $ \state -> do
  x <- m
  pure (x, state)

runSPCM :: SPCState -> SPCM a -> IO a
runSPCM state (SPCM f) = fst <$> f state

jobStatus :: SPC -> JobId -> IO (Maybe JobStatus)
jobStatus (SPC c) jobid =
  requestReply c $ MsgJobStatus jobid

handleMsg :: Chan SPCMsg -> SPCM ()
handleMsg c = do
  msg <- io $ receive c
  case msg of
    MsgJobAdd job rsvp -> do
      state <- get
      let JobId jobid = spcJobCounter state
      put $
        state
          { spcJobsPending =
              (spcJobCounter state, job) : spcJobsPending state,
            spcJobCounter = JobId $ succ jobid
          }
      io $ reply rsvp $ JobId jobid
    MsgJobStatus jobid rsvp -> do
      state <- get
      io $ reply rsvp $ case lookup jobid $ spcJobsPending state of
        Just _ -> Just JobPending
        _ -> Nothing

jobAdd :: SPC -> Job -> IO JobId
jobAdd (SPC c) job =
  requestReply c $ MsgJobAdd job

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcJobCounter = JobId 0,
            spcJobsPending = []
          }
  server <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  pure $ SPC server

pingSPC :: SPC -> IO Int
pingSPC (SPC c) = do
  requestReply c MsgPing

