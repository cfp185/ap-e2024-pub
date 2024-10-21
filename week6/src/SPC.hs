module SPC
  ( -- * SPC startup
    SPC,
    startSPC,
    pingSPC,
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
data SPCMsg = MsgPing (ReplyChan Int)

-- | A Handle to the SPC instance.
data SPC = SPC (Server SPCMsg)

-- | The central state. Must be protected from the bourgeoisie.
data SPCState = SPCState
  { spcPingCounter :: Int
  }

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
runSPCM state (SPCM a) = ast <$> a state

handleMSG :: Chan SPCMsg -> SPCM ()
handleMSG = do
  handle c = do
      msg <- receive c
      case msg of
        MsgPing rsvp -> do
          reply rsvp 4243
          --increment++

startSPC :: IO SPC
startSPC = do
  let initial_state =
        SPCState
          { spcPingCounter = 0
          }
  server <- spawn $ \c -> runSPCM initial_state $ forever $ handleMsg c
  pure $ SPC server

startSPC :: IO SPC
-- a = IO (a, state)
startSPC = do
  server <- spawn $ \c -> forever $ handle c
  pure $ SPCM server
  where
    handle c = do
      msg <- receive c
      case msg of
        MsgPing rsvp ->
          reply rsvp 4243

pingSPC :: SPC -> IO Int
pingSPC (SPC c) = do
  requestReply c MsgPing

