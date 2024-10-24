module APL.InterpIO (runEvalIO) where

import APL.Monad
import APL.Util
import System.Directory (removeFile)
import System.IO (hFlush, readFile', stdout)

-- Converts a string into a value. Only 'ValInt's and 'ValBool' are supported.
readVal :: String -> Maybe Val
readVal = unserialize

-- 'prompt s' prints 's' to the console and then reads a line from stdin.
prompt :: String -> IO String
prompt s = do
  putStr s
  hFlush stdout
  getLine

-- 'writeDB dbFile s' writes the 'State' 's' to the file 'db'.
writeDB :: FilePath -> State -> IO ()
writeDB db s =
  writeFile db $ serialize s

-- 'readDB db' reads the database stored in 'db'.
readDB :: FilePath -> IO (Either Error State)
readDB db = do
  ms <- readFile' db
  case unserialize ms of
    Just s -> pure $ pure s
    Nothing -> pure $ Left "Invalid DB."

-- 'copyDB db1 db2' copies 'db1' to 'db2'.
copyDB :: FilePath -> FilePath -> IO ()
copyDB db db' = do
  s <- readFile' db
  writeFile db' s

withTempDB :: (FilePath -> IO a) -> IO a
withTempDB m = do
  tempDB <- newTempDB
  res <- m tempDB
  removeFile tempDB
  pure res

-- Removes all key-value pairs from the database file.
clearDB :: IO ()
clearDB = writeFile dbFile ""

-- The name of the database file.
dbFile :: FilePath
dbFile = "db.txt"

runEvalIO :: EvalM a -> IO (Either Error a)
runEvalIO evalm = do
  clearDB
  runEvalIO' envEmpty dbFile evalm
  where
    runEvalIO' :: Env -> FilePath -> EvalM a -> IO (Either Error a)
    runEvalIO' _ _ (Pure x) = pure $ pure x
    runEvalIO' r db (Free (ReadOp k)) = runEvalIO' r db $ k r
    runEvalIO' r db (Free (StateGetOp k)) = do
      result <- readDB db
      case result of
        Left err -> pure $ Left err
        Right s  -> runEvalIO' r db (k s)
    runEvalIO' r db (Free (StatePutOp s k)) = do
      writeDB db s
      runEvalIO' r db k
    runEvalIO' r db (Free (PrintOp p m)) = do
      putStrLn p
      runEvalIO' r db m
    runEvalIO' _ _ (Free (ErrorOp e)) = pure $ Left e
    runEvalIO' r db (Free (TryCatchOp m1 m2)) = do
      m1' <- runEvalIO' r db m1
      case m1' of
        Left _ -> runEvalIO' r db m2
        Right _  -> pure m1'
    runEvalIO' r db (Free (KvGetOp key k)) = do
      dbState <- readDB db
      case dbState of
        Left err -> pure $ Left err
        Right s  -> case lookup key s of
          Just val -> runEvalIO' r db (k val)
          Nothing -> do
            putStr $ "Invalid key: " ++ show key ++ ". "
            result <- prompt "Enter a replacement: "
            case readVal result of
              Nothing -> pure $ Left $ "Invalid value input: " ++ result
              Just val -> runEvalIO' r db (k val)
    runEvalIO' r db (Free (KvPutOp key val m)) = do
      dbState <- readDB db
      case dbState of
        Left err -> pure $ Left err
        Right s -> do
          let newState = (key, val) : filter (\(k, _) -> k /= key) s
          writeDB db newState
          runEvalIO' r db m
    runEvalIO' r db (Free (TransactionOp e a)) = withTempDB $ \tempDB -> do
      copyDB db tempDB
      result <- runEvalIO' r tempDB e
      case result of
        Left _ -> runEvalIO' r db a
        Right _ -> do
          copyDB tempDB db
          runEvalIO' r db a