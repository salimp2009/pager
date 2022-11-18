{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
module HCat (runHCat, runHCat7) where
import qualified System.Environment as Env 
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError 
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Pages (getContinue, ContinueCancel (..))

handleArgs :: IO (Either String FilePath)
handleArgs = 
    parseArgs <$> Env.getArgs 
    where
      parseArgs argList =
        case argList of
          [fileName] -> Right fileName
          [] -> Left "empty list!!"
          _  -> Left "multiple files not supported"

    -- | Alternative way for above           
    --     do 
    --  args <- Env.getArgs 
    --  case args of
    --   [] -> return  (Left "empty list!!")
    --   (x:xs) -> return (Right x)
    

-- | this will let us to pass argument at commandline
runHCat2 :: IO ()
runHCat2 = do
   arg  <- handleArgs
   case arg of
    Left errmessage -> putStrLn $ "Error: " <> errmessage
    Right fname     -> putStrLn $ "Opening file : " <> fname

runHCat3 :: IO ()
runHCat3 = 
    handleArgs >>= displayMessage 
    where
      displayMessage arg =
        case arg of
         Left errmessage -> putStrLn $ "Error" <> errmessage
         Right fname     -> putStrLn $ "Opening file : " <> fname

runHCat4 :: IO ()
runHCat4 =          
  handleArgs >>= \case
      Left errmessage  -> 
        putStrLn $ "Error: " <> errmessage
      Right fname ->
        readFile fname >>= putStrLn

-- | handle exceptions
{-
>>>:t Exception.catch
Exception.catch :: Exception e => IO a -> (e -> IO a) -> IO a
-}  

runHCat5 :: IO ()
runHCat5 = Exception.catch
    (handleArgs >>= \case
      Left errmessage  -> 
        putStrLn $ "Error: " <> errmessage
      Right fname ->
        readFile fname >>= putStrLn 
    ) handlerr
    where
      handlerr :: IOError -> IO ()
      handlerr error = putStrLn "ran into an error => " >> print error
-- >>>:i IOError 
-- type IOError :: *
-- type IOError = IOException
--   	-- Defined in ‘GHC.IO.Exception’

-- >>>:i IOException
-- Not in scope: `IOException'

runHCat6 :: IO()
runHCat6=
  withErrorHandling $ 
    handleArgs >>= \case
     Left errmessage  -> 
       putStrLn $ "Error: " <> errmessage
     Right fname ->
       readFile fname >>= putStrLn 
  where
    withErrorHandling :: IO () -> IO ()
    withErrorHandling ioAction = Exception.catch ioAction handler
    
    handler :: IOError -> IO ()
    handler error = putStrLn "ran into an error => \t " >> print error      

eitherToError :: Show a => Either a b -> IO b
eitherToError (Right x) = return x
eitherToError (Left err) = 
  Exception.throwIO . IOError.userError $ show err

-- | alternative to above
eitherToError2 :: Show a => Either a b -> IO b      
eitherToError2 val = do
  case val of
    Right x  -> return x
    Left err -> do
      let except = IOError.userError $ show err
      Exception.throwIO except

runHCat :: IO ()      
runHCat =
  handleIOError $
    handleArgs 
    >>= eitherToError
    >>= TextIO.readFile
    >>= TextIO.putStrLn
  where
    handleIOError :: IO () -> IO ()
    handleIOError ioAction =
      Exception.catch ioAction $
        \err -> print "ran into an error =>" >> print @IOError err

runHCat7 :: IO ()      
runHCat7 = do 
  putStrLn "to Continue please press (Space) or to Quit press(q or Q)"  
  contCancel  <- getContinue
  case contCancel of
    Continue -> putStrLn "ok Continuing...!" >> runHCat7
    Cancel   -> putStrLn "goodbye :)"


