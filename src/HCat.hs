{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
module HCat (runHCat, runHCat7) where
import qualified System.Environment as Env 
import qualified Control.Exception as Exception
import qualified System.IO.Error as IOError 
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Pages (getContinue, ContinueCancel (..), getTerminalSize, showPages, paginate, paginate2)
import System.IO (openFile, IOMode (ReadMode), hSetBuffering, stdout, BufferMode (NoBuffering))
import StatusLine (fileInfo)



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

runHCat8 :: IO ()      
runHCat8 =
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

runHCat9 :: IO ()
runHCat9 =
  handleArgs
  >>= eitherToError
  >>= flip openFile ReadMode
  >>= TextIO.hGetChunk   -- ^ this was original @TextIO.hGetContents@
  >>= \contents ->
    getTerminalSize >>= \termSize ->
      let pages = paginate termSize contents
      in showPages pages

runHCat10 :: IO ()
runHCat10 = 
  handleArgs
  >>= eitherToError
  >>= \targetFilePath ->
        openFile targetFilePath ReadMode
        >>= TextIO.hGetContents
        >>= \contents ->
           getTerminalSize >>= \termSize ->
             hSetBuffering stdout NoBuffering
             >> fileInfo targetFilePath >>= \finfo ->
               let pages = paginate2 termSize finfo contents
               in showPages pages


runHCat11 :: IO ()
runHCat11 = do
  args <- handleArgs
  targetFilePath <- eitherToError args
  fileHandle <- openFile targetFilePath ReadMode
  contents <- TextIO.hGetContents fileHandle   
  termSize <-  getTerminalSize 
  hSetBuffering stdout NoBuffering
  finfo <- fileInfo targetFilePath 
  let pages = paginate2 termSize finfo contents
  showPages pages               

-- | final refactored
runHCat :: IO ()
runHCat = do
  targetFilePath <- do
    args <- handleArgs
    eitherToError args
  
  contents <- do 
    fileHandle <- openFile targetFilePath ReadMode
    TextIO.hGetContents fileHandle    
  
  termSize <-  getTerminalSize 
  hSetBuffering stdout NoBuffering
  finfo <- fileInfo targetFilePath 
  let pages = paginate2 termSize finfo contents
  showPages pages  