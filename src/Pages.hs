{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}
{-|
Pages are list of line
Groups will be the list of pages
we will also track line number in each page
define number of text we want in a line
so;
  - number of text in a line
  - number of lines in a page
  - number of pages 
will also have user scroll down at their own phase
-}

module Pages where
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Control.Monad (guard)
import qualified System.Process as Process
import qualified System.Info
import System.IO (stdin, hGetChar, BufferMode (NoBuffering), hSetBuffering, hSetEcho)
import qualified Data.ByteString as BS
import qualified Data.ByteString as BS
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _  = []
groupsOf _ [] = []
groupsOf n elems =
  let (hd, tl) = splitAt n elems
  in hd : groupsOf n tl

-- >>> 
wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText 
  | Text.length lineText <= lineLength = [lineText]
  | otherwise =
     let 
      (candidate, nextlines) = Text.splitAt lineLength lineText 
      (firstLine, overflow)  = softWrap candidate  (Text.length candidate - 1)
     in firstLine : wordWrap lineLength  (overflow <> nextlines)
     where
      softWrap hardwrappedText textIndex
        | textIndex <= 0 = (hardwrappedText, Text.empty)
        | Text.index hardwrappedText textIndex == ' ' =
            let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
            in (wrappedLine, Text.tail rest)
        | otherwise = softWrap hardwrappedText (textIndex - 1) 
        
data ScreenDimensions = ScreenDimensions
  { screenRows :: Int
  , screenCols :: Int
  } deriving Show
  
paginate :: ScreenDimensions -> Text.Text -> [Text.Text]  
paginate (ScreenDimensions rows cols) text =
  let unwrappedLines = Text.lines text
      wrappedLines    = concatMap (wordWrap cols) unwrappedLines
      pageLines       = groupsOf rows wrappedLines
  in map Text.unlines pageLines

-- | screen dimensipn
{-
>>>getTerminalSize
ScreenDimensions {screenRows = 1, screenCols = 1}
-}
getTerminalSize :: IO ScreenDimensions
getTerminalSize =
  case System.Info.os of 
    "darwin" -> tputScreenDimensions
    "linux"  -> tputScreenDimensions
    _other   -> pure $ ScreenDimensions 25 80
  where
    -- tputScreenDimensions :: IO ScreenDimensions
    -- tputScreenDimensions = 
    --   Process.readProcess "tput" ["lines"] ""
    --   >>= \lines ->
    --     Process.readProcess "tput" ["cols"] ""
    --   >>= \cols ->   
    --     let lines' = read $ init lines
    --         cols'   = read $ init cols
    --     in return $ ScreenDimensions lines' cols'            
    tputScreenDimensions :: IO ScreenDimensions
    tputScreenDimensions = do
        lines <- Process.readProcess "tput" ["lines"] ""  
        cols  <- Process.readProcess "tput" ["cols"] ""
        let lines' = read $ init lines
            cols'  = read $ init cols
        return $ ScreenDimensions lines' cols'

data ContinueCancel = Continue | Cancel 
  deriving stock (Eq, Show)   

getContinue :: IO ContinueCancel
getContinue =  do
   hSetBuffering stdin NoBuffering 
   hSetEcho stdin False 
   userinput <- hGetChar stdin
   case userinput of
    ' ' -> return Continue
    'q' -> return Cancel 
    'Q' -> return Cancel
    _   -> getContinue

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) = do
  clearScreen
  TextIO.putStrLn page
  contCancel <- getContinue
  case contCancel of
    Continue -> showPages pages
    Cancel   -> return ()

clearScreen :: IO ()    
clearScreen =
  BS.putStr "\^[[1J\^[[1;1H"


-- >>>:i Text.lines
-- lines :: Text -> [Text] 	-- Defined in ‘Data.Text’

-- >>>Text.lines (Text.pack "one, two, three")
-- ["one, two, three"]

-- >>>Text.lines (Text.pack "one\ntwo\nthree")
-- ["one","two","three"]

-- >>>:t concatMap 
-- concatMap :: Foldable t => (a -> [b]) -> t a -> [b]



-- | Examples;
-- >>> wordWrap 6 (Text.pack "word Wrapping is tricky")
-- ["word","Wrappi","ng is","tricky"]
