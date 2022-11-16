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
