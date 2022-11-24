{- | Basic File info
to display status; eg. ls -l command in Linux/Unix
-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module StatusLine where

import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified System.Directory as Directory
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text
import qualified Text.Printf as Printf


-- | Basic file info
data FileInfo = FileInfo
  { filePath :: FilePath
  , fileSize :: Int
  , fileMTime :: Clock.UTCTime
  , fileReadable :: Bool
  , fileWritable :: Bool
  , fileExecutable :: Bool
  } deriving Show

-- >>>fileInfo "hcat.cabal"
-- FileInfo {filePath = "hcat.cabal", fileSize = 1474, fileMTime = 2022-11-22 12:40:24.0468457 UTC, fileReadable = True, fileWritable = True, fileExecutable = False}
fileInfo :: FilePath -> IO FileInfo
fileInfo filePath = do
  perms <- Directory.getPermissions filePath
  mtime <- Directory.getModificationTime filePath
  contents <- TextIO.readFile filePath
  let size = Text.length contents
  return FileInfo
    { filePath = filePath
    , fileSize = size
    , fileMTime = mtime
    , fileReadable = Directory.readable perms
    , fileWritable = Directory.writable perms
    , fileExecutable = Directory.executable perms
    }   
     
  
formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo{..} maxWidth totalPages currentPage =
    let
      timestamp =
        TimeFormat.formatTime TimeFormat.defaultTimeLocale "%F %T" fileMTime
      permissionString =
        [ if fileReadable then 'r' else '-'
        , if fileWritable then 'w' else '-'
        , if fileExecutable then 'x' else '-' ]
      statusLine = Text.pack $
        Printf.printf
        "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
        filePath
        permissionString
        fileSize
        timestamp
        currentPage
        totalPages
    in invertText (truncateStatus statusLine)
    where
    invertText inputStr =
      let
        reverseVideo = "\^[[04m"
        resetVideo = "\^[[1m"
      in reverseVideo <> inputStr <> resetVideo
    truncateStatus statusLine
            | maxWidth <= 3 = ""
            | Text.length statusLine > maxWidth =
                Text.take (maxWidth - 3) statusLine <> "..."
            | otherwise = statusLine
