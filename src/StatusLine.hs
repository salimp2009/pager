{- | Basic File info
to display status; eg. ls -l command in Linux/Unix
-}
module StatusLine where

import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified Data.Time.Clock.POSIX as PosixClock
import qualified System.Directory as Directory
import qualified Data.Text.IO as TextIO
import qualified Data.Text as Text

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
