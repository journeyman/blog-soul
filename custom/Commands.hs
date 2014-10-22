--------------------------------------------------------------------------------
-- | My custom Hakyll commands
module Custom.Commands
    ( deploy
    ) where


--------------------------------------------------------------------------------
import           System.Directory
import           System.FilePath 
import           Data.List
import           Control.Monad

import           Hakyll.Core.Configuration (Configuration, deploySite)

--------------------------------------------------------------------------------
deploy :: Configuration -> IO ExitCode
deploy conf = deploySite conf conf

removeDirWithExceptions :: FilePath -> [FilePath] -> IO Bool
removeDirWithExceptions dirName excludeList = do
    rawItems <- getDirectoryContents dirName
    let afterExclusion = rawItems \\ excludeList
    let didExclude = length rawItems /= length afterExclusion
    let items = map (dirName </>) $ afterExclusion \\ dotDirs 
    files <- filterM doesFileExist items
    dirs <- filterM doesDirectoryExist items
    mapM_ removeFile files
    results <- mapM (flip removeDirWithExceptions excludeList) dirs
    let exclusionsOccured = or results
    let shouldRemove = not (didExclude || exclusionsOccured)
    when shouldRemove (removeDirectory dirName)
    return $ not shouldRemove

dotDirs :: [FilePath]
dotDirs = [".",".."]
