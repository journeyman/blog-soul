--------------------------------------------------------------------------------
-- | My custom Hakyll commands
module Custom.Commands
    ( deploy
    ) where


--------------------------------------------------------------------------------
import           System.Directory
import           System.FilePath 
import           System.Exit (ExitCode (..))
import           Data.List
import           Control.Monad
import           Control.Applicative ((<$>))
import           Control.Exception (throw)

import           Hakyll.Core.Configuration (Configuration (..), deploySite)

--------------------------------------------------------------------------------
--caution: case-sensitive!
ignoredItems :: [FilePath]
ignoredItems = 
    [".git",
     "README.md",
     ".gitignore"]

deploy :: FilePath -> Configuration -> IO ExitCode
deploy targetDir conf = do 
    _ <- removeDirWithExceptions targetDir ignoredItems
    copyDir (destinationDirectory conf) targetDir
    return ExitSuccess

removeDirWithExceptions :: FilePath -> [FilePath] -> IO Bool
removeDirWithExceptions dirName excludeList = do
    doesExist <- doesDirectoryExist dirName
    if (doesExist) then do
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
    else return False 

-- borrowed from http://stackoverflow.com/questions/6807025/
-- and a bit modified to overwrite dst directory if exists
copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  whenM (not <$> doesDirectoryExist src) $
    throw (userError "source does not exist")
  whenM (not <$> doesDirectoryExist dst) $
    createDirectory dst

  contents <- getDirectoryContents src
  let items = contents \\ dotDirs
  forM_ items $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath

  where
    whenM s r = s >>= flip when r


dotDirs :: [FilePath]
dotDirs = [".",".."]