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
deploy :: FilePath -> Configuration -> IO ExitCode
deploy targetDir conf = do 
    _ <- removeDirWithExceptions targetDir [".git"]
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
copyDir ::  FilePath -> FilePath -> IO ()
copyDir src dst = do
  whenM (not <$> doesDirectoryExist src) $
    throw (userError "source does not exist")
  whenM (doesFileOrDirectoryExist dst) $
    throw (userError "destination already exists")

  createDirectory dst
  content <- getDirectoryContents src
  let xs = filter (`notElem` [".", ".."]) content
  forM_ xs $ \name -> do
    let srcPath = src </> name
    let dstPath = dst </> name
    isDirectory <- doesDirectoryExist srcPath
    if isDirectory
      then copyDir srcPath dstPath
      else copyFile srcPath dstPath

  where
    doesFileOrDirectoryExist x = orM [doesDirectoryExist x, doesFileExist x]
    orM xs = or <$> sequence xs
    whenM s r = s >>= flip when r


dotDirs :: [FilePath]
dotDirs = [".",".."]