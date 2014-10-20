--------------------------------------------------------------------------------
-- | My custom Hakyll commands
module Custom.Commands
    ( deploy
    ) where


--------------------------------------------------------------------------------
import           System.Directory (canonicalizePath)
import           System.Exit      (ExitCode)
import           System.FilePath  (isAbsolute, normalise, takeFileName)
import           System.IO.Error  (catchIOError)
import           System.Process   (system)

import           Hakyll.Core.Configuration (Configuration, deploySite)


--------------------------------------------------------------------------------
deploy :: Configuration -> IO ExitCode
deploy conf = deploySite conf conf