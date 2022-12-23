import Control.Monad (when)
import Data.List (isPrefixOf)
import System.Directory (listDirectory, renamePath)
import System.FilePath (FilePath, (</>))

renameFiles :: FilePath -> String -> String -> IO ()
renameFiles dir oldStr newStr = do
  files <- listDirectory dir
  mapM_ (renameFile dir oldStr newStr) files

renameFile :: FilePath -> String -> String -> FilePath -> IO ()
renameFile dir oldStr newStr file =
  when (oldStr `isPrefixOf` file) $ do
    let oldPath = dir </> file
        newPath = dir </> newStr ++ drop (length oldStr) file
    renamePath oldPath newPath