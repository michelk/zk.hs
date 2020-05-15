module Main where

import Control.Monad
import Data.List
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.Process

_ZK_DIR_NAME :: FilePath
_ZK_DIR_NAME = ".neuron"

main :: IO ()
main = do
  args <- getArgs
  when
    (length args == 0 || (length args == 1 && head args `elem` ["-h", "--help"]))
    ( putStrLn
        "Wrapper for 'neuron' which searches for a '.neuron' directory \
        \ as zettelkasten in the descending directory tree"
        >> exitSuccess
    )
  pwd <- getCurrentDirectory
  let ds = splitDirectories pwd
  zkDir <- findZkDir _ZK_DIR_NAME ds
  ext <-
    case zkDir of
      Nothing -> do
        die ("Could not find directory " <> _ZK_DIR_NAME)
      Just d -> do
        let prog = ["neuron", "-d", d]
        let cmd = intercalate " " (prog ++ args)
        system cmd
  return ()

findZkDir :: FilePath -> [FilePath] -> IO (Maybe FilePath)
findZkDir _ [] = return Nothing
findZkDir zkdir ds = do
  let dAbs = joinPath ds
  found <- doesDirectoryExist (dAbs </> zkdir)
  if found
    then return (Just dAbs)
    else findZkDir zkdir (init ds)
