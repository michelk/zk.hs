module Main where

import Control.Monad
import System.Directory
import System.Environment
import System.Exit
import System.FilePath.Posix
import System.Process

_ZK_DIR_NAME :: FilePath
_ZK_DIR_NAME = ".zk"

_NEURON_EXE :: FilePath
_NEURON_EXE = "neuron"

main :: IO ()
main = do
  args <- getArgs
  when
    (null args || (length args == 1 && head args `elem` ["-h", "--help"]))
    ( putStrLn
        ( "Wrapper for 'neuron' which searches for a" <> _ZK_DIR_NAME
            <> "directory \
               \ as zettelkasten in the descending directory tree"
        )
        >> exitSuccess
    )
  pwd <- getCurrentDirectory
  let ds = splitDirectories pwd
  zkDir <- findZkDir _ZK_DIR_NAME ds
  _ <-
    case zkDir of
      Nothing -> die ("Could not find directory " <> _ZK_DIR_NAME)
      Just d -> do
        let prog = unwords [_NEURON_EXE, "-d", d]
        case head args of
          "link" -> do
            when (length args /= 2) (error "Please provide a file to link")
            let linkFile = args !! 1
            neuronFile <-
              head . lines
                <$> readProcess _NEURON_EXE ["-d", d, "new", "DUMMY"] []
            createFileLink linkFile neuronFile
            exitSuccess
          "dir" -> do
            isSymLink <- pathIsSymbolicLink d
            d' <-
              if isSymLink
                then (dropFileName d </>) <$> getSymbolicLinkTarget d
                else return d
            putStrLn d'
            exitSuccess
          _ -> do
            let cmd = unwords ([prog] ++ map quote args)
            system cmd
  return ()

findZkDir :: FilePath -> [FilePath] -> IO (Maybe FilePath)
findZkDir _ [] = return Nothing
findZkDir zkdir ds = do
  let dAbs = joinPath (ds ++ [zkdir])
  found <- doesDirectoryExist dAbs
  if found
    then return (Just dAbs)
    else findZkDir zkdir (init ds)

quote :: String -> String
quote x = mconcat ["'", x, "'"]
