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
  zkdir <- getZkDir pwd
  case head args of
    "link" -> do
      when (length args /= 2) (error "Please provide a file to link")
      let linkFile = args !! 1
      linkFileToZk linkFile
      exitSuccess
    _ -> do
      let prog = unwords [_NEURON_EXE, "-d", zkdir]
      let cmd = unwords ([prog] ++ map quote args)
      system cmd
  return ()

quote :: String -> String
quote x = mconcat ["'", x, "'"]

linkFileToZk :: FilePath -> IO ()
linkFileToZk f = do
  pwd <- getCurrentDirectory
  rootdir <- errorOnNonExistence <$> getZkRootDir pwd
  zkreldir <- getZkRelDir rootdir
  let pwdreldir = getRelPathRoot rootdir pwd
  newzf <- neuronNewZettel (rootdir </> zkreldir)
  removePathForcibly newzf
  createFileLink
    (relPathToDescTree zkreldir </> pwdreldir </> f)
    newzf

errorOnNonExistence :: Maybe FilePath -> FilePath
errorOnNonExistence Nothing = error "zk-directory not found"
errorOnNonExistence (Just d) = d

getZkDir :: FilePath -> IO FilePath
getZkDir d = do
  zkrootdir <- getZkRootDir d
  return ((errorOnNonExistence zkrootdir) </> _ZK_DIR_NAME)

getZkRootDir' :: [FilePath] -> IO (Maybe FilePath)
getZkRootDir' [] = return Nothing
getZkRootDir' ds = do
  let p = joinPath ds
  let dAbs = p </> _ZK_DIR_NAME
  found <- doesDirectoryExist dAbs
  if found
    then return (Just p)
    else getZkRootDir' (init ds)

getZkRootDir :: FilePath -> IO (Maybe FilePath)
getZkRootDir f = getZkRootDir' (splitDirectories f)

getZkRelDir :: FilePath -> IO FilePath
getZkRelDir rootdir = do
  let zkdirname = _ZK_DIR_NAME
  let zkdir = rootdir </> zkdirname
  isSymLinkZkDir <- pathIsSymbolicLink zkdir
  if isSymLinkZkDir
    then getSymbolicLinkTarget zkdir
    else return zkdirname

getRelPathRoot :: FilePath -> FilePath -> FilePath
getRelPathRoot rootdir d = diffPath d rootdir

diffPath :: FilePath -> FilePath -> FilePath
diffPath x y = joinPath $ drop (length ys) xs
  where
    xs = splitDirectories x
    ys = splitDirectories y

neuronNewZettel :: FilePath -> IO FilePath
neuronNewZettel d =
  head . lines <$> readProcess _NEURON_EXE ["-d", d, "new", "DUMMY"] []

relPathToDescTree :: FilePath -> FilePath
relPathToDescTree f = joinPath . take n . repeat $ ".."
  where
    n = length . splitDirectories $ f
