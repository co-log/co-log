module Colog.Rotation
       (
         Limit(..)
       , withLogRotation
       ) where

import Data.Maybe (mapMaybe)
import Data.Semigroup (Max (..))
import System.IO (hFileSize)

import Colog.Core.Action (LogAction (..))

import qualified System.Directory as D
import qualified System.FilePath.Posix as POS


data Limit = LimitTo Natural | Unlimited deriving (Eq, Ord)

{- | Logger rotation action. Takes name of the logging file @file.foo@. Always
writes new logs to file named @file.foo@ (given file name, also called as /hot log/).

* If the size of the file exceeds given limit for file sizes then this action
  renames @file.foo@ to @file.foo.(n + 1)@ (where @n@ is the number of latest
  renamed file).
* If the number of files on the filesystem is bigger than the files number limit
  then the given @FilePath -> IO ()@ action is called on the oldest file. As
  simple solution, you can pass @removeFile@ function to delete old files but
  you can also pass some archiving function if you don't want to lose old logs.
-}
withLogRotation
    :: forall r msg m .
       MonadIO m
    => Limit
    -- ^ Max allowed file size in bytes
    -> Limit
    -- ^ Max allowed number of files to have
    -> FilePath
    -- ^ File path to log
    -> (FilePath -> IO ())
    -- ^ What to do with old files; pass @removeFile@ here for deletion
    -> (Handle -> LogAction m msg)
    -- ^ Action that writes to file handle
    -> (LogAction m msg -> IO r)
    -- ^ Continuation action
    -> IO r
withLogRotation sizeLimit filesLimit path cleanup mkAction cont = do
    -- TODO: figure out how to use bracket to safely manage
    -- possible exceptions
    handle <- openFile path AppendMode
    handleRef <- newIORef handle
    cont $ rotationAction handleRef
  where
    rotationAction :: IORef Handle -> LogAction m msg
    rotationAction refHandle
      = LogAction $ \msg -> do
        handle <- liftIO $ readIORef refHandle
        unLogAction (mkAction handle) msg

        whenM
          (liftIO $ isFileSizeLimitReached sizeLimit handle)
          (cleanupAndRotate refHandle)
    cleanupAndRotate :: IORef Handle -> m ()
    cleanupAndRotate refHandle = liftIO $ do
      h <- readIORef refHandle
      hClose h
      maxN <- maxFileIndex path
      renameFileToNumber (succ maxN) path
      oldFiles <- getOldFiles filesLimit path
      mapM_ cleanup oldFiles
      newHandle <- openFile path AppendMode
      modifyIORef' refHandle (const newHandle)

isLimitedBy :: Integer -> Limit -> Bool
isLimitedBy _ Unlimited = False
isLimitedBy size (LimitTo limit) | size <= 0 = False
                                 | otherwise = limit > (fromInteger size :: Natural)

isFileSizeLimitReached :: Limit -> Handle -> IO Bool
isFileSizeLimitReached limit handle = do
  fileSize <- hFileSize handle
  pure $ isLimitedBy fileSize limit

-- if you have files node.log.0, node.log.1 and node.log.2 then this function
-- will return `2` if you give it `node.log`
maxFileIndex :: FilePath -> IO Int
maxFileIndex path = do
  files <- D.listDirectory (POS.takeDirectory path)
  let logFiles = filter (== POS.takeBaseName path) files
  let maxFile = getMax . foldMap Max <$> nonEmpty (mapMaybe logFileIndex logFiles)
  pure $ fromMaybe 0 maxFile

-- given number 4 and path `node.log` renames file `node.log` to `node.log.4`
renameFileToNumber :: Int -> FilePath -> IO ()
renameFileToNumber n path = D.renameFile path (path POS.<.> show n)

-- if you give it name like `node.log.4` then it returns `Just 4`
logFileIndex :: FilePath -> Maybe Int
logFileIndex path = nonEmpty (POS.takeExtension path) >>= readMaybe . tail

-- creates list of files with indices who are older on given Limit than the latest one
getOldFiles :: Limit -> FilePath -> IO [FilePath]
getOldFiles limit path = do
    currentMaxN <- maxFileIndex path
    files <- D.listDirectory (POS.takeDirectory path)
    let tuple = map (\a -> (a, toInteger <$> logFileIndex a)) files
    pure $ map fst $ filter (maybe False (isOldFile currentMaxN) . snd) tuple
  where
    isOldFile :: Int -> Integer -> Bool
    isOldFile maxN n = case limit of
                         Unlimited -> False
                         LimitTo l -> n < toInteger maxN - toInteger l

