module Main where

import qualified Data.ByteString.Char8 as B
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (forever)
import System.Posix.Files
import System.Posix.Types
import Foreign.C.Error
import System.Posix.IO

import System.Fuse

type FileName = String

type HT = ()

liveString :: B.ByteString
liveString = B.pack "live!\n"

livePath :: FilePath
livePath = "/live"

printMVar :: MVar String -> IO ()
printMVar mv = do
 v <- takeMVar mv
 putStrLn $ "Recieved MVar: " ++ v

main :: IO ()
main = do
 mv <- newEmptyMVar
 forkIO $ forever $ printMVar mv
 let
     liveFSOps :: FuseOperations HT
     liveFSOps =
      defaultFuseOps {
       fuseGetFileSystemStats = liveGetFileSystemStats,
       fuseGetFileStat        = liveGetFileStat,
       fuseAccess             = liveAccess,
       fuseOpen               = liveOpen,
       fuseRead               = liveRead,
       fuseWrite              = liveWrite,
       fuseFlush              = liveFlush,
       fuseRelease            = liveRelease,
       fuseSynchronizeFile    = liveSynchronizeFile,
       fuseCreateDirectory    = liveCreateDirectory,
       fuseOpenDirectory      = liveOpenDirectory,
       fuseReadDirectory      = liveReadDirectory,
       fuseSetFileSize        = liveSetFileSize
     }

     liveGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
     liveGetFileSystemStats str = do
      putMVar mv "GetFileSystemStats"
      return $ Right $ FileSystemStats
       { fsStatBlockSize       = 512
       , fsStatBlockCount      = 1
       , fsStatBlocksFree      = 1
       , fsStatBlocksAvailable = 1
       , fsStatFileCount       = 5
       , fsStatFilesFree       = 10
       , fsStatMaxNameLength   = 255
      }

     liveGetFileStat :: FilePath -> IO (Either Errno FileStat)
     liveGetFileStat "/" = do
      putMVar mv "GetFileStat(1)"
      ctx <- getFuseContext
      return $ Right $ dirStat ctx
     liveGetFileStat _ = do
      putMVar mv "GetFileStat(2)"
      ctx <- getFuseContext
      return $ Right $ fileStat ctx

     liveAccess :: FilePath -> Int -> IO Errno
     liveAccess _ _ = do
      putMVar mv "Access"
      return eOK

     liveCreateDirectory :: FilePath -> FileMode -> IO Errno
     liveCreateDirectory _ _ = putMVar mv "CreateDirectory" >> return eOK

     liveOpenDirectory :: FilePath -> IO Errno
     liveOpenDirectory _ = putMVar mv "OpenDirectory" >> return eOK

     liveReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
     liveReadDirectory _ = do
      putMVar mv "ReadDirectory"
      ctx <- getFuseContext
      return $ Right
       [(".", dirStat ctx)
       ,("..", dirStat ctx)
       ,(liveName, fileStat ctx)
       ]
      where (_:liveName) = livePath

     liveSetFileSize :: FilePath -> FileOffset -> IO Errno
     liveSetFileSize _ _ = putMVar mv "SetFileSize" >> return eOK

     liveOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
     liveOpen path mode flags = putMVar mv "Open" >> return (Right ())

     liveRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
     liveRead path _ byteCount offset = do
      putMVar mv "Read"
      return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) liveString

     liveWrite :: FilePath -> HT -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
     liveWrite path _ byteString offset = do
      putMVar mv "Write"
      putStrLn $ show byteString
      return $ Right $ fromIntegral $ B.length byteString

     liveFlush :: FilePath -> HT -> IO Errno
     liveFlush _ _ = putMVar mv "Flush" >> return eOK

     liveRelease :: FilePath -> HT -> IO ()
     liveRelease _ _ = putMVar mv "Release" >> return ()

     liveSynchronizeFile :: FilePath -> SyncType -> IO Errno
     liveSynchronizeFile _ _ = putMVar mv "SynchronizeFile" >> return eOK

 fuseMain liveFSOps (\e -> print e >> defaultExceptionHandler e)



dirStat ctx = FileStat {
 statEntryType = Directory
 , statFileMode = foldr1 unionFileModes
   [ ownerReadMode
   , ownerExecuteMode
   , groupReadMode
   , groupExecuteMode
   , otherReadMode
   , otherExecuteMode
   , ownerWriteMode
   , groupWriteMode
   , otherWriteMode
   ]
 , statLinkCount        = 2
 , statFileOwner        = fuseCtxUserID ctx
 , statFileGroup        = fuseCtxGroupID ctx
 , statSpecialDeviceID  = 0
 , statFileSize         = 4096
 , statBlocks           = 1
 , statAccessTime       = 0
 , statModificationTime = 0
 , statStatusChangeTime = 0
 }


fileStat ctx = FileStat
 { statEntryType = RegularFile
 , statFileMode = foldr1 unionFileModes
    [ ownerReadMode
    , groupReadMode
    , otherReadMode
    , ownerWriteMode
    , groupWriteMode
    , otherWriteMode
    ]
 , statLinkCount        = 1
 , statFileOwner        = fuseCtxUserID ctx
 , statFileGroup        = fuseCtxGroupID ctx
 , statSpecialDeviceID  = 0
 , statFileSize         = fromIntegral $ B.length liveString
 , statBlocks           = 1
 , statAccessTime       = 0
 , statModificationTime = 0
 , statStatusChangeTime = 0
 }
