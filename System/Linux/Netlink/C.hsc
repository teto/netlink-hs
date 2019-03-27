{-# LANGUAGE ForeignFunctionInterface #-}
{-|
Module      : System.Linux.Netlink.C
Description : A module to bridge the haskell code to underlying C code
Maintainer  : ongy
Stability   : testing
Portability : Linux

I consider this module internal.
The documentation may be a bit sparse.
-}
module System.Linux.Netlink.C
    ( makeSocket
    , makeSocketGeneric
    , closeSocket
    , sendmsg
    , recvmsg
    , joinMulticastGroup
    , leaveMulticastGroup
    )
where

-- hack described at https://github.com/haskell/network/issues/43#issuecomment-7631533
-- to word around hsc2hs failure ?
#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>), (<*))
#endif

import Control.Monad (when)
import Data.ByteString (ByteString)
import Data.ByteString.Internal (createAndTrim, toForeignPtr)
import Data.Word (Word32)
import Foreign.C.Error (throwErrnoIf, throwErrnoIfMinus1, throwErrnoIfMinus1_)
import Foreign.C.Types
import Foreign.ForeignPtr (touchForeignPtr)
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr)
import Foreign.Marshal.Array (withArrayLen)
import Foreign.Marshal.Utils (with)
import Foreign.Ptr (Ptr, castPtr, plusPtr)
import Foreign.Storable (Storable(..))

import System.Linux.Netlink.Constants (eAF_NETLINK, eNETLINK_ADD_MEMBERSHIP, eNETLINK_DROP_MEMBERSHIP)

#include <unistd.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <linux/netlink.h>

-- FFI declarations for clib syscall wrappers
-- So if we are not blocking long or calling back into haskell it should be ok to do unsafe imports?
-- These should be done fast, and we know the will never call back into haskell
foreign import ccall unsafe "socket" c_socket :: CInt -> CInt -> CInt -> IO CInt
foreign import ccall unsafe "bind" c_bind :: CInt -> Ptr SockAddrNetlink -> Int -> IO CInt
foreign import ccall unsafe "close" c_close :: CInt -> IO CInt
foreign import ccall unsafe "setsockopt" c_setsockopt :: CInt -> CInt -> CInt -> Ptr a -> CInt -> IO CInt
foreign import ccall unsafe "memset" c_memset :: Ptr a -> CInt -> CInt -> IO ()

-- those two may block for a while, so we'll not do unsafe for them
foreign import ccall "sendmsg" c_sendmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CInt
foreign import ccall "recvmsg" c_recvmsg :: CInt -> Ptr MsgHdr -> CInt -> IO CInt

data SockAddrNetlink = SockAddrNetlink Word32

instance Storable SockAddrNetlink where
    sizeOf    _ = #{size struct sockaddr_nl}
    alignment _ = 4
    peek p = do
        family <- #{peek struct sockaddr_nl, nl_family} p
        when ((family :: CShort) /= eAF_NETLINK) $ fail "Bad address family"
        SockAddrNetlink . fromIntegral <$> (#{peek struct sockaddr_nl, nl_pid} p :: IO CUInt)
    poke p (SockAddrNetlink pid) = do
        zero p
        #{poke struct sockaddr_nl, nl_family} p (eAF_NETLINK :: CShort)
        #{poke struct sockaddr_nl, nl_pid   } p (fromIntegral pid :: CUInt)

data IoVec = IoVec (Ptr (), Int)

instance Storable IoVec where
    sizeOf    _ = #{size struct iovec}
    alignment _ = 4
    peek p = do
        addr <- #{peek struct iovec, iov_base} p
        len  <- #{peek struct iovec, iov_len}  p :: IO CSize
        return $ IoVec (addr, fromIntegral len)
    poke p (IoVec (addr, len)) = do
        zero p
        #{poke struct iovec, iov_base} p addr
        #{poke struct iovec, iov_len } p (fromIntegral len :: CSize)

data MsgHdr = MsgHdr (Ptr (), Int)

instance Storable MsgHdr where
    sizeOf    _ = #{size struct msghdr}
    alignment _ = 4
    peek p = do
        iov     <- #{peek struct msghdr, msg_iov   } p
        iovlen  <- #{peek struct msghdr, msg_iovlen} p :: IO CSize
        return $ MsgHdr (iov, fromIntegral iovlen)
    poke p (MsgHdr (iov, iovlen)) = do
        zero p
        #{poke struct msghdr, msg_iov   } p iov
        #{poke struct msghdr, msg_iovlen} p (fromIntegral iovlen :: CSize)


-- |Create a netlink socket, for legacy reasons this will be of the route family
makeSocket :: IO CInt
makeSocket = makeSocketGeneric #{const NETLINK_ROUTE}

-- TODO maybe readd the unique thingy (look at git log)
-- |Create any netlink socket
makeSocketGeneric 
  :: Int -- ^The netlink family to use
  -> IO CInt
makeSocketGeneric prot = do
  fd <- throwErrnoIfMinus1 "makeSocket.socket" $
          c_socket eAF_NETLINK #{const SOCK_RAW} (fromIntegral prot)
  -- we need to bind or joining multicast groups will be useless
  with (SockAddrNetlink 0) $ \addr ->
    throwErrnoIfMinus1_ "makeSocket.bind" $
      c_bind fd (castPtr addr) #{size struct sockaddr_nl}
  return fd


-- |Close a socket when it is not needed anymore
closeSocket :: CInt -> IO ()
closeSocket fd = throwErrnoIfMinus1_ "closeSocket" $ c_close fd

-- |Send a message over a socket.
sendmsg :: CInt -> [ByteString] -> IO ()
sendmsg fd bs =
    useManyAsPtrLen bs $ \ptrs ->
    withArrayLen (map IoVec ptrs) $ \iovlen iov ->
    with (MsgHdr (castPtr iov, iovlen)) $ \msg ->
    throwErrnoIfMinus1_ "sendmsg" $c_sendmsg fd (castPtr msg) (0 :: CInt)

-- |Receive a message over a socket.
recvmsg :: CInt -> Int -> IO ByteString
recvmsg fd len =
    createAndTrim len $ \ptr ->
    with (IoVec (castPtr ptr, len)) $ \vec ->
    with (MsgHdr (castPtr vec, 1)) $ \msg ->
    fmap fromIntegral . throwErrnoIf (<= 0) "recvmsg" $
        c_recvmsg fd (castPtr msg) (0 :: CInt)

useManyAsPtrLen :: [ByteString] -> ([(Ptr (), Int)] -> IO a) -> IO a
useManyAsPtrLen bs act =
    let makePtrLen (fptr, off, len) =
            let ptr = plusPtr (unsafeForeignPtrToPtr fptr) off
            in (ptr, len)
        touchByteStringPtr (fptr, _, _) = touchForeignPtr fptr
        foreigns = map toForeignPtr bs
    in act (map makePtrLen foreigns) <* mapM_ touchByteStringPtr foreigns

sizeOfPtr :: (Storable a, Integral b) => Ptr a -> b
sizeOfPtr = fromIntegral . sizeOf . (undefined :: Ptr a -> a)

zero :: Storable a => Ptr a -> IO ()
zero p = void $ c_memset (castPtr p) 0 (sizeOfPtr p)

void :: Monad m => m a -> m ()
void act = act >> return ()


-- |Set membership to netlink multicast group
joinOrLeaveMulticastGroup :: Bool -> CInt -> Word32 -> IO ()
joinOrLeaveMulticastGroup beMember fd fid = do
  _ <- throwErrnoIfMinus1 "joinMulticast" $ with fid (\ptr ->
    c_setsockopt fd sol_netlink value (castPtr ptr) size)
  return ()
  where
    size = fromIntegral $sizeOf (undefined :: CInt)
    sol_netlink = 270 :: CInt
    value = if beMember
      then eNETLINK_ADD_MEMBERSHIP
      else eNETLINK_DROP_MEMBERSHIP

-- |Join a netlink multicast group
joinMulticastGroup :: CInt -> Word32 -> IO ()
joinMulticastGroup = joinOrLeaveMulticastGroup True

-- |Leave a netlink multicast group
leaveMulticastGroup :: CInt -> Word32 -> IO ()
leaveMulticastGroup = joinOrLeaveMulticastGroup False


