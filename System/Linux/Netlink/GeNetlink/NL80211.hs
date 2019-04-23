{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-|
Module      : System.Linux.Netlink.GeNetlink.NL80211
Description : Implementation of NL80211
Maintainer  : ongy
Stability   : testing
Portability : Linux

This module providis utility functions for NL80211 subsystem.
For more information see /usr/include/linux/nl80211.h
-}
module System.Linux.Netlink.GeNetlink.NL80211
  ( NL80211Socket
  , NL80211Packet

  , makeNL80211Socket
  , joinMulticastByName
  , queryOne
  , query
  , getInterfaceList
  , getScanResults
  , getConnectedWifi
  , getWifiAttributes
  , getPacket
  , getFd
  , getMulticastGroups
  )
where

#if MIN_VERSION_base(4,8,0)
#else
import Control.Applicative ((<$>))
#endif

import Data.Bits ((.|.))
import Data.ByteString.Char8 (unpack)
import Data.List (intercalate)
import Data.Maybe (mapMaybe)
import Data.Serialize.Get (runGet, getWord32host)
import Data.Serialize.Put (runPut, putWord32host)
import Data.Word (Word32, Word16, Word8)

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (length)
import qualified Data.Map as M (empty, lookup, fromList, member, toList)

import System.Posix.Types (Fd)

import System.Linux.Netlink.Helpers (indent)
import System.Linux.Netlink.Constants
import System.Linux.Netlink.GeNetlink
import System.Linux.Netlink.GeNetlink.Control hiding (getMulticastGroups)
import qualified System.Linux.Netlink.GeNetlink.Control as C
import System.Linux.Netlink.GeNetlink.NL80211.Constants
import System.Linux.Netlink.GeNetlink.NL80211.StaInfo
import System.Linux.Netlink.GeNetlink.NL80211.WifiEI
import System.Linux.Netlink hiding (makeSocket, queryOne, query, recvOne, getPacket)
import qualified System.Linux.Netlink as I (queryOne, query, recvOne)

-- The Netlink socket with Family Id, so we don't need as many arguments
-- everywhere
-- |Wrapper for 'NetlinkSocket' we also need the family id for messages we construct
data NL80211Socket = NLS NetlinkSocket Word16

data NoData80211 = NoData80211 deriving (Eq, Show)

instance Convertable NoData80211 where
  getPut _ = return ()
  getGet _ = return NoData80211

-- |typedef for messages send by this mdoule
type NL80211Packet = GenlPacket NoData80211

instance Show NL80211Packet where
  showList xs = ((intercalate "===\n" . map show $xs) ++)
  show (Packet _ cus attrs) =
    "NL80211Packet: " ++ showNL80211Command cus ++ "\n" ++
    "Attrs: \n" ++ concatMap showNL80211Attr (M.toList attrs) ++ "\n"
  show p = showPacket p

showNL80211Command :: (GenlData NoData80211) -> String
showNL80211Command (GenlData (GenlHeader cmd _) _ ) =
  showNL80211Commands cmd

showNL80211Attr :: (Int, ByteString) -> String
showNL80211Attr (i, v)
  | i == eNL80211_ATTR_STA_INFO = showStaInfo v
  | i == eNL80211_ATTR_RESP_IE = showWifiEid v
  | i == eNL80211_ATTR_BSS = showAttrBss v
  | otherwise = showAttr showNL80211Attrs (i, v)

showStaInfo :: ByteString -> String
showStaInfo bs = let attrs = getRight $ runGet getAttributes bs in
    "NL80211_ATTR_STA_INFO: " ++ show (BS.length bs) ++ "\n" ++
    (indent . show . staInfoFromAttributes $ attrs)

showAttrBss :: ByteString -> String
showAttrBss bs = let attrs = getRight $ runGet getAttributes bs in
  "NL80211_ATTR_BSS: " ++ show (BS.length bs) ++ "\n" ++
  (indent . concatMap showBssAttr $ M.toList attrs)

showBssAttr :: (Int, ByteString) -> String
showBssAttr (i, v)
  | i == eNL80211_BSS_INFORMATION_ELEMENTS = "NL80211_BSS_INFORMATION_ELEMENTS " ++ showWifiEid v
  | i == eNL80211_BSS_BEACON_IES = "NL80211_BSS_BEACON_IES " ++ showWifiEid v
  | otherwise = showAttr showNL80211Bss (i, v)

-- |Get the raw fd from a 'NL80211Socket'. This can be used for eventing
getFd :: NL80211Socket -> Fd
getFd (NLS s _) = getNetlinkFd s

getRight :: Show a => Either a b -> b
getRight (Right x) = x
getRight (Left err) = error $show err


-- |Create a 'NL80211Socket' this opens a genetlink socket and gets the family id
makeNL80211Socket :: IO NL80211Socket
makeNL80211Socket = do
  sock <- makeSocket
  fid <- getFamilyId sock "nl80211"
  return $NLS sock fid


-- |Join a nl80211 multicast group by name
joinMulticastByName :: NL80211Socket -> String -> IO ()
joinMulticastByName (NLS sock _) name = do
  (_, m) <- getFamilyWithMulticasts sock "nl80211"
  let gid = getMulticast name m
  case gid of
    Nothing -> error $"Could not find \"" ++ name  ++ "\" multicast group"
    Just x -> joinMulticastGroup sock x


-- |Get the names of all multicast groups this nl80211 implementation provides
getMulticastGroups :: NL80211Socket -> IO [String]
getMulticastGroups (NLS sock fid) =
  map grpName <$> C.getMulticastGroups sock fid


getRequestPacket :: Word16 -> Word8 -> Bool -> Attributes -> NL80211Packet
getRequestPacket fid cmd dump attrs =
  let header = Header (fromIntegral fid) flags 0 0
      geheader = GenlHeader cmd 0 in
    Packet header (GenlData geheader NoData80211) attrs
  where flags = if dump then fNLM_F_REQUEST .|. fNLM_F_MATCH .|. fNLM_F_ROOT else fNLM_F_REQUEST


-- |queryOne for NL80211 (see 'System.Linux.Netlink.queryOne')
queryOne :: NL80211Socket -> Word8 -> Bool -> Attributes -> IO NL80211Packet
queryOne (NLS sock fid) cmd dump attrs = I.queryOne sock packet


  where packet = getRequestPacket fid cmd dump attrs

-- |query for NL80211 (see 'System.Linux.Netlink.query')
query :: NL80211Socket -> Word8 -> Bool -> Attributes -> IO [NL80211Packet]
query (NLS sock fid) cmd dump attrs = I.query sock packet
  where packet = getRequestPacket fid cmd dump attrs


parseInterface :: (ByteString, ByteString) -> (String, Word32)
parseInterface (name, ifindex) = 
  --This init is ok because the name will always have a \0
  (init $unpack name, getRight $runGet getWord32host ifindex)


-- |Get the list of interfaces currently managed by NL80211
getInterfaceList :: NL80211Socket -> IO [(String, Word32)]
getInterfaceList sock = do
  interfaces <- query sock eNL80211_CMD_GET_INTERFACE True M.empty
  return $ mapMaybe (fmap parseInterface . toTuple) interfaces
  where toTuple :: NL80211Packet -> Maybe (ByteString, ByteString)
        toTuple (Packet _ _ attrs) = do
          name <- M.lookup eNL80211_ATTR_IFNAME attrs
          findex <- M.lookup eNL80211_ATTR_IFINDEX attrs
          return (name, findex)
        toTuple x@(ErrorMsg{}) =
          error ("Something happend while getting the interfaceList: " ++ show x)
        toTuple (DoneMsg _) = Nothing


{- |get scan results

In testing this could be a big chunk of data when a scan just happened
or be pretty much only the currently connected wifi.

For more information about how this is structured look into kernel source
or just try it out.
-}
getScanResults
  :: NL80211Socket
  -> Word32 -- ^The id of the interface for which this should be looked up
  -> IO [NL80211Packet]
getScanResults sock ifindex = query sock eNL80211_CMD_GET_SCAN True attrs
  where attrs = M.fromList [(eNL80211_ATTR_IFINDEX, runPut $putWord32host ifindex)]

{- |Get the information about the currently connected wifi(s).

This would technically work for multiple connected wifis, but since we only get
information about one interface this should only ever be emtpy on a singleton list.

For more information about how this is structured look into kernel soruce
or just try it out.
-}
getConnectedWifi
  :: NL80211Socket
  -> Word32 -- ^The id of the interface for which this should be looked up
  -> IO [NL80211Packet]
getConnectedWifi sock ifindex = filter isConn <$> getScanResults sock ifindex
  where isConn :: NL80211Packet -> Bool
        isConn (Packet _ _ attrs) = hasConn $M.lookup eNL80211_ATTR_BSS attrs
  -- -16 is -EBUSY, which will be returned IF and (as far as I could see) only IF another dump
  -- is already in progress, so retrying should get something useful
  -- For other error codes we don't know for sure and want to return the error to the user
        isConn x@(ErrorMsg _ e _) = if e == (-16)
          then False
          else error ("Something stupid happened" ++ show x)
        isConn (DoneMsg _) = False
        hasConn Nothing = False
        hasConn (Just attrs) = M.member eNL80211_BSS_STATUS $getRight $runGet getAttributes attrs


-- |Get the EID attributes from a 'NL80211Packet' (for example from 'getConnectedWifi'
getWifiAttributes :: NL80211Packet -> Maybe Attributes
getWifiAttributes (Packet _ _ attrs) = getRight <$> runGet getWifiEIDs <$> eids
  where bssattrs = getRight . runGet getAttributes <$> M.lookup eNL80211_ATTR_BSS attrs
        eids = M.lookup eNL80211_BSS_INFORMATION_ELEMENTS =<< bssattrs
getWifiAttributes x@(ErrorMsg{}) = error ("Something stupid happened" ++ show x)
getWifiAttributes (DoneMsg _) = Nothing


-- |NL80211 version of 'System.Linux.Netlink.recvOne'
getPacket :: NL80211Socket -> IO [NL80211Packet]
getPacket (NLS sock _) = I.recvOne sock
