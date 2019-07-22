{-|
Module      : IP2Proxy
Description : IP2Proxy Haskell package
Copyright   : (c) IP2Location, 2019
License     : MIT
Maintainer  : sales@ip2location.com
Stability   : experimental

This Haskell package allows users to query an IP address to determine if it was being used as open proxy, web proxy, VPN anonymizer and TOR exits.

IP2Proxy LITE BIN databases are available for free at http://lite.ip2location.com/
-}
module IP2Proxy (Meta, IP2ProxyRecord(..), getModuleVersion, getPackageVersion, getDatabaseVersion, open, getAll, getCountryShort, getCountryLong, getRegion, getCity, getISP, getProxyType, getDomain, getUsageType, getASN, getAS, getLastSeen, isProxy) where

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Word
import Data.Bits
import Data.Binary.Get
import Data.IP
import Control.Exception

-- | Contains proxy results.
data IP2ProxyRecord = IP2ProxyRecord {
    -- | Country code
    country_short :: String,
    -- | Country name
    country_long :: String,
    -- | Region name
    region :: String,
    -- | City name
    city :: String,
    -- | ISP name
    isp :: String,
    -- | Proxy type
    proxy_type :: String,
    -- | Domain
    domain :: String,
    -- | Usage type
    usage_type :: String,
    -- | ASN
    asn :: String,
    -- | AS
    as :: String,
    -- | Last seen
    last_seen :: String,
    -- | Is proxy
    is_proxy :: Int
} deriving (Show)

-- | Contains the BIN database file metadata.
data Meta = Meta {
    -- | Database type
    databasetype :: Int,
    -- | Number of columns
    databasecolumn :: Int,
    -- | Database year
    databaseyear :: Int,
    -- | Database month
    databasemonth :: Int,
    -- | Database day
    databaseday :: Int,
    -- | IPv4 data count
    ipv4databasecount :: Int,
    -- | IPv4 data base address
    ipv4databaseaddr :: Int,
    -- | IPv6 data count
    ipv6databasecount :: Int,
    -- | IPv6 data base address
    ipv6databaseaddr :: Int,
    -- | IPv4 index base address
    ipv4indexbaseaddr :: Int,
    -- | IPv6 index base address
    ipv6indexbaseaddr :: Int,
    -- | IPv4 column size
    ipv4columnsize :: Int,
    -- | IPv6 column size
    ipv6columnsize :: Int
} deriving (Show)

getMeta = do
    databasetype <- getWord8
    databasecolumn <- getWord8
    databaseyear <- getWord8
    databasemonth <- getWord8
    databaseday <- getWord8
    ipv4databasecount <- getWord32le
    ipv4databaseaddr <- getWord32le
    ipv6databasecount <- getWord32le
    ipv6databaseaddr <- getWord32le
    ipv4indexbaseaddr <- getWord32le
    ipv6indexbaseaddr <- getWord32le
    let ipv4columnsize = fromIntegral databasecolumn `shiftL` 2 -- 4 bytes each column
    let ipv6columnsize = 16 + ((fromIntegral databasecolumn - 1) `shiftL` 2) -- 4 bytes each column, except IPFrom column which is 16 bytes
    let meta = Meta (fromIntegral databasetype) (fromIntegral databasecolumn) (fromIntegral databaseyear) (fromIntegral databasemonth) (fromIntegral databaseday) (fromIntegral ipv4databasecount) (fromIntegral ipv4databaseaddr) (fromIntegral ipv6databasecount) (fromIntegral ipv6databaseaddr) (fromIntegral ipv4indexbaseaddr) (fromIntegral ipv6indexbaseaddr) ipv4columnsize ipv6columnsize
    return meta

{-|
    The 'getModuleVersion' function returns a string containing the module version.
-}
getModuleVersion :: String
getModuleVersion = "2.1.0"

{-|
    The 'getPackageVersion' function returns a string containing the package version.
    It takes 1 argument; the metadata from 'open' function (Meta record).
-}
getPackageVersion :: Meta -> String
getPackageVersion meta = (show (databasetype meta))

{-|
    The 'getDatabaseVersion' function returns a string containing the database version.
    It takes 1 argument; the metadata from 'open' function (Meta record).
-}
getDatabaseVersion :: Meta -> String
getDatabaseVersion meta = "20" ++ (show (databaseyear meta)) ++ "." ++ (show (databasemonth meta)) ++ "." ++ (show (databaseday meta))

ipToOcts :: IP -> [Int]
ipToOcts (IPv4 ip) = fromIPv4 ip
ipToOcts (IPv6 ip) = fromIPv6b ip

ipToInteger :: IP -> Integer
ipToInteger = sum . map (\(n,o) -> toInteger o * 256 ^ n) . zip [0..] . reverse . ipToOcts

ipStringToInteger :: String -> Integer
ipStringToInteger = ipToInteger . read

{-|
    The 'open' function returns the Meta record containing metadata from the BIN database file.
    It takes one argument, of type 'String', which is the path to the BIN database file.
-}
open :: String -> IO Meta
open myfile = do
    contents <- BS.readFile myfile
    return $ runGet getMeta contents

readuint8 :: BS.ByteString -> Int -> Int
readuint8 contents startpos = fromIntegral (runGet getWord8 (BS.drop (fromIntegral startpos - 1) contents))

readuint32 :: BS.ByteString -> Int -> Int
readuint32 contents startpos = fromIntegral (runGet getWord32le (BS.drop (fromIntegral startpos - 1) contents))

getuint128 = do
    uint64A <- getWord64le
    uint64B <- getWord64le
    let uint128 = (toInteger uint64A) + ((toInteger uint64B) `rotateL` 64)
    return uint128

readuint128 :: BS.ByteString -> Int -> Integer
readuint128 contents startpos = runGet getuint128 (BS.drop (fromIntegral startpos - 1) contents)

readstr :: BS.ByteString -> Int -> String
readstr contents startpos = do
    let len = runGet getWord8 (BS.drop (fromIntegral startpos) contents)
    str <- BS8.unpack (BS.take (fromIntegral len) (BS.drop (fromIntegral startpos + 1) contents))
    return str

readcolcountry :: BS.ByteString -> Int -> Int -> [Int] -> (String, String)
readcolcountry contents dbtype rowoffset col = do
    let x = "NOT SUPPORTED"
    let [colpos] = take 1 (drop dbtype col)
    
    if colpos == 0
        then do
            (x, x)
        else do
            let coloffset = (colpos - 1) `shiftL` 2
            let x0 = readuint32 contents (rowoffset + coloffset)
            let x1 = readstr contents  x0
            let x2 = readstr contents (x0 + 3)
            (x1, x2)

readcolstring :: BS.ByteString -> Int -> Int -> [Int] -> String
readcolstring contents dbtype rowoffset col = do
    let [colpos] = take 1 (drop dbtype col)
    
    if colpos == 0
        then do
            "NOT SUPPORTED"
        else do
            let coloffset = (colpos - 1) `shiftL` 2
            readstr contents (readuint32 contents (rowoffset + coloffset))

readrecord :: BS.ByteString -> Int -> Int -> Int -> IP2ProxyRecord
readrecord contents dbtype rowoffset mode = do
    let country_position = [0, 2, 3, 3, 3, 3, 3, 3, 3]
    let region_position = [0, 0, 0, 4, 4, 4, 4, 4, 4]
    let city_position = [0, 0, 0, 5, 5, 5, 5, 5, 5]
    let isp_position = [0, 0, 0, 0, 6, 6, 6, 6, 6]
    let proxytype_position = [0, 0, 2, 2, 2, 2, 2, 2, 2]
    let domain_position = [0, 0, 0, 0, 0, 7, 7, 7, 7]
    let usagetype_position = [0, 0, 0, 0, 0, 0, 8, 8, 8]
    let asn_position = [0, 0, 0, 0, 0, 0, 0, 9, 9]
    let as_position = [0, 0, 0, 0, 0, 0, 0, 10, 10]
    let lastseen_position = [0, 0, 0, 0, 0, 0, 0, 0, 11]
     
    let countryshort_field = 1
    let countrylong_field = 2
    let region_field = 4
    let city_field = 8
    let isp_field = 16
    let proxytype_field = 32
    let isproxy_field = 64
    let domain_field = 128
    let usagetype_field = 256
    let asn_field = 512
    let as_field = 1024
    let lastseen_field = 2048
    
    let proxy_type = if (((.&.) mode proxytype_field) /= 0) || (((.&.) mode isproxy_field) /= 0)
        then readcolstring contents dbtype rowoffset proxytype_position
        else ""
    
    let (country_short, country_long) = if (((.&.) mode countryshort_field) /= 0) || (((.&.) mode countrylong_field) /= 0) || (((.&.) mode isproxy_field) /= 0)
        then readcolcountry contents dbtype rowoffset country_position
        else ("", "")
    
    let region = if ((.&.) mode region_field) /= 0
        then readcolstring contents dbtype rowoffset region_position
        else ""
    
    let city = if ((.&.) mode city_field) /= 0
        then readcolstring contents dbtype rowoffset city_position
        else ""
    
    let isp = if ((.&.) mode isp_field) /= 0
        then readcolstring contents dbtype rowoffset isp_position
        else ""
    
    let domain = if ((.&.) mode domain_field) /= 0
        then readcolstring contents dbtype rowoffset domain_position
        else ""
    
    let usage_type = if ((.&.) mode usagetype_field) /= 0
        then readcolstring contents dbtype rowoffset usagetype_position
        else ""
    
    let asn = if ((.&.) mode asn_field) /= 0
        then readcolstring contents dbtype rowoffset asn_position
        else ""
    
    let as = if ((.&.) mode as_field) /= 0
        then readcolstring contents dbtype rowoffset as_position
        else ""
    
    let last_seen = if ((.&.) mode lastseen_field) /= 0
        then readcolstring contents dbtype rowoffset lastseen_position
        else ""
    
    let is_proxy = if (country_short == "-") || (proxy_type == "-")
        then 0
        else if (proxy_type == "DCH") || (proxy_type == "SES")
            then 2
            else 1
    
    IP2ProxyRecord country_short country_long region city isp proxy_type domain usage_type asn as last_seen is_proxy

searchtree :: BS.ByteString -> Integer -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IP2ProxyRecord
searchtree contents ipnum dbtype low high baseaddr colsize iptype mode = do
    if low <= high
        then do
            let mid = ((low + high) `shiftR` 1)
            let rowoffset = baseaddr + (mid * colsize)
            let rowoffset2 = rowoffset + colsize
            
            let ipfrom = if (iptype == 4)
                then toInteger $ readuint32 contents rowoffset
                else readuint128 contents rowoffset
            
            let ipto = if (iptype == 4)
                then toInteger $ readuint32 contents rowoffset2
                else readuint128 contents rowoffset2
            
            if ipnum >= ipfrom && ipnum < ipto
                then do
                    if iptype == 4
                        then
                            readrecord contents dbtype rowoffset mode
                        else
                            readrecord contents dbtype (rowoffset + 12) mode
                else if ipnum < ipfrom
                    then
                        searchtree contents ipnum dbtype low (mid - 1) baseaddr colsize iptype mode
                    else
                        searchtree contents ipnum dbtype (mid + 1) high baseaddr colsize iptype mode
        else do
            let x = "INVALID IP ADDRESS"
            IP2ProxyRecord x x x x x x x x x x x (-1)
        
search4 :: BS.ByteString -> Integer -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IP2ProxyRecord
search4 contents ipnum dbtype low high baseaddr indexbaseaddr colsize mode = do
    if indexbaseaddr > 0
        then do
            let indexpos = fromIntegral (((ipnum `rotateR` 16) `rotateL` 3) + (toInteger indexbaseaddr))
            let low2 = readuint32 contents indexpos
            let high2 = readuint32 contents (indexpos + 4)
            searchtree contents ipnum dbtype low2 high2 baseaddr colsize 4 mode
        else
            searchtree contents ipnum dbtype low high baseaddr colsize 4 mode

search6 :: BS.ByteString -> Integer -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> IP2ProxyRecord
search6 contents ipnum dbtype low high baseaddr indexbaseaddr colsize mode = do
    if indexbaseaddr > 0
        then do
            let indexpos = fromIntegral (((ipnum `rotateR` 112) `rotateL` 3) + (toInteger indexbaseaddr))
            let low2 = readuint32 contents indexpos
            let high2 = readuint32 contents (indexpos + 4)
            searchtree contents ipnum dbtype low2 high2 baseaddr colsize 6 mode
        else
            searchtree contents ipnum dbtype low high baseaddr colsize 6 mode

tryfirst myIP = do
    result <- try (evaluate (ipStringToInteger myIP)) :: IO (Either SomeException Integer)
    case result of
        Left ex -> return $ toInteger (1 - 2)
        Right val -> return val

{-|
    The 'getAll' function returns an IP2ProxyRecord containing proxy data for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getAll :: String -> Meta -> String -> IO IP2ProxyRecord
getAll myfile meta myip = do
    result <- doQuery myfile meta myip 4095
    return result

{-|
    The 'getCountryShort' function returns the country code for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getCountryShort :: String -> Meta -> String -> IO String
getCountryShort myfile meta myip = do
    result <- doQuery myfile meta myip 1
    return (show (country_short result))

{-|
    The 'getCountryLong' function returns the country name for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getCountryLong :: String -> Meta -> String -> IO String
getCountryLong myfile meta myip = do
    result <- doQuery myfile meta myip 2
    return (show (country_long result))

{-|
    The 'getRegion' function returns the region name for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getRegion :: String -> Meta -> String -> IO String
getRegion myfile meta myip = do
    result <- doQuery myfile meta myip 4
    return (show (region result))

{-|
    The 'getCity' function returns the city name for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getCity :: String -> Meta -> String -> IO String
getCity myfile meta myip = do
    result <- doQuery myfile meta myip 8
    return (show (city result))

{-|
    The 'getISP' function returns the ISP name for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getISP :: String -> Meta -> String -> IO String
getISP myfile meta myip = do
    result <- doQuery myfile meta myip 16
    return (show (isp result))

{-|
    The 'getProxyType' function returns the proxy type for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getProxyType :: String -> Meta -> String -> IO String
getProxyType myfile meta myip = do
    result <- doQuery myfile meta myip 32
    return (show (proxy_type result))

{-|
    The 'getDomain' function returns the domain name for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getDomain :: String -> Meta -> String -> IO String
getDomain myfile meta myip = do
    result <- doQuery myfile meta myip 128
    return (show (domain result))

{-|
    The 'getUsageType' function returns the usage type for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getUsageType :: String -> Meta -> String -> IO String
getUsageType myfile meta myip = do
    result <- doQuery myfile meta myip 256
    return (show (usage_type result))

{-|
    The 'getASN' function returns the autonomous system number for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getASN :: String -> Meta -> String -> IO String
getASN myfile meta myip = do
    result <- doQuery myfile meta myip 512
    return (show (asn result))

{-|
    The 'getAS' function returns the autonomous system name for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getAS :: String -> Meta -> String -> IO String
getAS myfile meta myip = do
    result <- doQuery myfile meta myip 1024
    return (show (as result))

{-|
    The 'getLastSeen' function returns the number of days last seen for an IP address.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
getLastSeen :: String -> Meta -> String -> IO String
getLastSeen myfile meta myip = do
    result <- doQuery myfile meta myip 2048
    return (show (last_seen result))

{-|
    The 'isProxy' function returns 0 if IP is not a proxy, 1 if is a proxy and not data center IP, 2 if is a proxy and is a data center IP, -1 if error.
    It takes 3 arguments; the BIN database file path (String), the metadata from 'open' function (Meta record) & either IPv4 or IPv6 address (String).
-}
isProxy :: String -> Meta -> String -> IO String
isProxy myfile meta myip = do
    result <- doQuery myfile meta myip 64
    return (show (is_proxy result))

doQuery :: String -> Meta -> String -> Int -> IO IP2ProxyRecord
doQuery myfile meta myip mode = do
    contents <- BS.readFile myfile
    let fromV4Mapped = 281470681743360
    let toV4Mapped = 281474976710655
    let fromV4Compatible = 0
    let toV4Compatible = 4294967295
    let from6To4 = 42545680458834377588178886921629466624
    let to6To4 = 42550872755692912415807417417958686719
    let fromTeredo = 42540488161975842760550356425300246528
    let toTeredo = 42540488241204005274814694018844196863
    let last32Bits = 4294967295
    
    ipnum <- tryfirst myip
    if ipnum == -1
        then do
            let x = "INVALID IP ADDRESS"
            return $ IP2ProxyRecord x x x x x x x x x x x (-1)
        else if ipnum >= fromV4Mapped && ipnum <= toV4Mapped
            then do
                return $ search4 contents (ipnum - (toInteger fromV4Mapped)) (databasetype meta) 0 (ipv4databasecount meta) (ipv4databaseaddr meta) (ipv4indexbaseaddr meta) (ipv4columnsize meta) mode
            else if ipnum >= from6To4 && ipnum <= to6To4
                then do
                    return $ search4 contents ((ipnum `rotateR` 80) .&. last32Bits) (databasetype meta) 0 (ipv4databasecount meta) (ipv4databaseaddr meta) (ipv4indexbaseaddr meta) (ipv4columnsize meta) mode
                else if ipnum >= fromTeredo && ipnum <= toTeredo
                    then do
                        return $ search4 contents ((complement ipnum) .&. last32Bits) (databasetype meta) 0 (ipv4databasecount meta) (ipv4databaseaddr meta) (ipv4indexbaseaddr meta) (ipv4columnsize meta) mode
                    else if ipnum >= fromV4Compatible && ipnum <= toV4Compatible
                        then do
                            return $ search4 contents ipnum (databasetype meta) 0 (ipv4databasecount meta) (ipv4databaseaddr meta) (ipv4indexbaseaddr meta) (ipv4columnsize meta) mode
                        else do
                            return $ search6 contents ipnum (databasetype meta) 0 (ipv6databasecount meta) (ipv6databaseaddr meta) (ipv6indexbaseaddr meta) (ipv6columnsize meta) mode
