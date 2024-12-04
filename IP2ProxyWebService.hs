{-# LANGUAGE OverloadedStrings,TemplateHaskell #-}
{-|
Module      : IP2ProxyWebService
Description : IP2Proxy Haskell package
Copyright   : (c) IP2Location, 2018 - 2024
License     : MIT
Maintainer  : sales@ip2location.com
Stability   : experimental

This Haskell package allows users to query an IP address to determine if it was being used as open proxy, web proxy, VPN anonymizer and TOR exits.

IP2Proxy Web Service API subscription at https://www.ip2location.com/web-service/ip2proxy
-}
module IP2ProxyWebService (WSResult(..), WSConfig, openWS, lookUp, getCredit) where

import Control.Exception
import System.Exit
import Data.Aeson as DA
import Data.Aeson.TH
import Network.HTTP.Client
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Status (statusCode)
import Data.Maybe
import Network.URI.Encode as URIE
-- import Text.Regex.Base
-- import Text.Regex.TDFA

-- | Contains the web service configuration.
data WSConfig = WSConfig {
    -- | Web service API key
    apiKey  :: String,
    -- | API package
    apiPackage :: String,
    -- | Use SSL
    useSSL :: Bool
} deriving (Show)

-- | Contains the web service results.
data WSResult = WSResult {
    -- | Response status or error
    response  :: String,
    -- | Country code
    countryCode :: Maybe String,
    -- | Country name
    countryName :: Maybe String,
    -- | Region name
    regionName :: Maybe String,
    -- | City name
    cityName :: Maybe String,
    -- | ISP name
    isp :: Maybe String,
    -- | Domain
    domain :: Maybe String,
    -- | Usage type
    usageType :: Maybe String,
    -- | Autonomous System Number
    asn :: Maybe String,
    -- | Autonomous System
    as :: Maybe String,
    -- | Proxy last seen in days
    lastSeen :: Maybe String,
    -- | Proxy type
    proxyType :: Maybe String,
    -- | Threat type
    threat :: Maybe String,
    -- | Whether is a proxy
    isProxy :: Maybe String,
    -- | VPN provider name
    provider :: Maybe String
} deriving (Show, Eq)

$(deriveJSON defaultOptions ''WSResult)

checkparams :: String -> String -> IO String
checkparams apikey apipackage = do
    return "OK"
    --- regex part commented out due to cabal dependency issues
    -- let apikeyok = apikey =~ ("^[0-9A-Z]{10}$" :: String) :: Bool
    -- if apikeyok == False
        -- then die(show "Invalid API key.")
        -- else do
            -- let apipackageok = apipackage =~ ("^PX[0-9]+$" :: String) :: Bool
            -- if apipackageok == False
                -- then die(show "Invalid package name.")
                -- else return "OK"

{-|
    The 'openWS' function initializes the web service configuration.
    It takes 3 arguments; the web service API key, the API package to call & whether to use SSL.
-}
openWS :: String -> String -> Bool -> IO WSConfig
openWS apikey apipackage usessl = do
    paramok <- checkparams apikey apipackage
    return (WSConfig apikey apipackage usessl)

{-|
    The 'lookUp' function returns an WSResult containing proxy data for an IP address.
    It takes 2 arguments; the web service configuration from 'openWS' function (WSConfig record) & either IPv4 or IPv6 address (String).
-}
lookUp :: WSConfig -> String -> IO WSResult
lookUp myconfig ip = do
    let key = apiKey myconfig
    let package = apiPackage myconfig
    let usessl = useSSL myconfig

    paramok <- checkparams key package
    let protocol = if usessl == True
        then "https"
        else "http"
    manager <- newManager tlsManagerSettings
    httprequest <- parseRequest $ protocol ++ "://api.ip2proxy.com/?key=" ++ key ++ "&package=" ++ package ++ "&ip=" ++ (URIE.encode ip)
    httpresponse <- httpLbs httprequest manager
    let json = responseBody httpresponse
    let Just result = DA.decode json :: Maybe WSResult
    return result

{-|
    The 'getCredit' function returns an WSResult containing web service credit balance for the API key.
    It takes 1 argument; the web service configuration from 'openWS' function (WSConfig record).
-}
getCredit :: WSConfig -> IO WSResult
getCredit myconfig = do
    let key = apiKey myconfig
    let package = apiPackage myconfig
    let usessl = useSSL myconfig

    paramok <- checkparams key package
    let protocol = if usessl == True
        then "https"
        else "http"
    manager <- newManager tlsManagerSettings
    httprequest <- parseRequest $ protocol ++ "://api.ip2proxy.com/?key=" ++ key ++ "&check=true"
    httpresponse <- httpLbs httprequest manager
    let json = responseBody httpresponse
    let Just result = DA.decode json :: Maybe WSResult
    return result
