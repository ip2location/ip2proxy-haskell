# IP2Proxy Haskell Module

This Haskell package allows user to query an IP address if it was being used as VPN anonymizer, open proxies, web proxies, Tor exits, data center, web hosting (DCH) range, search engine robots (SES) and residential (RES). It lookup the proxy IP address from **IP2Proxy BIN Data** file. This data file can be downloaded at

* Free IP2Proxy BIN Data: https://lite.ip2location.com
* Commercial IP2Proxy BIN Data: https://www.ip2location.com/database/ip2proxy

As an alternative, this package can also call the IP2Proxy Web Service. This requires an API key. If you don't have an existing API key, you can subscribe for one at the below:

https://www.ip2location.com/web-service/ip2proxy

## Installation

```bash
cabal install IP2Proxy
```

## QUERY USING THE BIN FILE

## Methods
Below are the methods supported in this package.

|Method Name|Description|
|---|---|
|open|Open the IP2Proxy BIN data for lookup.|
|getPackageVersion|Get the package version (1 to 11 for PX1 to PX11 respectively).|
|getModuleVersion|Get the module version.|
|getDatabaseVersion|Get the database version.|
|isProxy|Check whether if an IP address was a proxy. Returned value:<ul><li>-1 : errors</li><li>0 : not a proxy</li><li>1 : a proxy</li><li>2 : a data center IP address or search engine robot</li></ul>|
|getAll|Return the proxy information in a record.|
|getProxyType|Return the proxy type. Please visit <a href="https://www.ip2location.com/database/px10-ip-proxytype-country-region-city-isp-domain-usagetype-asn-lastseen-threat-residential" target="_blank">IP2Location</a> for the list of proxy types supported.|
|getCountryShort|Return the ISO3166-1 country code (2-digits) of the proxy.|
|getCountryLong|Return the ISO3166-1 country name of the proxy.|
|getRegion|Return the ISO3166-2 region name of the proxy. Please visit <a href="https://www.ip2location.com/free/iso3166-2" target="_blank">ISO3166-2 Subdivision Code</a> for the information of ISO3166-2 supported.|
|getCity|Return the city name of the proxy.|
|getISP|Return the ISP name of the proxy.|
|getDomain|Return the domain name of the proxy.|
|getUsageType|Return the usage type classification of the proxy. Please visit <a href="https://www.ip2location.com/database/px10-ip-proxytype-country-region-city-isp-domain-usagetype-asn-lastseen-threat-residential" target="_blank">IP2Location</a> for the list of usage types supported.|
|getASN|Return the autonomous system number of the proxy.|
|getAS|Return the autonomous system name of the proxy.|
|getLastSeen|Return the number of days that the proxy was last seen.|
|getThreat|Return the threat type of the proxy.|
|getProvider|Return the provider of the proxy.|

## Usage

```haskell
import IP2Proxy

main :: IO ()
main = do
    let myfile = "./IP2PROXY-IP-PROXYTYPE-COUNTRY-REGION-CITY-ISP-DOMAIN-USAGETYPE-ASN-LASTSEEN-THREAT-RESIDENTIAL-PROVIDER.BIN"
    let ip = "199.83.103.79"
    meta <- open myfile
    
    putStrLn $ "module_version: " ++ getModuleVersion
    putStrLn $ "package_version: " ++ (getPackageVersion meta)
    putStrLn $ "database_version: " ++ (getDatabaseVersion meta)
    
    result <- getAll myfile meta ip
    putStrLn $ "country_short: " ++ (show (country_short result))
    putStrLn $ "country_long: " ++ (show (country_long result))
    putStrLn $ "region: " ++ (show (region result))
    putStrLn $ "city: " ++ (show (city result))
    putStrLn $ "isp: " ++ (show (isp result))
    putStrLn $ "proxy_type: " ++ (show (proxy_type result))
    putStrLn $ "domain: " ++ (show (domain result))
    putStrLn $ "usage_type: " ++ (show (usage_type result))
    putStrLn $ "asn: " ++ (show (asn result))
    putStrLn $ "as: " ++ (show (as result))
    putStrLn $ "last_seen: " ++ (show (last_seen result))
    putStrLn $ "threat: " ++ (show (threat result))
    putStrLn $ "provider: " ++ (show (provider result))
    putStrLn $ "is_proxy: " ++ (show (is_proxy result))

    result <- getCountryShort myfile meta ip
    putStrLn $ "country_short: " ++ result
    result <- getCountryLong myfile meta ip
    putStrLn $ "country_long: " ++ result
    result <- getRegion myfile meta ip
    putStrLn $ "region: " ++ result
    result <- getCity myfile meta ip
    putStrLn $ "city: " ++ result
    result <- getISP myfile meta ip
    putStrLn $ "isp: " ++ result
    result <- getProxyType myfile meta ip
    putStrLn $ "proxy_type: " ++ result
    result <- getDomain myfile meta ip
    putStrLn $ "domain: " ++ result
    result <- getUsageType myfile meta ip
    putStrLn $ "usage_type: " ++ result
    result <- getASN myfile meta ip
    putStrLn $ "asn: " ++ result
    result <- getAS myfile meta ip
    putStrLn $ "as: " ++ result
    result <- getLastSeen myfile meta ip
    putStrLn $ "last_seen: " ++ result
    result <- getThreat myfile meta ip
    putStrLn $ "threat: " ++ result
    result <- getProvider myfile meta ip
    putStrLn $ "provider: " ++ result
    result <- isProxy myfile meta ip
    putStrLn $ "is_proxy: " ++ result
```

## QUERY USING THE IP2PROXY PROXY DETECTION WEB SERVICE

## Methods
Below are the methods supported in this package.

|Method Name|Description|
|---|---|
|openWS| Expects 3 input parameters:<ol><li>IP2Proxy API Key.</li><li>Package (PX1 - PX11)</li></li><li>Use HTTPS or HTTP</li></ol> |
|lookUp|Query IP address. This method returns a WSResult record containing the proxy info. <ul><li>countryCode</li><li>countryName</li><li>regionName</li><li>cityName</li><li>isp</li><li>domain</li><li>usageType</li><li>asn</li><li>as</li><li>lastSeen</li><li>threat</li><li>proxyType</li><li>isProxy</li><li>provider</li><ul>|
|getCredit|This method returns the web service credit balance in a WSResult record.|

## Usage

```haskell
import IP2ProxyWebService
import Data.Maybe

main :: IO ()
main = do
    let apikey = "YOUR_API_KEY"
    let apipackage = "PX11"
    let usessl = True
    let ip = "37.252.228.50"
    wsconfig <- openWS apikey apipackage usessl
    result <- lookUp wsconfig ip
    putStrLn $ "response: " ++ (response result)
    putStrLn $ "countryCode: " ++ (fromMaybe ("-") $ (countryCode result))
    putStrLn $ "countryName: " ++ (fromMaybe ("-") $ (countryName result))
    putStrLn $ "regionName: " ++ (fromMaybe ("-") $ (regionName result))
    putStrLn $ "cityName: " ++ (fromMaybe ("-") $ (cityName result))
    putStrLn $ "isp: " ++ (fromMaybe ("-") $ (isp result))
    putStrLn $ "domain: " ++ (fromMaybe ("-") $ (domain result))
    putStrLn $ "usageType: " ++ (fromMaybe ("-") $ (usageType result))
    putStrLn $ "asn: " ++ (fromMaybe ("-") $ (asn result))
    putStrLn $ "as: " ++ (fromMaybe ("-") $ (as result))
    putStrLn $ "lastSeen: " ++ (fromMaybe ("-") $ (lastSeen result))
    putStrLn $ "proxyType: " ++ (fromMaybe ("-") $ (proxyType result))
    putStrLn $ "threat: " ++ (fromMaybe ("-") $ (threat result))
    putStrLn $ "isProxy: " ++ (fromMaybe ("-") $ (isProxy result))
    putStrLn $ "provider: " ++ (fromMaybe ("-") $ (provider result))
    result <- getCredit wsconfig
    putStrLn $ "Credit Balance: " ++ (response result)
```