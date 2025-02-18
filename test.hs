import IP2Proxy

main :: IO ()
main = do
    let myfile = "./IP2PROXY-IP-PROXYTYPE-COUNTRY-REGION-CITY-ISP-DOMAIN-USAGETYPE-ASN-LASTSEEN-THREAT-RESIDENTIAL-PROVIDER-FRAUDSCORE.BIN"
    let ip = "197.85.191.64"
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
    putStrLn $ "fraud_score: " ++ (show (fraud_score result))
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
    result <- getFraudScore myfile meta ip
    putStrLn $ "fraud_score: " ++ result
    result <- isProxy myfile meta ip
    putStrLn $ "is_proxy: " ++ result
