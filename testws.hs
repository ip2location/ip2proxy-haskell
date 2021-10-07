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
