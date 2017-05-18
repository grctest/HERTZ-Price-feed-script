#!/usr/bin/env runhaskell

{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

-- | Importing qualified packages
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Text as T


-- | Importing further Haskell packages
import Control.Applicative ((<$>))
import Data.Aeson -- You need to install this (cabal install aeson)
import Data.Aeson.Types
import Data.Fixed
import Data.List
import Data.Maybe
import GHC.Generics
import Turtle -- You need to install this (cabal install turtle)


-- | Global variables
-- Change these filepath values at your discretion
blockNumbers = [16621272..16900000] --We want to calculate the wave data for the next few million blocks
genesisBlock = 16621271
blocksInPeriod = 876000
backingAssetValue = 27.0 --Scrape directly from (bitUSD|ALTCAP.XDR) pricefeed.
amplitude = 0.5 --Varying the reference asset by 50% - Open to suggestion for alternative values.

jsonFile :: Prelude.FilePath
jsonFile = "XDR.json"

outputFile :: Prelude.FilePath
outputFile = "Output.csv"

-- | Configuring Aeson to handle the getpeerinfo.json fields
data XDR_JSON_DATA = XDR_JSON_DATA { xdrval :: Float } deriving (Show, Generic, Eq)

instance ToJSON XDR_JSON_DATA where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON XDR_JSON_DATA

-- | JSON data in, list out
jsonToList :: [XDR_JSON_DATA] -> [Float]
jsonToList [] = []
jsonToList (x:xs) = do
    [xdrval x] ++ jsonToList xs

-- | Write to CSV
outputToCSV :: [Float] -> [Float] -> [Float] -> Float -> IO ExitCode
outputToCSV (x:xs) (y:ys) (z:zs) referenceAssetValue =
    if (xs == [])
        then do
          shell (T.pack ("echo " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " " ++ (show referenceAssetValue) ++ " >> " ++ outputFile)) Turtle.empty
        else do
          shell (T.pack ("echo " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " " ++ (show referenceAssetValue) ++ " >> " ++ outputFile)) Turtle.empty
          outputToCSV xs ys zs referenceAssetValue

main :: IO ExitCode
main = do
    shell "wget https://cryptofresh.com/api/asset/price_feeds?asset=ALTCAP.XDR" Turtle.empty
    shell "mv price_feeds?asset=ALTCAP.XDR XDR_init.json" Turtle.empty
    shell "jq '[. | {xdrval: .[]}]' XDR_init.json > XDR.json" Turtle.empty

    -- | Extract historical feed data from cryptofresh API
    -- Sums the latest 10 published feeds and takes an average (potentially change to adapt to rapid fluctuations?)
    xsrJSON <- decode <$> (DBL.readFile jsonFile) :: IO (Maybe [XDR_JSON_DATA])
    let xdrList = (jsonToList (fromJust xsrJSON))
    let xdrRecentValue = (sum(take 10 xdrList))/10.0

    -- | List comprehension expressions
    let blocksSinceGenesis = [prevVal - genesisBlock | prevVal <- blockNumbers] -- Difference between the genesis block and the current block
    let xPeriod = [x/blocksInPeriod | x <- blocksSinceGenesis] -- The value we want to feed into the sin equation
    let priceFeedValue = [(xdrRecentValue + (amplitude * sin((xPVal) * (2 * pi)))) | xPVal <- xPeriod] -- Calculating the price feed variation

    shell (T.pack ("echo -n 'BlockNumbers XPeriodValue PriceFeedValue OriginalAssetValue\n' > " ++ outputFile)) Turtle.empty
    outputToCSV blockNumbers xPeriod priceFeedValue xdrRecentValue