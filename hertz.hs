#!/usr/bin/env runhaskell
-- | Enabling the script to be run from the command line
-- In the future, the script will accept arguments from the CLI.

-- | Don't worry about this
{-# LANGUAGE DeriveAnyClass, DeriveGeneric, OverloadedStrings, ScopedTypeVariables #-}

-- | Importing 'qualified' packages
-- We do this because some of the names clash with other packages and 'DBL.readFile' is shorter than 'Data.ByteString.Lazy.readFile'
import qualified Data.ByteString.Lazy as DBL
import qualified Data.Text as T

-- | Importing further Haskell packages!
-- You need to install both Aeson and Turtle through Cabal (or whatever Haskell package manager you use).
-- Without the dependancies, the script will fail to run.
import Control.Applicative ((<$>))
import Data.Aeson -- You need to install this (cabal install aeson)
import Data.Aeson.Types
import Data.Fixed
import Data.List
import Data.Maybe
import GHC.Generics
import Turtle -- You need to install this (cabal install turtle)

-- | Global variables
blockNumbers = [16621272..17000000] --We want to calculate the wave data for the next few million blocks
genesisBlock = 16621271 -- The block we want to reference for a beginning timestamp, this may change prior to the asset 'going live'
blocksInPeriod = 876000 -- Blocks per 'oscillation' period (Frequency); 876000 is approx one month worth of blocks.
amplitude = 0.5 -- Varying the reference asset by 50% - Open to suggestion for alternative values, this value may change prior to the asset 'going live'

-- | Test global variables
--blockNumbers = [2..202]
--genesisBlock = 1
--blocksInPeriod = 100 -- Blocks per 'oscillation' period (Frequency); 876000 is approx one month worth of blocks.


-- Change these filepath values at your discretion
jsonFile :: Prelude.FilePath
jsonFile = "XDR.json"

-- Change these filepath values at your discretion
outputFile :: Prelude.FilePath
outputFile = "Output.csv"

-- | Configuring Aeson to handle the getpeerinfo.json fields
data XDR_JSON_DATA = XDR_JSON_DATA { xdrval :: Float } deriving (Show, Generic, Eq)

-- | Configuring Aeson to handle the getpeerinfo.json fields
instance ToJSON XDR_JSON_DATA where
    toEncoding = genericToEncoding defaultOptions

-- | Configuring Aeson to handle the getpeerinfo.json fields
instance FromJSON XDR_JSON_DATA

-- | Pass in the JSON data, output the data in a list for traversing
-- 'xdrval' is not a function but rather pointing to the 'xdrval' field in the JSON data.
jsonToList :: [XDR_JSON_DATA] -> [Float]
jsonToList [] = []
jsonToList (x:xs) = do
    [xdrval x] ++ jsonToList xs  -- Recursively call this function with the next JSON record elements.

-- | Write to CSV
-- Takes three lists in (BlockNumbers, xPeriod and priceFeedValue) as well as the recent backing asset value scraped from Cryptofresh!
outputToCSV :: [Float] -> [Float] -> [Float] -> IO ExitCode
outputToCSV (x:xs) (y:ys) (z:zs) =
    -- Check if we're on our final recursion, rather than checking for 3 empty lists.
    if (xs == [])
        then do
          -- Final recursion
          shell (T.pack ("echo " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " >> " ++ outputFile)) Turtle.empty
        else do
          -- More recursions to go!
          shell (T.pack ("echo " ++ (show x) ++ " " ++ (show y) ++ " " ++ (show z) ++ " >> " ++ outputFile)) Turtle.empty
          outputToCSV xs ys zs -- Recursively call this function with the next list elements.

-- | The main function that is called from the CLI
-- In the future, we will pass in arguments from the CLI so that global variables aren't hardcoded into the script.
-- Potential args: Backing asset, Price feed sources, the 'genesis' block for a new HERTZ FBA, Period duration, Amplitude, filenames.
--main :: IO ExitCode
main = do
    -- | Scraping cryptofresh's API For the ALTCAP.XDR asset, using the 'Turtle' package.
    -- Using jq to convert cryptofresh's JSON data into a traversable format.
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
    let timePeriod = [((mod' (x/blocksInPeriod) 1.00) * blocksInPeriod) | x <- blocksSinceGenesis] -- The value we want to feed into the sin equation]
    let priceFeedValue = [xdrRecentValue + (((xdrRecentValue*amplitude)*(sin(currentTime * ((2*pi)/blocksInPeriod) )))) | currentTime <- timePeriod] -- Calculating the price feed variation
    let waveValue = [(((amplitude)*(sin(currentTime * ((2*pi)/blocksInPeriod) )))) | currentTime <- timePeriod] -- Calculating 

    -- | Starting output to CSV
    -- We start with overwriting any past version of the CSV, and placing the headers in the first row.
    -- Then we pass the lists created above into the outputToCSV function which recursively writes the data on new lines within the same CSV file.
    shell (T.pack ("echo -n 'BlockNumber PriceFeedValue waveValue OriginalAssetValue: " ++ (show xdrRecentValue) ++ "\n' > " ++ outputFile)) Turtle.empty
    outputToCSV blockNumbers priceFeedValue waveValue