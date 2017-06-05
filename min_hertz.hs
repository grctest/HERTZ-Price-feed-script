#!/usr/bin/env runhaskell
-- | Enabling the script to be run from the command line

-- | Importing further Haskell packages!
import Data.Fixed
import System.Environment

-- | Arguments:
-- 1. referenceBlock 
--      The block we want to use as a time reference prior to publishing an updated price feed.
-- 2. genesisBlock
--      The block we want to reference for a beginning timestamp.
-- 3. blocksInPeriod
--      Blocks per 'oscillation' period (Frequency)
--      Examples:
--       1 block = Approx 3 seconds
--       1 hour = 1200
--       1 day = 28800
--       1 week = 201600
--       1 month = 876000
--       12 months = 10512000
-- 4. amplitude
--      Example: 0.5 = Varies reference asset by 50% 
-- 5. referenceAssetValue
--     The BTS value of the reference backing asset
--      Example: 25

-- Example usage: ./min_hertz.hs 500 1 100 0.5 25

-- | Main function
main = do
    -- | Getting the argument data from command line
    -- Arguments are loaded in as strings in a list ["1", "2", "3", "4", "5"]
    args <- getArgs

    -- | Reading the arguments from the 'args' list
    -- Uses floats because the calculation was complaining, deal with it.
    let referenceBlock = read (args !! 0) :: Float 
    let genesisBlock = read (args !! 1) :: Float
    let blocksInPeriod = read (args !! 2) :: Float
    let amplitude = read (args !! 3) :: Float
    let referenceAssetValue = read (args !! 4) :: Float

    -- | Processing the argument values
    let blocksSinceGenesis = referenceBlock - genesisBlock -- Difference between the genesis block and the current block
    let timePeriod = (mod' (blocksSinceGenesis/blocksInPeriod) 1.00) * blocksInPeriod -- The value we want to feed into the sin equation]
    let priceFeedValue = referenceAssetValue + ((referenceAssetValue*amplitude)*(sin(timePeriod * ((2*pi)/blocksInPeriod)) )) -- Calculating the price feed variation

    putStrLn (show priceFeedValue)