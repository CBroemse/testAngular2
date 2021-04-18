module SearchCSV
   (   afunc2                 -- also shorten length of output file avoid stack overflow
                              -- e.g afunc2 "archive/BTC-USD.csv" 10 "Open" "High" "Low" 0 
                              -- => determine ength of output Via solong

    ,  afunc  -- afunc "archive/BTC-USD.csv" "Open"
    , checK  -- see collum names of csv 
           --   checK "archive/BTC-USD.csv" "Open" 0 or 1..
           --   
               ) where

import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO

import UsefulFunctions19

ausw r t = drop(r-1)$(take r t)
avanti e =  mapM_ putStrLn $ e


checK = (\file spot -> do
    dobe <- readFile file
    putStrLn (unwords(ausw 1 (lines dobe)))
    let legend = words$replaceColon(unlines(ausw 1 (lines dobe)))
    let spotS = (spot) `elemIndices` legend
    putStrLn (show spotS))

-- file :: String , source of file
-- spot :: String, e.g "Date" needs elemIndices 
-- mode :: Int if == 0 then select date plus a val else pick only val 
afunc = (\file spot mode -> do
    dobe <- readFile file
    putStrLn (unwords(ausw 1 (lines dobe)))
    let legend = words$replaceColon(unlines(ausw 1 (lines dobe)))
    let spotS = (head ((spot) `elemIndices` legend) ) +1 
    putStrLn (show spotS)
    let solong = length$lines dobe
    aTuck <- forM [1..solong](\fg -> do
        let unO = ausw fg (lines dobe)
       -- let spotS = [(spot)] `elemIndices` (ausw 1 (lines dobe))
        let fromCol o = ausw o (words$replaceColon(unlines unO)) 
        let fromLine = if mode == 0 then [(unwords (fromCol 1))++","++(unwords(fromCol spotS))]
                       else fromCol spotS
        return (fromLine)) 
    avanti (concat aTuck)
    writeFile "archive/btcNew.txt" (unlines(map unwords aTuck))
    putStrLn "wrote archive/btcNEW.txt")


-- due to fast production only works with 
-- spot1 ,spot2 , spot3  
-- also shorten length of output file avoid stack overflow
-- e.g afunc2 "archive/BTC-USD.csv" 10 "Open" "High" "Low" 0 
-- => determine ength of output Via solong
afunc2 = (\file solong spot1 spot2 spot3 mode -> do
    dobe <- readFile file
    putStrLn (unwords(ausw 1 (lines dobe)))
    let legend = words$replaceColon(unlines(ausw 1 (lines dobe)))
    let spotS1 = (head ((spot1) `elemIndices` legend) ) +1 
    let spotS2 = (head ((spot2) `elemIndices` legend) ) +1 
    let spotS3 = (head ((spot3) `elemIndices` legend) ) +1 
    putStrLn ((show spotS1)++(show spotS2)++(show spotS3))
    let solonG = length$lines dobe
    aTuck <- forM [1..solong](\fg -> do
        let unO = ausw fg (lines dobe)
       -- let spotS = [(spot)] `elemIndices` (ausw 1 (lines dobe))
        let fromCol o = ausw o (words$replaceColon(unlines unO)) 
        let fromLine = if mode == 0 then [(unwords (fromCol 1))++","++(unwords(fromCol spotS1))++","++(unwords(fromCol spotS2))++","++(unwords(fromCol spotS3))]
                       else fromCol spotS1
        return (fromLine)) 
    avanti (concat aTuck)
    writeFile "archive/btcNew.txt" (unlines(map unwords aTuck))
    putStrLn "wrote archive/btcNEW.txt")
