module UsefulFunctions19 (
          replaceE
        , replaceColon
        , replaceDit
   --     , getCurrentTime
        , zufallsBasic1
        , getInts
 --       , time
        , unto -- iterator
        , takerleiN
        , concatM
        , evallis
        , mymonaD
        , desper
        , add
  -- 'Punkt' functions 
        , allAccU
        , publishPunktRAW
        , checkflowU
        , maybePu
        , maybePu2 
        , basisRAW
    --    , nACC
    --    , nACCRAW
        , fnACCRAW2
        , fhackbart 
    --    , innerAccess
    --    , innerAccessRAW
        , tk -- ==ausw
        , checkFo
        , evalToWrite
        , evalToCounter -- as above but breaks at 10
        , takeMY -- 'lay it out to pay it out'
        , charFilterInt -- filter Ints out of a Char, just 1 digit
        , time
        , getCurrentTime
        , diffDays 
        , timE
        , formelNormWahr -- statistical destribution to calc metric for ptc plot in svg
        , form2 -- based on this  
        , zui -- 
        , minkowskiAdd --(lister2 KAAnsatz19) a list selector to buid a tensor
        , minkowskiAdd2 -- apply a list of functions to the outcome of above 
        , maxMy
        , lengthXY
        , lengthXY2
         ) where
     --   , nameACCESSFUNCTION ) where
-- always color sceME 'delek'

import Data.List
import Data.Char
import Control.Monad
import System.Environment
import System.IO
import Data.Time as T
--import System.Locale
import System.Random

--own modules
import DataTypePunkt


replaceE = map (\c -> if c=='e' then '1'; else c)
replaceColon = map (\c -> if c==',' then ' '; else c)
replaceDit dit dat = map (\c -> if c== dit then dat; else c)


add :: [String] -> IO ()  
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")  
-- source for rndlist 
zufallsBasic1 t a x = (take t  (randomRs (1,a) (mkStdGen x)))::[Int]

rndLength1 = zufallsBasic1 3 1000000000 10
easyfoNowRun1 wieviele = zufallsBasic1 wieviele 5000 10
--rndList:[String] e.g ["1","55","4","34566543"] any N number which is I

-- foTime: IO must read Time
-- soMany : Int; length of outputList :soMany lines
-- digits: Int ; lenght og each line 
getInts foTime soMany digits = let prepStringtoInt = (map chr (filter (<58)(filter (>47)(map ord (show foTime)))))
                               in let plugRandom wieviele = zufallsBasic1 wieviele (read prepStringtoInt) digits
                               in (show (plugRandom soMany))
 
time :: IO ()
time = do
  myTime <- getCurrentTime
  putStrLn $ "It's currently: " ++ show myTime

  let fiveHours = 5 * 60 * 60
  let later = fiveHours `addUTCTime` myTime
  putStrLn $ "Five hours later it will be: " ++ show later

  let format = formatTime T.defaultTimeLocale "%Y/%m/%e %l:%M %P"
  putStrLn $ "Formatted, that is: " ++ format later

  let christmasDay = T.fromGregorian 2013 12 25
      n = christmasDay `diffDays` utctDay myTime
  putStrLn $ "Only " ++ show n ++ " days to go until Christmas!"

timE = do
  
  myTime <- getCurrentTime
  putStrLn $ "point in time " ++ show myTime
 
-----------------------------------
-- x = how man 2 take
-- y = which function 2 iterate
-- z = of which value y(z) 
unto x y z = take x ( iterate y z)
-----------------------------------




------------------------------------------------------------------
--fo MYWHIZ a functio that flattens lists sort of concat
--it finds out how many concat do to by itself
concatM x= let a = map ord x
           in let b = filter (==91) a
           in let c = filter (==93) a
           in let mer = let ant l k =  l \\ k
                        in let be  = ant a b
                        in let ce = ant be c
                        in ce 
              
           in let back = map chr mer
           in back
----------------------------------------------




----------------------------------------------
-- the same that filters '(' ')' can be used 
-- to unzip IF contents of (a , b) are the same typ 
--____________________________________________
unPair x = let a = map ord x
           in let b = filter (==40) a
           in let c = filter (==41) a
           in let mer = let ant l k =  l \\ k
                        in let be  = ant a b
                        in let ce = ant be c
                        in ce
           in let back =  mer
           in back
----------------------------------------------

----------------------------------------------------------
--statistical destribution -- 
-- zui)++ "  Ist das Vorkommen der Zahlen im Spektrum von min nach Max")
zui dipfa= map length (group (concat (sort (concat dipfa))))

maxMy val crit dipfa = (maximum (dropWhile (\(val) -> val < crit ) ( dipfa)))
minMy val crit dipfa = (minimum (dropWhile (\(val) -> val < crit ) ( dipfa)))
-- to build a tensor we connect to a list generator
-- dipfa: [String] ; 
-- *C..> minkowskiAdd  1 "1" ["","2","1"]
--   [200,199,..100]
-- *C..> minkowskiAdd  1 "1" ["","2","0.1"]
--   [200,199,..100,99..10]
-- *C..> minkowskiAdd  1 "1" ["","2","0.01"]
--   [200,199,..100,99..10,9..1]
-- *C..> minkowskiAdd  1 "1" ["","2","0.001"]
--   [200,199,..100,99..10,9..1,0]  -- equals
-- *C..> minkowskiAdd  1 "1" ["","2","0.0"]
--   [200,199,..100,99..10,9..,1,0]


minkowskiAdd val crit dipfa = let a=  maxMy val crit dipfa
                    in let b = (read a)
                    in let a2 = (read (minMy val crit dipfa))
                    in let c = ((b)-0.01)
                    in let d = [(b),(c)..(a2)]
                  --  in let e = drop 4 (take 5 d)
                    in let f = map round (map (*100) d)
                    in f
---------------------------------------------------------------------- 
-- To be plugged in another function that has an own metric or (maps a list)
--  in this used in  ??? fofina2 in Col*> or experiment3RAW11 in Exp*3> ????????????????????????????
-- -- prepare data to do (head$head $nub$foptc 10) 
           --  by (head$head $nub$foptc val)  
--
--TASK -  in 'Experiment3.hs' :1623 fofina2 write trianlges with ptc values to forBrad metric
--      using minkowskiAdd2 apply list of functions to outcome of minkowskiAdd
--      'Colored..Counter' fofina2 writes svg file and triangles of Bradley transformation
--
--
--      foMAx: String e.g "2.8" -> gauss ceiling ->  "2.9" 
--      => for each line of x or y run this function seperatly and not as pairs
--         and find the corresponding value x or y in a given A 
--         we will run an 'elemIndices' to find a B of A  with foptc+0.1 as limit
--         A: a minkowskiAdd list ([Int]) that represents A stigmatic 
--            and not stigmatic 
--         B: the first two digits of an occouring max x or y value a metric determined
--            by the ptc functions 
--            via e.g Col*>  ((nub$ptc6 10)) -- 
--               .... -- => [[x,y,z]] a list of lists 
--                          x y z coordinate points
--
--atom: Double e.g 22.28 any value x or y of any list of pairs with varieng length 
--e.g Col*> minkowskiAdd2  10.0 "1" ["2.9","2.8","0.01"] "2.81" (((ptc6 ))) 1 1  ------------UNDER DEVELOPMENT 06-10-2020
-- wrap selected value of ptc values in [[]] 
-- phiMAX: if 1 then computbale fewer errors    
minkowskiAdd2 val crit dipfa foMax foptc atom line = 
                    -- how do we read ptc , set to phi max find maxima of reduction
                    let phimax = nub$map maximum$ (foptc 10)
                    in let a=  maxMy val crit dipfa
                    in let b = (read a)
                    in let a2 = (read (minMy val crit dipfa))
                    in let c = ((b)-0.01)
                    in let d = [(b),(c)..(a2)]
                  --  in let e = drop 4 (take 5 d)
                    in let f = map round (map (*100) d)
           --  changed 'rekenNorm' to function phi max 07-10-2020 as in the main algorythm of kArmTrack5
           --  changed back to rekenNorm2  
                    in let rekenMe = ceiling(realToFrac (head$drop (atom-1)$ take atom $head$drop (line-1) $ take line$nub$foptc val)*100) -- just get same ptc values to claculate with
                    in let rekenNorm = floor(realToFrac (head$drop (atom-1)$ take atom $phimax)*100) -- floor; help find value in mink add
                    in let rekenNorm2 = floor(realToFrac (head$drop (atom-1)$ take atom $head$drop (line-1) $ take line$nub$foptc val)*100) -- floor; help find value in mink add

                    in let makEven = ((floor(realToFrac (head$drop (atom-1)$ take atom $head$drop (line-1) $ take line$nub$foptc val)*1)) * 100) 
                    in let anchorInt =   (minkowskiAdd 1 "1" ["0.0",foMax,"0.01"]) 
                    in let anchor = map show (minkowskiAdd 1 "1" ["0.0",foMax,"0.01"]) -- map show (f)
                    in do 
                    layerNO <- forM [1] (\ly -> do 
                      --  let plugCol = colorList ly 
                        aMonada <- forM ([1..(length anchor)]) (\os -> do -- ..(length anchor)] (\os -> do 
                            let conLongituda =  (tk os anchor)
                            let readMore = if length anchor == 1 then 1 
                                          else (length conLongituda)
                            innRead <- forM [1] (\cs -> do -- ..(length conLongituda)] (\cs -> do
                   -- feed minkowskiAdd to various functions 
                   --        val = 1 not used , crit "1" must be contained in dipfa otherwise error
                   --        dipfa ; the min and the max value of a given 2d field 
                                 let goONot = tk os (map show (minkowskiAdd 1 "1" ["0.0",foMax,"0.01"]))
                                 let gtFst = goONot --head$tk cs$words$show ( (conLongituda))
                          --   let gtSnd = show$snd$head$tk cs conLongituda
                            -- let inPlug = el gtFst gtSnd (unwords( map fst$dotSize ly)) (unwords(map snd$dotSize ly)) (concat plugCol)                    
                                 return (gtFst)) --(inPlug))
                            return(innRead))
    -- should be set to 200 or move whole field
                        let findIndex = (unlines$concat aMonada)

                        let zooSvg = unwords$concat$aMonada -- (mapField ly)++unwords(concat( concat aMonada))++(("</g>\n</g>\n"))
                        return(zooSvg))
                    return layerNO
                    let aSpectrum  = let toCalc = (take line (show rekenNorm)) -- rekenNorm
                                     in let findCalc = (read toCalc) `elemIndices` anchorInt 
                                     in show (head findCalc) --[layerNO] -- findCalc
                                         --in let abs2 = (rekenMe-((head$head$foptc 10)*100))
                                         --let run = number / 
                                        -- in abs1 -- if abs1 < ((head$head$foptc 10)*100) then  abs1
                                           -- else abs2

                   -- putStrLn (unwords(layerNO))
           --         putStrLn (show rekenMe) 
             --       putStrLn (show rekenNorm)
               --     putStrLn (show makEven)
                 --   putStrLn (show(aSpectrum)) --(unwords(concat aSpectrum))
       -- => export: max limit A , normal level B, first two digits B, location B in given minkowskiAdd 
                    [show rekenMe,show rekenNorm2,show makEven,aSpectrum]


     where
    -- dar is so =  tk is so;
   --  colorList c = dar c ["lightblue","lime","blue","darkgray","black","white","lime"];
   --  dotSize d = dar d [("5","7"),("10","12"),("5","7")];
     fieldPosi fp = (" <g transform=\"translate(0,"++ fp ++")\">\n<g>\n");
     mapField d = tk d $ map fieldPosi ["0","200"];


-----------------------------------------------------------------------------------------------
-- CALCULATE: get METRIC: e.g 'foBrad' ((Colored..Counter20.hs') 
-- select line with  'rowMumber': Int ; export the foBrad coordinates
-- then 
-- add  given vector 'aPunkt' : Int ; via fst or snd select x or y 
--                                    the vector naturally must be within 
--                                       boundaries:
--                                         0..max x of ptc
--                                       and
--                                         0..max y of ptc
--      in example below set to zero so the result will
--      be the corrosponding axes x and y 
--      thus    ... fst ... => x + distance 'aPunkt' -> if 'aPunkt' == 0 then x-axis 
--                            else distance of parallel over x-axis 
--              ... snd ... => y + distance 'aPunkt' -> if 'aPunkt' == 0 then y-axis
--                           else distance of parallel over y-axis
  -- extend x or y  by 0.01 for plot boundary X Y
  --    foptc: function a; apply any ptc function 
  --    forBrad:  [[(Double,Double)]] coordinate points used in metric for svg Experiment3
   --   fstOsnd : function ; by fst do for X by snd do for Y of ONE pair of  coordinates
  --    foXorY :  a coordinate value x or y of a ptc function
  --      e.g   "26.470588235294116"
  --    forMinkAdd : needs 'formatted data'
  --      e.g  "2.647"	
  -- -------------------------------------------------------------
  -- e.g Colored...*> (lengthXY (ptc6 )  [foBrad] 1 0 snd  "26.470588235294116" "2.647"	1 1)
  -- --------------------------------------------------------------
  -- *> [99.0]
  -- -- ------------------------------------------------------------- 
  --   => bradX: the X OR Y coordinates of a selected row 
  --             which is the metric for an svg plot
  --   => toSvgInt: Enter a reasonable value ( 0..999.99)
  --              transforms value into bradX; a x-or-y-metric 
  --              by log n * (x or y) + coordinate point on screen
  --   => in example above 'aPunkt' is 'n' = 0 which leads
  --             with 'rowNumber' = 1 (of 5 possible rows of 'foBradly') 
  --             to y (via 'snd') at coordinate 0 
  --   => of a vector : 'aPunkt' = 0   
  --             transformed into 'toSvgInt' coordinate = 0 
  --             to be plotted in svg  
lengthXY foptc forBrad rowNumber aPunkt fstOsnd foXorY forMinkAdd atom line = let foBound = foXorY --"26.470588235294116" -- map maximum (transpose(foptc 10)) -- max X coordinate
-- => head [22.77992277992278,26.470588235294116,25.233644859813086]
      -- a minkowskiAdd2 list 
                  in let minkActioRaw = ((minkowskiAdd2  8 "1" ["1.9","1.8","0.01"] forMinkAdd (foptc ) atom line ))
                  in let ofMinkList = (minkowskiAdd  8 "1" ["1.9",forMinkAdd,"0.01"] )
                  in let findCoordinate = let rawform =  ((map scanChar(show (last minkActioRaw)))) -- veryfies maximum is first
                                          in let stepke = (filter (>=0) (rawform ) )
                                          in let myLog = [1,10,100,1000,10000,100000,1000000]
                                          in let tobuild = reverse(take (length stepke) myLog)
                                          in let to10erSystem = sum(zipWith (*) tobuild stepke)
                                          in to10erSystem
        -- outcome 'ofMinkList' or finds location of coordinate smaller than  (max X) or smaller (max Y)  
                  in let minkTake = drop (findCoordinate-1) (take (findCoordinate) ofMinkList)
                  in let byThisMetricRAW = ((realToFrac(head ofMinkList)) / 6) -- 228 <- 2278 <- 22.779 <- maxX of (ptc6 10)
                  in let byThisMetric = ceiling byThisMetricRAW
                  in let foFindPunkt = floor byThisMetricRAW
            -- get x's out of foBrad per line 
                  in let ofBradLine  =   ( tk rowNumber (concat((forBrad)))) -- [(190,35),(270,66),(216,46),(245,55),(298,75),(322,83)]
                  
                  in let bradX =  ( (map fstOsnd ofBradLine)) --[190,270,216,245,298,322]
                  in let goBrad p= drop (p-1) (take p bradX)
                  in let ratio = (last bradX) - (head bradX) -- 132
                  in let calcRat = ((realToFrac ratio) / 100)  -- 1.32
   -----------------------------------------------------------------------------------
   -- keep track of which part of minkowskiAdd list is worked on
   --  not used yet !!!!!!!!!!!!!! 
                  in let getOfRatRAW k p = drop (k) (take p ofMinkList)
                  in let getOfRat = getOfRatRAW (foFindPunkt*(rowNumber-1)) (foFindPunkt*rowNumber)  -- pool every line of minkowskiAdd that has been changed
                  in let seeRat = head getOfRat 
                  in let seeEnd = last getOfRat 
                  in let calcMinkListRat = ((realToFrac (seeRat - seeEnd)) /100)   
                  in let bradY = (map snd ofBradLine)
   ------------------------------------------------------------------------------------
   -- determine how to add ptc data to the foBrad metric
   --  with '+' will add below x axis with '-' vice versa
   --  also needs the right atom to start plotting ptc (0,0) -> (foBrad 148,148)  (line 1 , atom 6)
                 -- in let toSvgInt k = (aPunkt) + (realToFrac (head (drop(k-1)(take k bradX))))
                  in let toSvgInt k = (-1)*(aPunkt) + (realToFrac (head (drop(k-1)(take k bradX))))

                  in [(toSvgInt atom)] --,ratio,calcRat,seeRat,seeEnd,calcMinkListRat]
                {-    do 
                     let go = take findCoordinate [1,2,3]--minkIntStep --byThisMetric
                     putStrLn "yo"
                     putStrLn  (show (ofBradLine))
                     putStrLn (show bradX) 
                     putStrLn (show ratio)
                     putStrLn (show calcRat) -- used: any value * calcRat = coordinate in svg 
                     putStrLn (show toSvgInt) 
                     putStrLn (show foFindPunkt) -- track minkowskiAdd list here not being used yet
                     putStrLn (show getOfRat) 
                     putStrLn (show seeRat)
                     putStrLn (show seeEnd)
                     putStrLn (show calcMinkListRat)-}
lengthXY2 foptc forBrad rowNumber aPunkt fstOsnd foXorY forMinkAdd atom line = let foBound = foXorY --"26.470588235294116" -- map maximum (transpose(foptc 10)) -- max X coordinate
-- => head [22.77992277992278,26.470588235294116,25.233644859813086]
      -- a minkowskiAdd2 list 
                  in let minkActioRaw = ((minkowskiAdd2  8 "1" ["1.9","1.8","0.01"] forMinkAdd (foptc ) atom line ))
                  in let ofMinkList = (minkowskiAdd  8 "1" ["1.9",forMinkAdd,"0.01"] )
                  in let findCoordinate = let rawform =  ((map scanChar(show (last minkActioRaw)))) -- veryfies maximum is first
                                          in let stepke = (filter (>=0) (rawform ) )
                                          in let myLog = [1,10,100,1000,10000,100000,1000000]
                                          in let tobuild = reverse(take (length stepke) myLog)
                                          in let to10erSystem = sum(zipWith (*) tobuild stepke)
                                          in to10erSystem
        -- outcome 'ofMinkList' or finds location of coordinate smaller than  (max X) or smaller (max Y)  
                  in let minkTake = drop (findCoordinate-1) (take (findCoordinate) ofMinkList)
                  in let byThisMetricRAW = ((realToFrac(head ofMinkList)) / 6) -- 228 <- 2278 <- 22.779 <- maxX of (ptc6 10)
                  in let byThisMetric = ceiling byThisMetricRAW
                  in let foFindPunkt = floor byThisMetricRAW
            -- get x's out of foBrad per line 
                  in let ofBradLine  =   ( tk rowNumber (concat((forBrad)))) -- [(190,35),(270,66),(216,46),(245,55),(298,75),(322,83)]
                  
                  in let bradX =  ( (map fstOsnd ofBradLine)) --[190,270,216,245,298,322]
                  in let goBrad p= drop (p-1) (take p bradX)
                  in let ratio = (last bradX) - (head bradX) -- 132
                  in let calcRat = ((realToFrac ratio) / 100)  -- 1.32
   -----------------------------------------------------------------------------------
   -- keep track of which part of minkowskiAdd list is worked on
   --  not used yet !!!!!!!!!!!!!! 
                  in let getOfRatRAW k p = drop (k) (take p ofMinkList)
                  in let getOfRat = getOfRatRAW (foFindPunkt*(rowNumber-1)) (foFindPunkt*rowNumber)  -- pool every line of minkowskiAdd that has been changed
                  in let seeRat = head getOfRat 
                  in let seeEnd = last getOfRat 
                  in let calcMinkListRat = ((realToFrac (seeRat - seeEnd)) /100)   
                  in let bradY = (map snd ofBradLine)
   ------------------------------------------------------------------------------------
   -- determine how to add ptc data to the foBrad metric
   --  with '+' will add below x axis with '-' vice versa
   --  also needs the right atom to start plotting ptc (0,0) -> (foBrad 148,148)  (line 1 , atom 6)
                 -- in let toSvgInt k = (aPunkt) + (realToFrac (head (drop(k-1)(take k bradX))))
                  in let toSvgInt k = (aPunkt) + (realToFrac (head (drop(k-1)(take k bradX))))

                  in [(toSvgInt atom)]
-- Formel zurBerechnung der proz Wahrscheinlichkeit
-- laesst Raum fuer offene Proznte, d,h, die liste der 
-- Normwahrscheinlichkeit wird immer unter 100% bleiben 
-- problem k	leine Wahrscheinlichkeiten unter 1% werden 
-- nichtmehr erfasst; 
-- -> begrenzt den Zufalls generator , schraenkt die bandbreite
-- d.h das Spektrum der simulierten vals ein 
-- eine loesung: der io string wird  multipliziert
form t c z = (100/(t+(2*c)))* z

-- Formel fuer NORMALE Wahrscheinlichkeit
-- wird getested um Verhalten im Memory2 in hinblick auf 
-- bessere simulierte vals
-- -> gibt volles Spektrum der simul val an volle Bandbreite
form2 t c z = 100/(t* (c/c)*(1/z))

-- set to form2 : full bandwidth of prior data set
-- *C..> formelNormWahr  25 999999999 "" [["20.00"],["10.00"],["20.00"],["30.00"],["00.01"]]
-- "100.0" -- verify all selected values for the tensor add up to the desired percentage
formelNormWahr dr val crit dipfa = let a1 = let dieZeut = (sum (zui dipfa))
                                            in show dieZeut 
                                   in let a2 = show (length (minkowskiAdd val crit (concat dipfa)))
                                   in let b1 i ii iii = show (form2 i ii iii)
                        --  in let b2 = b1 (a1 a2 (head a3)) 
                          
                                   in a2 --b1 (read a1) (read a2) dr --[[a1],[a2],a3] 
   
----------------------------------------------
--___________________________________________
-- take x lists of the length y of a [Int]
-- can process flattend lists
--
takeMY x y z = (mits y) (hits z)
  where
   mits f = (take f);
   hits = repeat.(mits x)   
        --   in let wider = map b l --l: liste der laenge l von
        --
 
 
-------------------------------------------
--
--ALLTIME FAVORITE
--Nimmt zeile aus String
--x: Int ; y:String
takerleiN x y = drop (x -1)(take x (lines y))
tk x y =  concat$ drop (x -1)(take x y)

-- a monad in steno
fhackbart some = do(return (do[([some])])) 

reaDin pathNo = do 
    thisfi <- readFile "HtmlS/dininWalAllPath.txt"
    let inse =  (mymonaD thisfi 9)
    putStrLn (show inse)
    let oteval = evallis (pathNo) thisfi
    let foproccfi zu = fst (break (=='"') zu)
    let proccfi = takerleiN pathNo (thisfi) 
    putStrLn (unwords(concat oteval)++"\n\n" )
    putStrLn ((unlines proccfi)) 
--------------------------------------------------------------------------- 

---------------------------------------------------------------------------
----- SimilaritYwWiter20 12-6-20
-- Punkt functions
--allAccU p = show(checkflow [] [p])           
  --allAcc   
allAccU foPun =  (checkflowU [] [(foPun)])

-- transform any PUNKT conctruction into a string
-- Punkt a -> String a -> show a 
--architect: Punkt ; a whole Overview of the DataType tree with all its Branches
-- searchTrail: ACCESFUNCTIONS e.g. [mother,mother,father] 
publishPunktRAW architect searchTrail = let firstAccess = architect searchTrail 
               in let ste1 = map ord (show firstAccess)
               in let breakCriteria = (32 `elemIndices` ste1)
     -- leersteln komme nur vor wenn ein term wie "Just blue" appears 
     -- this in order ot get rid of the Just
               in let findeLeerstelln = snd (break (==32) ste1)
               in let seeIfBreaksNeeded = if (length breakCriteria<=0) then ste1
                                          else findeLeerstelln 
               in let ste2 = filter (/=34) seeIfBreaksNeeded
               in  let mapToChar = map chr ste2
               in mapToChar


--see the different states of flowState above
-- e.g checkflow [mother] (flowState fotrack3 head) -> the optimized OUTPUT row ala [0.52,0.52,0.59,0.52,0.59,0.59,0.59]
checkflowU lis f = let ste1 lis f= publishPunktRAW f lis
                  in let ste2 = map (ste1 lis) f
                  in ste2
-- let basis mm foA = Punkt (fnACCRAW(nACCRAW (unwords(allAcc connectWrist)) ["When set M will work:"++" now sleeping", checkFo mm ] ) ) foA foA foA foA foA


maybePu rt = Punkt rt Nothing Nothing Nothing Nothing Nothing
 -- Punkt function that inherits ancestory
 -- *> type: maybePu:: String -> Maybe Punkt -> Punkt
maybePu2 rt koA = Punkt rt koA Nothing Nothing Nothing Nothing

 -- let basis mm foA = Punkt (fnACCRAW(nACCRAW (unwords(allAcc connectWrist)) ["When set M will work:"++" now sleeping", checkFo mm ] ) ) foA foA foA foA foA

basisRAW f n co ch mm foA = Punkt (f( n (unwords(allAccU ch)) ["When set M will work:"++" now sleeping", ch mm ]  )) foA foA foA foA foA
 
    --putStrLn "Wrote Punkt: startWert"
-- this builds our sheep family tree
-- HYRACHY IN DATA-TYPE
--          |ACCESS FUNCTIONS  
-- ___________|___________________________________________________________________________|
-- LEVEL 0   :|    nACC                                                                   |
-- LEVEL 1 in:|    nACCRAW -> fst -> tracker1                                             |
--            |            -> snd -> findArm,findHand                                     |
-- LEVEL 1    |     tracker1        |    findArm,findHand                                 |   
-- LEVEL 1 output =>fst: "tracker!" OR    snd:  ["findArm","hand"])                       |                      
-- LEVEL 2    |                     |    innerAccessRAW "1" this OR inner..RAW "2" this   |
-- LEVEL 2    |   e.g 
-- -- as INNERACCESS function retieving data of each single ACCESSFUNCTIONS          
-- within the string of a Punkt
-- e.g: *Main> Co.innerAccessRAW "2" (unwords ["findArm","hand\n"])
--      " hand\n"

innerAccessRAW on this = let convToBit = map ord this
                   in let chopUp1 = (break (<=32) convToBit)
                   in let chopUp2 = (break (>=32) convToBit)

                   in if on=="1" then 
                        --let theFst = fst (break (>32) ((fst chopUp1)))	
                        map chr (fst chopUp1)
                      else
                        (map chr (snd chopUp1))

innerAccess on this = (innerAccessRAW on) this
----------------------------------------------------------------------
-----------------------------------------------------------------------
--Fill AccesFuctions with Data
-- theName: String; Fill AccesFuctions with Data
-- set to shwo tracker1 see below
nameACCESSFUNCTION on theName input= let whichAccesFunction = (innerAccess on theName)
                            in let theData = whichAccesFunction++" "++(unlines input)
                            in theData


--short
nACC theName input = (nameACCESSFUNCTION "1" theName input)
nACCRAW tracker1 input = break (==' ') (nACC tracker1 input)
--nACCTAG =  

fnACCRAW2 foCon cou = if unPoint == ("\"notM\"") then fst cou
                        else snd cou
          where 
             unPoint = (show(head(words(unwords(checkflowU [] foCon ))))) ;

checkFo g = if (length g) == 0 then ""
            else "MOTHER MODE: on" 


-- prog data arcitectures with Punkt
 

-----------------------------------------------------------------------------



----------------------------------------------------------------------------
--filtert String z nach zeichenzahl (s. dropwhile ..)
--in 'wolfram' werden leerstellen noch angezeigt (concat entfernt diese)
--in 'rythmuS' werden die stellen der valieden pfade als [Int] ausgegeben
--MOMENTAN GETRIGGERT AUF : rythmuS
--z: inputDatei String
--d: liste der laenge von input int

mymonaD z d = let wolfram = let mof z d = map (takerleiN d) (z)
                            in let mymonad z d = map (mof z) d --maped [string] ueber [Int] (select)
                            in let momonad = let fo = concat.concat
                                             in let iNN = fo (mymonad (lines z) ([1..d]))
                                             in let mYY = group (map length iNN)
                                             in let unconcat = (map show mYY) -- gegenteil von concat
                                             in let hg = (head mYY)
                                             in map (dropWhile (<500)) mYY
                            in momonad 
   
              in let rythmuS  = let prep = group (zip [1..d] ( wolfram))
                                in let dorf = map (dropWhile (\(num,lis) -> lis == [] ) ) prep -- durch map wird die ganze liste bearbeitet   
                            --(dropWhile (\(num,lis) -> lis == [] )  prep) -so ohne map   
                                in  map fst ((concat ( dorf))) 
              in ( rythmuS) -- oder rythmus oder concat wolfram             
--         putStrLn "\n\n____Wolfram:\n"          
  --    print wolfram
    --  putStrLn "\n\n____NONUMbers: flattend length of valid paths\n" 
      --print (concat wolfram) 
     -- putStrLn "\n\n____The Rythem of valid paths: flattend position of valid paths\n"
    --  print rythmuS 
   --   putStrLn "\n"         
   --
   --
   --
   --

--____________________________________________________________________________
--nimmt EINEN valieden pfad aus einer beliebigen text datei
--aus der zeile x  (valide zeilen bekannt durch mymonaD)
--getriggered mit mymonaD (<zeichenzahl)  und takerlein also 
--string wird mit Int geemapped, in evallis wird eine
-- [Int] gegen [String] gemapped
--
--x: int ->  valieder pfad  in zeile ..
--s: string -> eingelesene pfade
evallis x s =  let prqw u t =  (take u) t  
               in let vorbe = (mymonaD s x)
               in let erg = let tzu = prqw x (lines s) -- 1 variable nimmt validen 'path' string
                            in tzu 
               in let fodritte = let preb u =  prqw u erg  --waehlt von erg (alle input) nur die
                                 in  map preb vorbe -- mit vorbe werde valide pfade ausgewaehlt 
                                        -- in take a erg  
               in let nooky = let buena = take (length vorbe) ( (fodritte))
                              in ( ( (buena))) -- mymonaD (concat(concat buena)) (take a vorbe)
               in nooky  


--gibt Liste der validen pfade [string] aus
--v: [Int] -> liste der LAENGE: anzahl valide pfade
--s: String ->  der gleiche wie in mymonaD  und evallis
desper v s = let aa j u  = evallis  u j 
             in let ab j u =  map (aa j) u
             in let ac = [1..(length v)]
             in ab s ac  
--brauch och print befehl fuer IO
--
-------------------------------------------------------------------------------------------


haalTal :: String -> Int
haalTal s = groesserneun
  where  
    groesserneun = let verwerk = map ord s
                   in let a  = filter (<=57) verwerk
                   in read (map chr a)

{-
------------------------------------------------------------------------------------------------------------------
-- Funktion berechnet monats ende und anfang  in tagen
-- Ist gregorianischer Kalendar jahreslaenge 365 tage
-- wird benoetigt fuer mehrzeilige Monats uebersicht
-- dies ist die Analyse der daten BSP fuer spaetere
-- simulation      
monatllist jahrzahl laengezeit monaT = 
              let faktorM = (365 * laengezeit)	
                  jahreE = (jahrzahl/4) 
                  schjahrlistTrue = [31,29,31,30,31,30,31,31,30,31,30,31]
                  schjahrlistFalse = [31,28,31,30,31,30,31,31,30,31,30,31]
                  takese wd wquel= drop (wd-1) (take wd wquel) 
                  kesNoDrop wd wquel= (take wd wquel) 
                  
                   -- filter . test auf teiler4 -> schaltjahr?
                  schjahrtest k = let dezimalstellenOnly = let tochr = map ord (show jahreE)
                                                               breakeR = snd(partition (==46) tochr)--breaK AT '.'
                                                               breakeR2 = length breakeR
                                                               prozesPaar k g = (takese k g)
                                                           in let staun1 = if (breakeR2>1) then ((prozesPaar k schjahrlistFalse),(kesNoDrop k schjahrlistFalse)  )
                                                                           else ((prozesPaar k schjahrlistTrue), (kesNoDrop k schjahrlistTrue))
                                                                  -- nim ganze liste fuer abzaehl datum
                                                       {-    in let staun2 = if (breakeR2>1) then (kesNoDrop k schjahrlistFalse)
                                                                           else (kesNoDrop k schjahrlistTrue)-}

                                                          in (staun1)
                                  in  dezimalstellenOnly
                  einsetzenJahrMax k = ((sum(snd (schjahrtest k)))  + (365*(laengezeit-1))) 
                  einsetzenJahrDurch k = (fst (schjahrtest k))--((einsetzenJahrMax k) - (einsetzenJahrMax(k-1)))
                  einsetzenJahrMin k =  ((einsetzenJahrMax k) - ((einsetzenJahrMax k) - (einsetzenJahrMax(k-1))) )
                  selectMax x = if (laengezeit==1) then (einsetzenJahrMax x) --(fst(schjahrtest x))
                                else (einsetzenJahrMax x) --(einsetzenJahr x) -- if (fak1== ) then ( 
                  selectMin x = if (laengezeit==1) then (einsetzenJahrMin x) --(fst(schjahrtest x))
                                else (einsetzenJahrMin x)          
              in ((einsetzenJahrDurch monaT),((selectMin monaT), (selectMax monaT)) )
  -- singleLine4MonthSelector 

 -}

-- builds SVG Data like Backbone, builts SVG Animated Groups (<g>..</g>)
-- that will be inserted into activeField
--theDNA g  = do 
      
-- SUPER !! 1. gebe Wort aus <g>..</g> file ein
--          2. filtert Automatisch Koordinaten x y 
--          3. Bewegt Inhalt um 2 Felder
--            (mittles 'position1 vektor')
--          4. gibt neue Koordinaten im fertigen <g>..</g> String aus 
-- filtert das Wort P F A D aus foKoordinates
-- zeigt Stelle fuer Break Kriteria fuer Einfuegen
-- von item Koordinaten
--  EXPORTS: fertigen <g>..</g> String mit FERTIGE ANIMATION
--  token:: Int
itemReaderPfadFinder file vektor token = do
        afile <- readFile file --e.g. "GraphicEngine17/Backbone/TheGameItemCircleWORKS.txt"
        let criterium = ("1122")
        let stepper = [1..(length (lines afile))]-- findet raus wie lang input file ist	
        iteM <- forM (stepper) (\ a -> do
             let auswahL = map ord (unwords (takerleiN a afile))

             let check = foKoordinates auswahL
           
             let checkPfad = if (check)==([112,97,116,104]) then (a) -- P F A D
                             else (0)
             let hohlPfad = let as1 = [0..checkPfad]
                          --  in let as2 = zip [1..] ( as1)
                            --in let as3 = length [1..as2]
                            in (maximum( as1)) --welche Zeile MATCHED ?  
             return (hohlPfad) )
        let hohlersteZeilen = let as1 = take (maximum iteM) (lines afile) --nimm erste zeilen aus afile
                              in let as2 = tail ( reverse as1 ) -- drop last 
                              in reverse as2  
-- tail : SVG </g> drop everything before path only take the rest
        let hohlRest =  let as1 = drop (maximum iteM) (take (length stepper) (lines afile)) 
                        in as1
        let inseRRt = ("GraphicEngine17/Backbone/VektorBewegung/GamePosi"++ ( token)++".txt")
        joGas <- readFile inseRRt
        let checkAlles = (((unlines hohlersteZeilen)) ++("         "++( joGas )++"\n")++ (unlines( hohlRest)))  
        putStrLn (show iteM) --zeilen match PFAD
  --      putStrLn (show hohlersteZeilen) -- Header der <g>..</g> File
  --      putStrLn (show stepper) laenge input file
        putStrLn (show checkAlles) -- EXPORT: fertigen <g>..</g> String
        writeFile ("GraphicEngine17/Backbone/VektorBewegung/GamePosiWORKS"++ (token)++".txt") ( checkAlles)

 -- nur kleine Buchstaben; Heisst:
            -- zahlen weg
            -- leerstellen weg
            -- anfuehrungszeichen weg
-- Funktion fuer ItemReaderFinder s.u.
-- um P F A D zu filtern   
foKoordinates stream = let as2 = stream 
                       in let as6 = filter (>=97) as2
                in as6

-- turn "123" into Int 123

scanChar :: Char -> Int

scanChar c | '0' <= c && c <= '9' = fromEnum c - fromEnum '0'
           | otherwise = -1

--scanString :: String -> Int a -> Double 
scanString = let ste = (go 0) 
             in zipWith (/) [1] 
    where go a [] = a
          go a (x:xs) | 0 <= sc && sc <= 9 = go (10*a+sc) xs
                      | otherwise = 0
              where sc = scanChar x

--------------------------------------------------------------------------
-- Gaussian influenced - Aehnlichkeitswarscheinlichkeit
---this function rates the similarity of two
-- lists. 
-- gibt die Prozentzahl der Einerstelle 
-- der beiden Listen aus

foaehnli1 a b = (a-b)
foaehnli2 a b = (b-a)
--umrechnen in % mit a < b und b > a  
aehnlichF a b = let a1 = if a > b then ((foaehnli1 a b)/ (a/100) )
                         else if a<b then  ((foaehnli2 a b)/ (b/100) )
                         else 0
                in let b1 g h = ((g) / ((h)/100)) 
                in a1 

 
similaritYvalue li la = let a = la -- z.b. [11,12,34,44]
                        in let b = li 
                        in let c1 w = sum w
                        in let c2 = c1 a--length
                        in let c3 = c1 b--length
                        in  aehnlichF c2 c3 -- in let d = 


--g: is for pi
--h: wie spitz/flach ist Glocke
--x: Wert der  beobachteten Groesse e.g. val 
--innerhalb von  
fogAussVerteilung h g= (h/(sqrt g))

foefactor h x e= (((sqrt h^2)*(x^2))*e)
gaussVerTeilung h g x e = ((fogAussVerteilung h g )*(foefactor h x e))


-- the simple similarity value
sim a b =  similaritYvalue (map realToFrac (map ord a)) (map realToFrac(map ord b))
atoB a b =  (a++b++a++b++a++b++a++b++a++b)
betoA a b = (b++a++b++a++b++a++b++a++b++a)
-- write six files then start back overwrite 1 ....
-- will turn on number digit of a Sring +1
--  e.g "aname1.txt" -> "aname2.txt"
--  used in time depending random file writing in 'dit' 
--aString:Sting ; the sourceFile
--beak9 : Int , e.g 9 if pointer reaches 9 start at 1 again
evalToWriteRAW astrinG break9 = if tzBool>0 then prsRoot++(head tz3)++(show tzInt)++"."++(last tz3)
                      else prsRoot++(head tz3)++("1.")++(last tz3)
     where
    poc1 fOsnd = reverse( fOsnd(break (=='/') (reverse(astrinG))));--prevent '/' cause trouble
    prsInput = poc1 fst;
    prsRoot = poc1 snd;  
    tz0 = (map ord prsInput);
    tz = (filter (>47) (filter (<58)  tz0));
    tzExp = (map chr tz);
    tzBool = length tzExp;
    tzRootSource  = filter (==47); 
    tz1 = tz0 \\ tz;
    tz2 = map (\c -> if c=='.' then ' '; else c);
    tz3 = words (tz2 (map chr tz1));
    tzInt = if tzBool==0 then 1
            else if (read tzExp)<break9 then (read tzExp)+1
            else 1 ;


evalToWrite astrinG = evalToWriteRAW astrinG 9
evalToCounter aString = evalToWriteRAW aString 11
-- to be mapped in triggerword 'Experiment3.hs' 
-- e.g *> let cellType ct =  fst(charFilterInt(evalToWrite ct ))
-- *> take 4 (iterate cellType "cell44")
-- *> ["cell44","cell","cell","cell"]  
-- => check the type the String "cell44" and retrieve the Int out of it
charFilterInt astrinG = if tzBool>0 then ((head tz3),(tzInt))
                      else ((head tz3),0)
     where
    poc1 fOsnd = reverse( fOsnd(break (=='/') (reverse(astrinG))));--prevent '/' cause trouble
    prsInput = poc1 fst;
    prsRoot = poc1 snd;  
    tz0 = (map ord prsInput);
    tz = (filter (>47) (filter (<58)  tz0));
    tzExp = (map chr tz);
    tzBool = length tzExp;
    tzRootSource  = filter (==47); 
    tz1 = tz0 \\ tz;
    tz2 = map (\c -> if c=='.' then ' '; else c);
    tz3 = words (tz2 (map chr tz1));
    tzInt = if tzBool==0 then 1
            else if (read tzExp)<9 then (read tzExp)+0
            else 1 ;


