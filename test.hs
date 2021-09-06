import qualified Data.List as L (sort)
import Data.Array (Array, array, (!), elems)
import System.IO (hGetContents, openFile,hClose,IOMode(ReadMode))
import Text.Printf (printf, PrintfArg)

rangeS :: (Num a, Ord a) => [a] -> a
rangeS = (\xs -> last xs - head xs) . L.sort

intervalNumbers :: Floating a => a -> a
intervalNumbers totalElemens = 1 + (3.3 * logBase 10 totalElemens)

amplitud :: Fractional a => a -> a -> a
amplitud range intervals = range / intervals

firstOverview :: [Float] -> Array Int (Either Float [Float])
firstOverview xs =  array (0,5) 
    [ (0,Left $ maximum xs)
    , (1,Left $ minimum xs)
    , (2,Left range)
    , (3,Left intervals)
    , (4,Left ampld)
    , (5,Right $ inferiorLimits ampld (head xs) 8)
    ]
    where 
        totalElements = (read . show . length) xs
        range = rangeS xs
        intervals = (myRound . intervalNumbers) totalElements
        ampld = amplitud range intervals
        myRound :: RealFrac a => a -> Float
        myRound = read . show . round 

roundToStr :: (PrintfArg a, Floating a) => Int -> a -> String
roundToStr n f = printf (printf "%%0.%df" n) f

inferiorLimits :: Float -> Float -> Int -> [Float]
inferiorLimits amp root interv = 
    let xs = [m + amp | m <- root : xs] 
        in (take interv . map ((\x -> read x :: Float) . roundToStr 2) . (:) root) xs

main :: IO ()
main = do
    handle <- openFile "datasSecheep.txt" ReadMode
    contents <- hGetContents handle
    print $ elems $ (firstOverview . aux) contents
    hClose handle
    where 
        aux :: String -> [Float]
        aux = converToFloat . lines
        converToFloat :: [String] -> [Float]
        converToFloat = map read

