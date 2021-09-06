import Data.Maybe (isNothing, isJust)
import Data.Array (Array, array, (!), (//), bounds)
import qualified Data.List as L (foldl') 

m :: [[Int]]
m = [ [0,0,0,1,1,1,0,1,1,1]
    , [0,0,0,0,0,0,0,0,0,1]
    , [0,0,1,1,0,0,0,1,1,0]
    , [0,0,0,0,0,1,0,0,0,0]
    , [0,1,1,0,0,0,0,0,1,0]
    , [0,0,0,0,0,0,0,0,1,0] 
    , [0,1,0,0,0,0,1,0,1,0]
    , [0,0,0,0,0,0,0,0,1,0]
    , [1,0,0,0,0,0,0,0,0,0]
    , [0,0,0,0,0,0,0,0,0,0]]

data Ones a = Ones (a,a) | Zero deriving (Show, Eq) 

validate :: [[Int]] -> IO ()
validate field =
    let eitherBetweenBoolArray = checkTable $ cornersAndAloneOnes field
        correctTable = array (1,10) ((1,4):(2,3):(3,2):(4,1):[(i,0) | i <- [5..10]])
        in case eitherBetweenBoolArray of
                Left False -> mapM_ print m >> print False
                Right hashTable -> mapM_ print m >> print (hashTable == correctTable)
    where
        checkTable :: (Bool, Int) -> Either Bool (Array Int Int)
        checkTable (True,_) = Left False
        checkTable (False,amoundAloneOnes) = Right $ shipsOnRows 0 (shipsOnCols 0 hashShip)
            where
                tups :: [(Int,Int)]
                tups = (,) <$> [0..9] <*> [0..9]
                hashShip :: Array Int Int
                hashShip = array (1,10) [if i == 1 then (i,amoundAloneOnes) else (i,0) | i <- [1..10]]
                shipsOnCols :: Int -> Array Int Int -> Array Int Int
                shipsOnCols cint ht
                    | cint > 9 = ht
                    | otherwise = shipsOnCols (cint + 1) (countShips ht (filter (\(x,y) -> y == cint) tups))
                shipsOnRows :: Int -> Array Int Int -> Array Int Int
                shipsOnRows cint ht
                    | cint > 9 = ht
                    | otherwise = shipsOnRows (cint + 1) (countShips ht (filter (\(x,y) -> x == cint) tups))
        countShips :: Array Int Int -> [(Int,Int)] -> Array Int Int
        countShips hashTable [] = hashTable
        countShips hashTable (x:xs) = 
            let z = fst x
                w = snd x
            in  if m !! z !! w == 1 then
                    let (amountOnes,ys) = countOneOfShip xs 1
                        in  if amountOnes /= 1 then 
                                countShips (hashTable // [(amountOnes, hashTable ! amountOnes + 1)]) ys
                            else
                                countShips hashTable ys
                else
                    countShips hashTable xs
            where
                countOneOfShip :: [(Int,Int)] -> Int -> (Int,[(Int,Int)])
                countOneOfShip [] acc = (acc,[])
                countOneOfShip (x:xs) acc = let k = fst x
                                                q = snd x 
                                                in if m !! k !! q == 1 then countOneOfShip xs (acc + 1) else (acc,xs)
        cornersAndAloneOnes :: [[Int]] -> (Bool, Int)
        cornersAndAloneOnes matrix = resultTuple $ existsOnesInTheCorners matrix
            where 
                resultTuple :: [Ones Bool] -> (Bool, Int)
                resultTuple [] = (False,0)
                resultTuple xs = let ones = filter (/= Zero) xs
                                    in ( any (\(Ones (x,y)) -> x) ones
                                    , L.foldl' (\acc (Ones c) -> if not $ snd c then acc + 1 else acc) 0 ones
                                    )
                existsOnesInTheCorners :: [[Int]] -> [Ones Bool]
                existsOnesInTheCorners matrix = 
                    let rows = length matrix - 1
                        cols = length (head matrix) - 1
                        tups = (,) <$> [0..rows] <*> [0..cols]
                        in map ((\m (x,y) -> if m !! x !! y == 1 then Ones $ existsOnesAtPosition x y else Zero) m) tups
                maybeExistsOnesAt :: [[Int]] -> Int -> Int -> ([Maybe Int], [Maybe Int])
                maybeExistsOnesAt matrix x y =
                    let maybeOneAtPosition (x,y) =
                            if x == -1 || x == 10 || y == -1 || y == 10 then Nothing else Just $ matrix !! x !! y
                        adyacents = [(x,y-1), (x,y+1), (x-1,y), (x+1,y)]
                        corners = (,) <$> [x-1,x+1] <*> [y-1,y+1]
                        in ([maybeOneAtPosition w | w <- corners],[maybeOneAtPosition z | z <- adyacents])
                converToMaybeBool :: [Maybe Int] -> [Maybe Bool]
                converToMaybeBool = map (\fx -> if isNothing fx then Just False else (== 1) <$> fx)
                existsOnesAtPosition :: Int -> Int -> (Bool,Bool)
                existsOnesAtPosition x y = 
                    let (cors, ady) = maybeExistsOnesAt m x y
                        existsCorners = any (\(Just x) -> x) $ converToMaybeBool cors
                        existsAdyacent = any (\(Just x) -> x) $ converToMaybeBool ady
                        in (existsCorners, existsAdyacent)
