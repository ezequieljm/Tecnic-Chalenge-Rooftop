
import qualified Data.List as L (foldl',sortBy, sortOn)

beforeEncode :: String -> Int -> Maybe [(Int,Int)]
beforeEncode msgUser rails
    | rails >= 2 = Just $ take ((length . replaceWitheSpaces) msgUser) $ buildDiagonal 0 0 rails 0
    | otherwise = Nothing
    where
        buildDiagonal :: Int -> Int -> Int -> Int -> [(Int,Int)]
        buildDiagonal x y tx ix
            | x > tx = buildInvertedDiagonal (x - 2) y tx ix
            | otherwise = (x,y) : buildDiagonal (x + 1) (y + 1) tx ix
        buildInvertedDiagonal :: Int -> Int -> Int -> Int -> [(Int,Int)]
        buildInvertedDiagonal x y tx ix 
            | x < ix = buildDiagonal (x + 2) y tx ix
            | otherwise = (x,y) : buildInvertedDiagonal (x - 1) (y + 1) tx ix

replaceWitheSpaces :: String -> String
replaceWitheSpaces [] = [] 
replaceWitheSpaces (x:xs) 
    | x == '\t' || x == ' ' || x == '\n' = "" ++ replaceWitheSpaces xs
    | otherwise = x : replaceWitheSpaces xs

psdoencode :: String -> Int -> (String,String)
psdoencode msg rails = 
    let withoutSpaces = replaceWitheSpaces msg 
        in  case beforeEncode msg rails of 
                 Nothing     -> ("The Rails is less than 2","")
                 Just indexs -> ( codedMessage 0 $ zip withoutSpaces indexs
                                , withoutSpaces)
    where
        codedMessage :: Int -> [(Char,(Int,Int))] -> String
        codedMessage count [] = []
        codedMessage count allList@(x:xs) = 
            let filterByCount = filter (\(_,(s,_)) -> s == count) allList
                in L.foldl' (\acc (h,_) -> acc ++ [h] ) [] filterByCount 
                    ++ codedMessage (count + 1) xs

encode :: String -> Int -> String
encode text rails = fst $ psdoencode text rails

decode :: String -> Int -> Maybe [Char]
decode xs rails = L.foldl' (\acc (x,_) -> acc ++ [x]) "" <$> psdoDecode xs rails
    where 
        psdoDecode :: String -> Int -> Maybe [(Char,(Int,Int))] 
        psdoDecode text rails = 
            case beforeEncode text rails of
                Nothing -> Nothing
                Just xs -> L.sortOn (snd . snd) <$> Just (go 0 rails text xs)
            where 
                go :: Int -> Int -> String -> [(Int,Int)] -> [(Char,(Int,Int))]
                go countX maxRail oldText ys
                    | countX > maxRail = []
                    | otherwise = 
                        let zs = filter (\(x,y) -> x == countX) ys
                            (newText, theList) = newGo oldText zs [] 
                            in  theList ++ go (countX + 1) maxRail newText ys
                newGo txt [] accL = (txt,accL)
                newGo (x:xs) (y:ys) accL = newGo xs ys ((x,y):accL)