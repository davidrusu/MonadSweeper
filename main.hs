import Control.Monad
import System.Random
import Data.List
import System.IO
import Debug.Trace

data State a = Mine | Unexplored | Clue a deriving(Show)

type Point   = (Int, Int)
type World   = [[State Int]]
type ClueMap = [[Int]]
type Explored = [[State Int]]

size   = 2
width  = 10
height = 10

main :: IO ()
main = do
  g        <- getStdGen
  explored <- return $ makeMatrix width height Unexplored -- nothing is explored
  world    <- return $ genGame width height 10 g
  runGame explored world >>= showWorld

runGame :: Explored -> World -> IO Explored
runGame e w = do
  showWorld e
  move <- getLine
  newE <- return $ explore w (parseInput move) e
  case isMineVisible newE of
      True  -> return newE
      False -> runGame newE w

explore :: World -> Point -> Explored -> Explored
explore w p@(x, y) e = case e !! x !! y of
                         Unexplored -> updateExplored w p e
                         _          -> e

updateExplored :: World -> Point -> Explored -> Explored
updateExplored w p@(x, y) e =
    case state of
      Clue 0 -> foldr (explore w) newExplored
                $ surrounding width height p
      _      -> newExplored
    where
      state       = (w !! x !! y)
      newExplored = replaceMatrixIndex p e state

isMineVisible :: Explored -> Bool
isMineVisible [] = False
isMineVisible (xs:xss)
              | containsMine xs = True
              | otherwise       = isMineVisible xss
              where containsMine xs = not $ all notMine xs
                    notMine Mine    = False
                    notMine _       = True

parseInput :: String -> Point
parseInput line = listToPair $ map read $ words line
    where listToPair (x:y:_) = (x, y)

genGame :: RandomGen g => Int -> Int -> Int -> g -> World
genGame w h n g = [zipWith combine ms cs | (ms, cs) <- zip mineMap clueMap]
                   where
                     mines   = nub $ genPoints w h n g
                     clueMap = genClueMap w h mines
                     mineMap = genWorld w h n mines

combine :: (State a) -> a -> State a
combine Mine _       = Mine
combine Unexplored _ = Unexplored
combine (Clue _) x   = Clue x

genPoints :: RandomGen g => Int -> Int -> Int -> g -> [Point]
genPoints w h n g = zip xs ys
    where
      xs = take n (randomRs (0, w-1) g)
      ys = drop n $ take (n*2) $ randomRs (0, h-1) g

genWorld :: Int -> Int -> Int -> [Point] -> World
genWorld w h n mines = foldr placeMine world mines
    where
      world             = makeMatrix w h (Clue 0) -- Initial world has no mines
      placeMine p world = replaceMatrixIndex p world Mine

genClueMap :: Int -> Int -> [Point] -> ClueMap
genClueMap w h mines = foldr succPoint clueMap surroundingPoints
    where
      surroundingPoints          = concat $ map (surrounding w h) mines
      clueMap                    = replicate w $ replicate h 0
      succPoint p@(x, y) clueMap = replaceMatrixIndex p clueMap
                                   $ succ (clueMap !! x !! y)

surrounding :: Int -> Int -> Point -> [Point]
surrounding w h (x, y) =
    filter (inBounds w h) [(x-1, y+1), (x, y+1), (x+1, y+1),
                           (x-1, y),             (x+1, y),
                           (x-1, y-1), (x, y-1), (x+1, y-1)]

showWorld ::  [[State Int]] -> IO ()
showWorld w = putStrLn $ showMatrixWith showSquare w

showSquare :: State Int -> String
showSquare Mine       = showCentered size "*"
showSquare Unexplored = showCentered size "#"
showSquare (Clue 0)   = showCentered size " "
showSquare (Clue n)   = showCentered size (show n)

showCentered :: Int -> String -> String
showCentered w x = (replicate leftPad ' ') ++ x ++ (replicate rightPad ' ')
    where leftPad  =  w `div` 2
          rightPad =  w - leftPad - (length x)

matrixMap :: (a -> b) -> [[a]] -> [[b]]
matrixMap f xss = map (map f) xss

showMatrixWith :: (a -> String) -> [[a]] -> String
showMatrixWith f = unlines . addBorder . map concat . matrixMap f . transpose

addBorder :: [String] -> [String]
addBorder xs = [horizontalBorder w]
               ++ map verticalBorder xs
               ++ [(horizontalBorder w)]
    where w                  = length (xs !! 0)
          h                  = length xs
          horizontalBorder w = "+" ++ (replicate w '-') ++ "+"
          verticalBorder xs  = "|" ++ xs ++ "|"

replaceMatrixIndex :: Point -> [[a]] -> a -> [[a]]
replaceMatrixIndex (x, y) m e = replaceIndex x m $ replaceIndex y (m !! x) e

replaceIndex :: Int -> [a] -> a -> [a]
replaceIndex index xs x = take index xs ++ ( x : (drop (index+1) xs))

makeMatrix :: Int -> Int -> a -> [[a]]
makeMatrix w h e = replicate w $ replicate h e

inBounds :: Int -> Int -> Point -> Bool
inBounds w h (x, y)
    | x < 0     = False
    | x >= w    = False
    | y < 0     = False
    | y >= h    = False
    | otherwise = True
