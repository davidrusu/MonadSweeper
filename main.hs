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

showWorld w = putStrLn $ showMatrixWith showSquare w
              
runGame e w = do
  showWorld e
  showWorld w
  move <- getLine
  newE <- return $ explore w (parseInput move) e
  case isMineVisible newE of
      True  -> return newE
      False -> runGame newE w

explore :: World -> (Int, Int) -> Explored -> Explored
explore w p@(x, y) e = case e !! x !! y of
                         Unexplored -> updateExplored w p e
                         _          -> e
                                       
updateExplored w p@(x, y) e = case state of
                                Clue 0 -> foldr (explore w) newExplored
                                          $ filter (inBounds width height) (surrounding p)
                                _      -> newExplored
    where
      state       = (w !! x !! y)
      newExplored = replaceMatrixIndex p e state
  
main = do
  g <- getStdGen
  e <- return $ makeMatrix width height Unexplored
  w <- return $ genGame width height 10 (mkStdGen 1)
  runGame e w >>= showWorld

isMineVisible :: Explored -> Bool
isMineVisible [] = False
isMineVisible (xs:xss)
              | containsMine xs = True
              | otherwise        = isMineVisible xss
                                  
containsMine :: [State a] -> Bool
containsMine xs = not $ all notMine xs

notMine :: State a -> Bool
notMine Mine = False
notMine _    = True

                    
parseInput :: String -> (Int, Int)
parseInput line = listToPair $ map read $ words line

listToPair :: [a] -> (a, a)
listToPair (x:y:_) = (x, y)
         
genGame :: RandomGen g => Int -> Int -> Int -> g -> World
genGame w h n g = [zipWith combine ms cs | (ms, cs) <- zip mineMap clueMap]
                   where
                     mines   = nub $ genPoints w h n g
                     clueMap = genClueMap w h mines
                     mineMap = genWorld w h n mines

combine :: (State a) -> a -> State a
combine Mine _     = Mine
combine (Clue _) x = Clue x
                               
genPoints :: RandomGen g => Int -> Int -> Int -> g -> [Point]
genPoints w h n g = zip xs ys
    where
      xs = take n (randomRs (0, w-1) g)
      ys = drop n $ take (n*2) (randomRs (0, h-1) g)
                             
genWorld :: Int -> Int -> Int -> [Point] -> World
genWorld w h n mines =
    foldr placeMine world mines
        where
          world             = makeMatrix w h (Clue 0)
          placeMine p world = replaceMatrixIndex p world Mine

genClueMap :: Int -> Int -> [Point] -> ClueMap
genClueMap w h mines = foldr incrementPoint clueMap surroundingPoints
    where
      surroundingPoints = filter (inBounds w h) $ concat $ map surrounding mines
      clueMap           = replicate w $ replicate h 0

incrementPoint :: Point -> ClueMap -> ClueMap
incrementPoint p@(x, y) clueMap = replaceMatrixIndex p clueMap
                                  $ succ (clueMap !! x !! y)

surrounding :: Point -> [Point]
surrounding (x, y) = [(x-1, y+1), (x, y+1), (x+1, y+1),
                      (x-1, y),             (x+1, y),
                      (x-1, y-1), (x, y-1), (x+1, y-1)]

showState :: State Int -> String
showState Mine     = "*"
showState (Clue 0) = " " 
showState (Clue n) = (show n)

showSquare :: State Int -> String
showSquare Mine       = showCentered size "*"
showSquare Unexplored = showCentered size " "
showSquare (Clue 0)   = showCentered size " " 
showSquare (Clue n)   = showCentered size (show n)
                    
showCentered :: Int -> String -> String
showCentered w x = (replicate hW ' ') ++ x ++ (replicate (w - hW - 1) ' ')
    where hW =  w `div` 2
                                                               
matrixMap :: (a -> b) -> [[a]] -> [[b]]
matrixMap f xss = map (map f) xss

showMatrix :: Show a => [[a]] -> String
showMatrix = showMatrixWith show

showMatrixWith :: Show a => (a -> String) -> [[a]] -> String
showMatrixWith f = unlines . addBorder . map concat . matrixMap f . transpose

addBorder :: [String] -> [String]
addBorder xs = let w = length (xs !! 0)
                   h = length xs in
               [horizontalBorder w]
               ++ map verticalBorder xs
               ++ [(horizontalBorder w)]

horizontalBorder :: Int -> String
horizontalBorder w = "+" ++ (replicate w '-') ++ "+"

verticalBorder :: String -> String
verticalBorder xs = "|" ++ xs ++ "|"

splitInto :: Int -> [a] -> [[a]]
splitInto 1 xs = [xs]
splitInto n xs = let w = length xs `div` n in
                 take w xs : splitInto (n-1) (drop w xs)

replaceIndex :: Int -> [a] -> a -> [a]
replaceIndex index xs x = take index xs ++ ( x : (drop (index+1) xs))

replaceMatrixIndex :: Point -> [[a]] -> a -> [[a]]
replaceMatrixIndex (x, y) m e = replaceIndex x m $ replaceIndex y (m !! x) e
                              
makeMatrix w h e = replicate w $ replicate h e
                   
inBounds :: Int -> Int -> Point -> Bool
inBounds w h (x, y)
    | x < 0     = False
    | x >= w    = False
    | y < 0     = False
    | y >= h    = False
    | otherwise = True
