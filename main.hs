import Control.Monad
import System.Random
import Data.List
import System.IO
import Debug.Trace

data State a = Mine | Unexplored | Clue a deriving(Show)

type Point    = (Int, Int)
type World    = [[State Int]]
type ClueMap  = [[Int]]
type Explored = [[State Int]]

size   = 2  -- the size of each cell
width  = 30 -- the width of the board
height = 30 -- the height of the board

{- We start the program by first creating
   a RandomGen which is used as a seed for planting mines.
   Then we create two World's explored and world,
     - explored will be what is displayed
     - world will store the mine locations
   We then start the game -}
main :: IO ()
main = do
  g        <- getStdGen
  explored <- return $ makeMatrix width height Unexplored -- nothing is explored
  world    <- return $ genGame width height (width * height `div` 10) g
  runGame explored world >>= showWorld

{- This is the game loop,
     - we first draw the board
     - then get the move from the player
     - then explore the world at the players move.
   The game ends if a mine is visible after a move.
   The loop happens by recursively calling runGame
   with the new explored board -}
runGame :: Explored -> World -> IO Explored
runGame e w = do
  showWorld e
  move <- getLine
  newE <- return $ explore w (parseInput move) e
  case isMineVisible newE of
      True  -> return newE
      False -> runGame newE w

{- There are two cases we care about when exploring a move,
     - The point is unexplored, in which case we explore it
     - it has already been explored, so we don't have to do anything -}
explore :: World -> Point -> Explored -> Explored
explore w p@(x, y) e = case e !! x !! y of
                         Unexplored -> updateExplored w p e
                         _          -> e

{- To update the explored map, we copy the state
   from the world to the explored map.
   If the state is Clue 0, we explore the
   surrounding cells to save the player the
   mindless job of exploring those manually -}
updateExplored :: World -> Point -> Explored -> Explored
updateExplored w p@(x, y) e =
    case state of
      Clue 0 -> foldr (explore w) newExplored $ surrounding width height p
      _      -> newExplored
    where
      state       = (w !! x !! y)
      newExplored = replaceMatrixIndex p e state

{- Checks if a mine is visible in the explored map -}
isMineVisible :: Explored -> Bool
isMineVisible [] = False
isMineVisible (xs:xss)
              | containsMine xs = True
              | otherwise       = isMineVisible xss
              where containsMine xs = not $ all notMine xs
                    notMine Mine    = False
                    notMine _       = True
                                      
{- Parses the user input into a point -}
parseInput :: String -> Point
parseInput line = listToPair $ map read $ words line
    where listToPair (x:y:_) = (x, y)

{- Generates a world of dimensions w, h, with n mines scattered
   randomly throughout and all the clues numbers calculated -}
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
                                            
{- Generates a list of random points with in the box of dimensions w h -}
genPoints :: RandomGen g => Int -> Int -> Int -> g -> [Point]
genPoints w h n g = zip xs ys
    where
      xs = take n (randomRs (0, w-1) g)
      ys = drop n $ take (n*2) $ randomRs (0, h-1) g

{- Generates a world of dimensions w h and mines randomly scattered -}
genWorld :: Int -> Int -> Int -> [Point] -> World
genWorld w h n mines = foldr placeMine world mines
    where
      world             = makeMatrix w h (Clue 0) -- Initial world has no mines
      placeMine p world = replaceMatrixIndex p world Mine

{- Generates a map with the clue numbers in each cell representing
   the number of adjacent to mines -}
genClueMap :: Int -> Int -> [Point] -> ClueMap
genClueMap w h mines = foldr succPoint clueMap surroundingPoints
    where
      surroundingPoints          = concat $ map (surrounding w h) mines
      clueMap                    = replicate w $ replicate h 0
      succPoint p@(x, y) clueMap = replaceMatrixIndex p clueMap
                                   $ succ (clueMap !! x !! y)
                                     
{- Finds all points surrounding a point with all
   points outside of the bounds w h filtered out -}
surrounding :: Int -> Int -> Point -> [Point]
surrounding w h (x, y) =
    filter (inBounds w h) [(x-1, y+1), (x, y+1), (x+1, y+1),
                           (x-1, y),             (x+1, y),
                           (x-1, y-1), (x, y-1), (x+1, y-1)]

{- Prints out the world -}
showWorld ::  World -> IO ()
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

{- Maps a function over a list of lists -}
matrixMap :: (a -> b) -> [[a]] -> [[b]]
matrixMap f xss = map (map f) xss

showMatrixWith :: (a -> String) -> [[a]] -> String
showMatrixWith f = unlines . addBorder . map concat . matrixMap f . transpose

{- Adds a border around a list of strings -}
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

{- Replaces an element at an index in a list with another element -}
replaceIndex :: Int -> [a] -> a -> [a]
replaceIndex index xs x = take index xs ++ ( x : (drop (index+1) xs))

{- Creates a matrix fill with a given element -}
makeMatrix :: Int -> Int -> a -> [[a]]
makeMatrix w h e = replicate w $ replicate h e

inBounds :: Int -> Int -> Point -> Bool
inBounds w h (x, y)
    | x < 0     = False
    | x >= w    = False
    | y < 0     = False
    | y >= h    = False
    | otherwise = True
