
import Control.Concurrent (forkIO, killThread, threadDelay,
                           MVar, newEmptyMVar, tryTakeMVar, tryPutMVar)
import Data.List (splitAt)
import Prelude hiding (Left, Right)
import System.IO (hSetEcho, stdin, hSetBuffering, BufferMode(NoBuffering))
import System.Random (getStdGen, randomRs)

type Point = (Int,Int)
type SnakeBody = [Point]
data Direction = Up | Down | Left | Right
data Snake = Snake SnakeBody Direction

type Apple = Point

type Dimensions = (Int,Int)
data Game = Game Dimensions Snake [Apple]

partitionAt :: Int -> [a] -> [[a]]
partitionAt _ [] = []
partitionAt n x  = partition : partitionAt n remaining
                   where (partition, remaining) = splitAt n x

renderCell :: Game -> Point -> Char
renderCell (Game _ (Snake body _) (apple:_)) p
  | elem p body = 'O'
  | p == apple  = '@'
  | otherwise   = '.'

render :: Game -> String
render game@(Game (cols, rows) _ _) =
  unlines $ partitionAt cols $ [ renderCell game (x,y) | y <- [0..rows-1],
                                                         x <- [0..cols-1]]

movePoint :: Dimensions -> Point -> Direction -> Point
movePoint (cols,rows) (x,y) Up    = (x,(mod (y-1) rows))
movePoint (cols,rows) (x,y) Down  = (x,(mod (y+1) rows))
movePoint (cols,rows) (x,y) Left  = ((mod (x-1) cols),y)
movePoint (cols,rows) (x,y) Right = ((mod (x+1) cols),y)

advanceGame :: Game -> Maybe Game
advanceGame (Game dimensions (Snake body direction) apples) =
  if eatsSelf
    then Nothing
    else Just (Game dimensions
                    (Snake (if eatsApple then newHead:body else newHead:(init body)) direction)
                    (if eatsApple then tail apples else apples))
  where newHead = movePoint dimensions (head body) direction
        eatsApple = newHead == (head apples)
        eatsSelf = elem newHead (init body)

applyNewDirection :: Game -> Direction -> Game
applyNewDirection (Game dimensions (Snake body direction) apples) newDirection =
  (Game dimensions (Snake body newDirection) apples)

charToDirection :: Char -> Maybe Direction
charToDirection 'j' = Just Left
charToDirection 'l' = Just Right
charToDirection 'k' = Just Down
charToDirection 'i' = Just Up
charToDirection _   = Nothing

gameLoop :: MVar (Maybe Direction) -> Maybe Game -> IO ()
gameLoop userActionVar (Just game) = do
  putStrLn $ render game
  threadDelay 500000
  userAction <- tryTakeMVar userActionVar
  case userAction of
    Just (Just newDirection) -> gameLoop userActionVar $
                                         advanceGame $
                                         applyNewDirection game newDirection
    _ -> gameLoop userActionVar $ advanceGame game
gameLoop _ Nothing = return ()

listenToKeys :: MVar (Maybe Direction) -> IO ()
listenToKeys userActionVar = do
  c <- getChar
  tryPutMVar userActionVar (charToDirection c)
  listenToKeys userActionVar

randomPoints :: Dimensions -> IO [Point]
randomPoints (cols,rows) = do
  g <- getStdGen
  return (zip (randomRs (0,cols-1) g) (randomRs (0,rows-1) g))

startScreen :: IO ()
startScreen  = do
  putStrLn "  _____             _"
  putStrLn " / ____|           | |"
  putStrLn "| (___  _ __   __ _| | _____"
  putStrLn " \\___ \\| '_ \\ / _` | |/ / _ \\"
  putStrLn " ____) | | | | (_| |   <  __/"
  putStrLn "|_____/|_| |_|\\__,_|_|\\_\\___|"
  putStrLn ""
  putStrLn ""
  putStrLn "Navigation: i = up  k = down  j = left  l = right"
  putStrLn ""
  putStrLn ""
  putStrLn "Press any key to start"
  getChar
  return ()

main :: IO ()
main = let dimensions = (40,15)
       in do
         hSetEcho stdin False
         hSetBuffering stdin NoBuffering
         startScreen
         userActionVar <- newEmptyMVar
         threadId <- forkIO $ listenToKeys userActionVar
         apples <- randomPoints dimensions
         gameLoop userActionVar
                  (Just (Game dimensions
                              (Snake [(4,0),(3,0),(2,0),(1,0),(0,0)] Right)
                              apples))
         putStrLn "Game Over!"
         killThread threadId
