import CodeWorld
import System.Random
import Data.Matrix

main:: IO ()
main = do 
  g <- getStdGen
  let   figs = randomRs (0, 7) g :: [Int]

  activityOf (initTetris figs) manageEvent drawTetris


type Tetris = ([Int], Figure, Playfield)
type Figure = ([Point], FigureType)
type FigureType = Int
type Playfield = Matrix Color


initTetris:: [Int] -> Tetris
initTetris (f:figs) = (figs, spawnFigure f, playfield)
    where playfield = matrix 10 20 (\_ -> black)

manageEvent:: Event -> Tetris -> Tetris
manageEvent = undefined

drawTetris:: Tetris -> Picture
drawTetris = undefined

spawnFigure:: Int -> Figure
spawnFigure = undefined

rotateFigure:: Figure -> Figure
rotateFigure ((center:ps), t) = ((center:ps'), t)
    where   ps' = map (rotate center) ps
            rotate (xo, yo) (xi, yi) = (yi-yo+xo, -(xi-xo)+yo)
