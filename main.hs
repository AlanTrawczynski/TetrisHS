import CodeWorld
import System.Random
import Data.Matrix

main:: IO ()
main = do 
  g <- getStdGen
  let figs = randomRs (0, 7) g :: [Int]

  debugActivityOf (initTetris figs) manageEvent drawTetris


type Tetris = ([Int], Figure, Playfield)

type Figure = ([Point], FigureType)
-- 0 -> O | 1 -> I | 2 -> L | 3 -> J | 4 -> S | 5 -> Z | 6 -> T
type FigureType = Int

-- Black -> Empty
type Playfield = Matrix Color


initTetris:: [Int] -> Tetris
initTetris (f:figs) = (figs, spawnFigure f, playfield)
    where playfield = matrix 20 10 (\_ -> black)

manageEvent:: Event -> Tetris -> Tetris
manageEvent _ state = state

drawTetris:: Tetris -> Picture
drawTetris (_, f, m) = center $ drawFigure f & drawPlayfield m
  where center = translated (-ncols'/2) (-nrows'/2)
        ncols' = fromIntegral $ ncols m
        nrows' = fromIntegral $ nrows m

drawFigure:: Figure -> Picture
drawFigure (ps, t) = pictures $ map (\p -> drawPoint p c) ps
  where c = red -- caseOf con color por figura

drawPlayfield:: Playfield -> Picture
drawPlayfield m = squares & bg
  where nr = nrows m
        nc = ncols m
        squares = pictures [drawPoint p color | 
                            row <- [1..nr], 
                            col <- [1..nc], 
                            let p = (fromIntegral col, fromIntegral row),
                            let color = m !. (row,col),
                            color /= black]
        bg = colored black (translated (x/2) (y/2) (solidRectangle x y))
          where x = fromIntegral nc
                y = fromIntegral nr

drawPoint :: Point -> Color -> Picture
drawPoint (x, y) c = colored c (translated (x-0.5) (y-0.5) (solidRectangle 0.95 0.95))

(!.) :: Matrix a -> (Int,Int) -> a
m !. (r,c) = getElem r' c m
  where r' = (nrows m) - r + 1

spawnFigure:: Int -> Figure
spawnFigure = undefined

rotateFigure:: Figure -> Figure
rotateFigure ((center:ps), t) = ((center:ps'), t)
    where   ps' = map (rotate center) ps
            rotate (xo, yo) (xi, yi) = (yi-yo+xo, -(xi-xo)+yo)
