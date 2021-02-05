{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import System.Random (getStdGen, randomRs)
import Data.Matrix (matrix)

main:: IO ()
main = do 
  g <- getStdGen
  let figs = randomRs (0, 7) g :: [Int]

  debugActivityOf (initTetris figs) manageEvent drawTetris


type Tetris = ([Int], Figure, Playfield)

type Figure = ([Point], FigureType)
type FigureType = Char

-- Black -> Empty
type Playfield = Matrix Color


initTetris:: [Int] -> Tetris
initTetris (f:figs) = (figs, spawnFigure f, playfield)
    where playfield = matrix 20 10 (\_ -> black)

manageEvent:: Event -> Tetris -> Tetris
manageEvent (TimePassing t) state@(figs,figura,playfield) 
                    | mod t 1000 == 0 = newState
                    | otherwise = state
                    where newState = (figs,moveDown figura,playfield) 

manageEvent (KeyPress t) (figs,figura,playfield) = (figs,nuevaFigura,playfield)
                    where nuevaFigura = case t of "Up" -> rotateFigure figura
                                             t of "Down" -> moveDown figura 
                                             t of "Left" -> moveLeft figura
                                             t of "Right" -> moveRight figura
                                             t of _ -> figura 


drawTetris:: Tetris -> Picture
drawTetris (_, f, m) = center $ drawFigure f & drawPlayfield m
  where center = translated (-nc'/2) (-nr'/2)
        nr' = fromIntegral $ nrows m
        nc' = fromIntegral $ ncols m

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
rotateFigure (ps, t) = case t of
  'O' -> (ps, t)
  'I' -> (ps', t)  
    where ps' = rotatePoints center ps
          [_,(x1,y1),(x2,y2),_] = ps
          center  | x1 < x2 = ((x1+x2)/2, y1-0.5)
                  | x1 > x2 = ((x1+x2)/2, y1+0.5)
                  | y1 > y2 = (x1-0.5, (y1+y2)/2)
                  | y1 < y2 = (x1+0.5, (y1+y2)/2)
  t   -> (ps', t)
    where ps' = center:(rotatePoints center rest)
          (center:rest) = ps

rotatePoints:: Point -> [Point] -> [Point]
rotatePoints center ps = map (rotate center) ps
  where rotate (xo,yo) (xi,yi) = (yi-yo+xo, -(xi-xo)+yo)
