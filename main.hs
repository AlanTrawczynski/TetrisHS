{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import System.Random (getStdGen, randomRs)
import Data.Matrix 
import Data.Text (pack)

main:: IO ()
main = do 
  g <- getStdGen
  let figs = randomRs (0, 7) g :: [Int]
  debugActivityOf (initTetris figs) manageEvent drawTetris


type Tetris = ([Int], Figure, Playfield, Time)
type Figure = ([Point], FigureType)
type FigureType = Char
type Playfield = Matrix Color -- Black -> Empty
type Time = Double


-- Init
-- ----------------------------------------------------------------------------------
initTetris:: [Int] -> Tetris
initTetris figs@(f:rest)
  | f == 0    = initTetris rest
  | otherwise = (figs, generateFigure f, playfield, 0)
    where playfield = matrix 20 10 (\_ -> black)
-- ----------------------------------------------------------------------------------


-- Events
-- ----------------------------------------------------------------------------------
manageEvent:: Event -> Tetris -> Tetris
manageEvent (TimePassing dt) st@(figs, f, pf, t) 
  | t > 1     = moveDown st
  | otherwise = (figs, f, pf, t+dt)
manageEvent (KeyPress k) st@(figs, f, pf, t) = case k of
  "Up"    -> (figs, rotateFigure f, pf, t)
  "Down"  -> moveDown st
  "Left"  -> moveLeft st
  "Right" -> moveRight st
  _       -> st
manageEvent _ st = st
-- ----------------------------------------------------------------------------------


-- Drawing
-- ----------------------------------------------------------------------------------
drawTetris:: Tetris -> Picture
drawTetris (_, f, pf,_) = ftext & (center $ drawFigure f & drawPlayfield pf) & coordinatePlane
  where center = id --translated ((-nc'-1)/2) ((-nr'-1)/2)
        nr' = fromIntegral $ nrows pf
        nc' = fromIntegral $ ncols pf
        ftext = colored green (lettering $ pack $ show $ fst f) --temp

drawFigure:: Figure -> Picture
drawFigure (ps, t) = pictures $ map (\p -> drawPoint p c) ps
  where c = dull $ case t of
          'O' -> yellow
          'I' -> light blue
          'L' -> orange
          'J' -> blue
          'S' -> red
          'Z' -> green
          'T' -> purple

drawPlayfield:: Playfield -> Picture
drawPlayfield pf = squares & bg
  where nr = nrows pf
        nc = ncols pf
        squares = pictures [drawPoint p c | 
                            row <- [1..nr], 
                            col <- [1..nc], 
                            let p = (fromIntegral col, fromIntegral row),
                            let c = pf !. p,
                            c /= black]
        bg = colored black (translated ((nc'+1)/2) ((nr'+1)/2) (solidRectangle nc' nr'))
          where nc' = fromIntegral nc
                nr' = fromIntegral nr

drawPoint :: Point -> Color -> Picture
drawPoint (x, y) c = colored c (translated x y (solidRectangle 0.95 0.95))
-- ----------------------------------------------------------------------------------


-- Playfield
-- ----------------------------------------------------------------------------------
(!.) :: Playfield -> Point -> Color
pf !. (x,y) = getElem r c pf
  where r = (nrows pf) - (round y) + 1
        c = round x       

setElem':: Color -> Point -> Playfield -> Playfield
setElem' color (x,y) pf = setElem color (r,c) pf
  where r = (nrows pf) - (round y) + 1
        c = round x  

validPosition :: Figure -> Playfield -> Bool
validPosition ([], _) _ = True
validPosition ((x,y):ps, t) pf = doesNotExceed && doesNotCollide && validPosition (ps, t) pf
  where doesNotCollide = y > 20 || (pf !. (x,y)) == black
        doesNotExceed = (x >= 1) && (x <= nc') && (y >= 1)
          where nc' = fromIntegral $ ncols pf

refresh :: Playfield -> Figure -> Playfield
refresh pf ([], _)        = pf
refresh pf (p:ps, t)  = refresh pf' (ps, t)
  where pf' = setElem' c p pf
        c = red -- hay que definir un map (tipo figura, color) para usarlo de forma general

moveDown :: Tetris -> Tetris
moveDown (figs, f, pf, _)
  | validPosition mf pf = (figs,  mf, pf,   0)
  | otherwise           = (figs', nf, pf',  0)
  where mf = moveFigure f 0 (-1)
        (nf, figs') = nextFigure figs
        pf' = refresh pf f

moveLeft :: Tetris -> Tetris
moveLeft st@(figs, f, pf, time) 
  | validPosition mf pf = (figs, mf, pf, time)
  | otherwise = st
  where mf = moveFigure f (-1) 0
 
moveRight :: Tetris -> Tetris
moveRight st@(figs, f, pf, time) 
  | validPosition mf pf = (figs, mf, pf, time)
  | otherwise = st
  where mf = moveFigure f 1 0

moveFigure:: Figure -> Double -> Double -> Figure
moveFigure (ps, t) dx dy = (move ps, t)
  where move [] = []
        move ((x,y):r) = (x+dx, y+dy):(move r)   
-- ----------------------------------------------------------------------------------


-- Figure
-- ----------------------------------------------------------------------------------
nextFigure:: [Int] -> (Figure, [Int])
nextFigure (current:next:rest)
  | current /= next && next /= 0 = (generateFigure next, next:rest)
  | otherwise = reroll
    where reroll = (generateFigure next', next':rest')
          (next':rest') = dropWhile (==0) rest

generateFigure:: Int -> Figure
generateFigure n = case n of
  1 -> ([(5,22),(6,22),(5,23),(6,23)], 'O')
  2 -> ([(4,22),(5,22),(6,22),(7,22)], 'I')
  3 -> ([(5,22),(4,22),(6,22),(6,23)], 'L')
  4 -> ([(5,22),(4,22),(6,22),(4,23)], 'J')
  5 -> ([(5,22),(4,22),(5,23),(6,23)], 'S')
  6 -> ([(5,22),(6,22),(4,23),(5,23)], 'Z')
  7 -> ([(5,22),(4,22),(6,22),(5,23)], 'T')

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
-- ----------------------------------------------------------------------------------
