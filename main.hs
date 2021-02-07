{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import System.Random (getStdGen, randomRs)
import Data.Matrix
import Data.Text (pack)

main :: IO ()
main = do
  g <- getStdGen
  let figs = randomRs (1, 7) g :: [Int]
  debugActivityOf (initTetris figs) manageEvent drawTetris


type Tetris = ([Int], Figure, Playfield, Time)
type Figure = ([Point], FigureType)
type FigureType = Char
type Playfield = Matrix Color -- Black -> Empty
type Time = Double


-- Init
-- ----------------------------------------------------------------------------------
initTetris :: [Int] -> Tetris
initTetris figs@(f:rest)
  | f == 0    = initTetris rest
  | otherwise = (figs, generateFigure f, playfield, 0)
    where playfield = matrix 20 10 (\_ -> black)
-- ----------------------------------------------------------------------------------


-- Events
-- ----------------------------------------------------------------------------------
manageEvent :: Event -> Tetris -> Tetris
manageEvent (TimePassing dt) st@(figs, f, pf, t)
  | t > 1     = moveDown st
  | otherwise = (figs, f, pf, t+dt)
manageEvent (KeyPress k) st@(figs, f, pf, t) = case k of
  "Up"    -> rotateFigure st
  "Down"  -> moveDown st
  "Left"  -> moveLeft st
  "Right" -> moveRight st
  _       -> st
manageEvent _ st = st
-- ----------------------------------------------------------------------------------


-- Drawing
-- ----------------------------------------------------------------------------------
drawTetris :: Tetris -> Picture
drawTetris (_, f, pf,_) = ftext & (center $ drawFigure f & drawPlayfield pf) & coordinatePlane
  where center = id --translated ((-nc'-1)/2) ((-nr'-1)/2)
        nr' = fromIntegral $ nrows pf
        nc' = fromIntegral $ ncols pf
        ftext = colored green (lettering $ pack $ show $ fst f) --temp

drawFigure :: Figure -> Picture
drawFigure (ps, t) = pictures $ map (\p -> drawPoint p c) ps
  where c = figuretypeColor t

drawPlayfield :: Playfield -> Picture
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

figuretypeColor :: FigureType -> Color
figuretypeColor t = dull $ case t of
  'O' -> yellow
  'I' -> light blue
  'L' -> orange
  'J' -> blue
  'S' -> red
  'Z' -> green
  'T' -> purple
-- ----------------------------------------------------------------------------------


-- Playfield
-- ----------------------------------------------------------------------------------
(!.) :: Playfield -> Point -> Color
pf !. (x,y) = getElem r c pf
  where r = (nrows pf) - (round y) + 1
        c = round x

setElem' :: Color -> Point -> Playfield -> Playfield
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
refresh pf ([], _)    = checkLines pf
refresh pf (p:ps, t)  = refresh pf' (ps, t)
  where pf' = setElem' c p pf
        c = figuretypeColor t

checkLines :: Playfield -> Playfield
checkLines pf
  | null is = pf
  | otherwise = newLines toAdd m <-> removeLines is pf 0
  where is = fullLines pf -- lista de las filas a eliminar
        toAdd = length is
        m = ncols pf

-- las líneas a introducir arriba de pf si procede. Serán tantas como eliminadas.
newLines :: Int -> Int -> Playfield
newLines n m = fromList n m xs
  where xs = case n of 0 -> []
                       _ -> take (n*m) $ repeat black

-- FullLines es la función encargada de obtener una lista de índices con las filas a borrar.
fullLines :: Playfield -> [Int]
fullLines pf = [ i | i <- [1..n], all (/= black) [ pf!(i,j) | j <- [1..m] ] ]
  where n = nrows pf
        m = ncols pf

-- aux: almacena el número de filas removidas. Los índices a remover son índices de la matriz pf.
-- no obstante el borrado se realiza de manera progresiva, si tenemos más de una fila a borrar,
-- se borrarán uno detrás de otro. Esto significa que los índices no nos valdrán, hay que transformarlos.
-- los índices que queden debajo de la fila borrada son i - aux.
-- 1 1
-- 2 1 <- si elimino esto, la fila 3 1 tendrá como índices 2 1.
-- 3 1
-- NOTA: La lista de índices a borrar está ordenada.

removeLines :: [Int] -> Playfield -> Int -> Playfield
removeLines [] pf aux = pf
removeLines (i:is) pf aux
  | i' == 1 = removeLines is (submatrix (i'+1) (n) 1 m pf) (aux+1)
  | i' == n = removeLines is (submatrix 1 (i'-1) 1 m pf) (aux+1)
  | otherwise = removeLines is ((submatrix 1 (i'-1) 1 m pf) <-> (submatrix (i'+1) (n) 1 m pf)) (aux+1)
  where m = ncols pf
        n = nrows pf
        i' = i - aux

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

moveFigure :: Figure -> Double -> Double -> Figure
moveFigure (ps, t) dx dy = (move ps, t)
  where move [] = []
        move ((x,y):r) = (x+dx, y+dy):(move r)
-- ----------------------------------------------------------------------------------


-- Figure
-- ----------------------------------------------------------------------------------
nextFigure :: [Int] -> (Figure, [Int])
nextFigure (current:next:next2:rest)
  | current /= next = (generateFigure next, next:rest)
  | otherwise       = (generateFigure next2, next2:rest)

generateFigure :: Int -> Figure
generateFigure n = case n of
  1 -> ([(5,22),(6,22),(5,23),(6,23)], 'O')
  2 -> ([(4,22),(5,22),(6,22),(7,22)], 'I')
  3 -> ([(5,22),(4,22),(6,22),(6,23)], 'L')
  4 -> ([(5,22),(4,22),(6,22),(4,23)], 'J')
  5 -> ([(5,22),(4,22),(5,23),(6,23)], 'S')
  6 -> ([(5,22),(6,22),(4,23),(5,23)], 'Z')
  7 -> ([(5,22),(4,22),(6,22),(5,23)], 'T')

rotateFigure :: Tetris -> Tetris
rotateFigure st@(figs, f, pf, t) = case maybef' of
  Nothing -> st
  Just f' -> (figs, f', pf, t)
  where maybef' = safeHead $ filter (\f -> validPosition f pf) (map ($rf) mvs)
        rf = rotateFigure' f
        mvs = [ \x -> moveFigure x 0    0,
                \x -> moveFigure x 1    0,
                \x -> moveFigure x (-1) 0,
                \x -> moveFigure x 2    0,
                \x -> moveFigure x (-2) 0]

rotateFigure' :: Figure -> Figure
rotateFigure' (ps, t) = case t of
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

rotatePoints :: Point -> [Point] -> [Point]
rotatePoints center ps = map (rotate center) ps
  where rotate (xo,yo) (xi,yi) = (yi-yo+xo, -(xi-xo)+yo)
-- ----------------------------------------------------------------------------------


-- Utils
-- ----------------------------------------------------------------------------------
safeHead:: [a] -> Maybe a
safeHead [] = Nothing
safeHead l = Just $ head l
-- ----------------------------------------------------------------------------------
