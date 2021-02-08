{-# LANGUAGE OverloadedStrings #-}
import CodeWorld
import System.Random (getStdGen, randomRs)
import Data.Matrix
import Data.Text (pack)

main :: IO ()
main = do
  g <- getStdGen
  let fgen = randomRs (1, 7) g :: FigureGenerator
  debugActivityOf (initTetris fgen) manageEvent drawTetris


-- Types
-- ----------------------------------------------------------------------------------
data Tetris =
  Tetris {
    fgen  ::  FigureGenerator,
    f     ::  Figure,
    pf    ::  Playfield,
    t     ::  Time,
    dclk  ::  DefaultClock,
    clk   ::  Clock,
    sc    ::  Score,
    st    ::  State
  }

type FigureGenerator = [Int]
type Figure = ([Point], FigureType)
type FigureType = Char
type Playfield = Matrix Color
type Time = Double
type DefaultClock = Double
type Clock = Double
type Score = Int
data State = Normal | Pause | GameOver
-- ----------------------------------------------------------------------------------


-- Init
-- ----------------------------------------------------------------------------------
initTetris :: FigureGenerator -> Tetris
initTetris fgen@(n:rest) = Tetris fgen f pf 0 1 1 0 Normal
    where pf = matrix 20 10 (\_ -> black)
          f = generateFigure n
-- ----------------------------------------------------------------------------------


-- Events
-- ----------------------------------------------------------------------------------
manageEvent, manageNormal, managePause, manageGameover::
  Event -> Tetris -> Tetris

manageEvent event tetris = manager event tetris
  where manager = case st tetris of
                    Normal    -> manageNormal
                    Pause     -> managePause
                    GameOver  -> manageGameover

manageNormal (KeyRelease "Esc") tetris = tetris {st = Pause}
manageNormal (TimePassing dt) tetris
  | clk tetris < 0  = moveDown tetris
  | otherwise       = tetris {t = (t tetris)+dt, clk = (clk tetris)-dt}
manageNormal (KeyPress k) tetris = case k of
  "Up"    -> rotateFigure tetris
  "Down"  -> moveDown tetris
  "Left"  -> moveLeft tetris
  "Right" -> moveRight tetris
  _       -> tetris
manageNormal _ tetris = tetris

managePause (KeyRelease "Esc") tetris = tetris {st = Normal}
managePause (KeyRelease "N") tetris = newGame tetris
managePause _ tetris = tetris

manageGameover (KeyRelease "N") tetris = newGame tetris
manageGameover _ tetris = tetris

newGame:: Tetris -> Tetris
newGame = initTetris . tail . fgen
-- ----------------------------------------------------------------------------------


-- Drawing
-- ----------------------------------------------------------------------------------
drawTetris :: Tetris -> Picture
drawTetris tetris = draw tetris & drawBackground $ pf tetris
  where draw = case st tetris of
                Normal    -> drawNormal
                Pause     -> drawPause
                GameOver  -> drawGameOver

drawBackground :: Playfield -> Picture
drawBackground pf = solidRectangle x y
  where y = (fromIntegral $ nrows pf) * 2
        x = 2*y

drawNormal :: Tetris -> Picture
drawNormal tetris = center $ pictures [drawFigure $ f tetris, drawPlayfield pf_, drawScore $ sc tetris]
  where center = translated ((-nc'-1)/2) ((-nr'-1)/2)
        nr' = fromIntegral $ nrows $ pf_
        nc' = fromIntegral $ ncols $ pf_
        pf_ = pf tetris

drawFigure :: Figure -> Picture
drawFigure (ps, ft) = pictures $ map (\p -> drawSquare p c) ps
  where c = figuretypeColor ft

drawPlayfield :: Playfield -> Picture
drawPlayfield pf = pictures [if c /= black then drawSquare p c else drawPoint p |
                            row <- [1..nrows pf],
                            col <- [1..ncols pf],
                            let p = (fromIntegral col, fromIntegral row),
                            let c = pf !. p]

drawScore :: Int -> Picture
drawScore n = translated (-2) 1.5 $ (lettering $ pack $ show n) & (colored gray $ solidRectangle 4 2)

drawSquare :: Point -> Color -> Picture
drawSquare (x, y) c = colored c (translated x y (solidRectangle 0.95 0.95))

drawPoint :: Point -> Picture
drawPoint (x, y) = colored pointColor (translated x y (solidRectangle 0.1 0.1))

drawPause :: Tetris -> Picture
drawPause tetris = drawMenu "PAUSED"

drawGameOver :: Tetris -> Picture
drawGameOver tetris = drawMenu "GAME OVER"

drawMenu :: String -> Picture
drawMenu text = colored white title
  where title = translated 0 5 (styledLettering Plain Fancy (pack $ text))
-- ----------------------------------------------------------------------------------


-- Colors
-- ----------------------------------------------------------------------------------
figuretypeColor :: FigureType -> Color
figuretypeColor ft = dull $ case ft of
  'O' -> yellow
  'I' -> light blue
  'L' -> orange
  'J' -> blue
  'S' -> red
  'Z' -> green
  'T' -> purple

pointColor:: Color
pointColor = bright yellow
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
validPosition ((x,y):ps, ft) pf = doesNotExceed && doesNotCollide && validPosition (ps, ft) pf
  where doesNotCollide = y > 20 || (pf !. (x,y)) == black
        doesNotExceed = (x >= 1) && (x <= nc') && (y >= 1)
          where nc' = fromIntegral $ ncols pf

updatePlayfield :: Playfield -> Figure -> (Playfield, [Int])
updatePlayfield pf ([], _)    = removeFullRows pf
updatePlayfield pf (p:ps, ft) = updatePlayfield pf' (ps, ft)
  where pf' = setElem' c p pf
        c = figuretypeColor ft

removeFullRows :: Playfield -> (Playfield, [Int])
removeFullRows pf
  | null is   = (pf,[])
  | otherwise = (newRows toAdd nc <-> removeRows is pf, is)
  where is = fullRows pf -- lista de las filas a eliminar
        toAdd = length is
        nc = ncols pf

-- las líneas a introducir arriba de pf si procede. Serán tantas como eliminadas.
newRows :: Int -> Int -> Playfield
newRows nr nc = matrix nr nc (\_ -> black)

-- fullRows es la función encargada de obtener una lista de índices con las filas a borrar.
fullRows :: Playfield -> [Int]
fullRows pf = [ row |
                (row, colors) <- zip [1..] (toLists pf),
                all (/= black) colors]

removeRows :: [Int] -> Playfield -> Playfield
removeRows [] pf = pf
removeRows toRemove pf = fromLists [row | (i,row) <- zip [1..nr] pfList, notElem i toRemove]
  where nr = nrows pf
        pfList = toLists pf

moveDown :: Tetris -> Tetris
moveDown tetris
  | validPosition mf pf_  = tetris {f = mf, clk = dclk tetris}
  | isGameOver f_         = tetris {st = GameOver}
  | otherwise             = moveDown' tetris
  where mf = moveFigure f_ 0 (-1)
        pf_ = pf tetris
        f_ = f tetris

moveDown' tetris = tetris {fgen = fgen', f = nf, pf = pf', dclk = dclk', clk = dclk', sc = sc'}
  where dclk_ = dclk tetris
        (nf, fgen') = nextFigure $ fgen tetris
        (pf', delRows) = updatePlayfield (pf tetris) (f tetris)
        dclk' = max 0.15 (dclk_-0.01)
        sc' = (sc tetris) + (round $ n * 100 * (1/dclk_))
          where n = fromIntegral $ length $ delRows

isGameOver:: Figure -> Bool
isGameOver (ps, _) = any (>20) (map snd ps)

moveLeft :: Tetris -> Tetris
moveLeft tetris
  | validPosition mf (pf tetris) = tetris {f = mf}
  | otherwise = tetris
  where mf = moveFigure (f tetris) (-1) 0

moveRight :: Tetris -> Tetris
moveRight tetris
  | validPosition mf (pf tetris) = tetris {f = mf}
  | otherwise = tetris
  where mf = moveFigure (f tetris) 1 0

moveFigure :: Figure -> Double -> Double -> Figure
moveFigure (ps, ft) dx dy = (move ps, ft)
  where move [] = []
        move ((x,y):r) = (x+dx, y+dy):(move r)
-- ----------------------------------------------------------------------------------


-- Figure
-- ----------------------------------------------------------------------------------
nextFigure :: FigureGenerator -> (Figure, FigureGenerator)
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
rotateFigure tetris = case maybef' of
  Nothing -> tetris
  Just f' -> tetris {f = f'}
  where maybef' = safeHead $ filter (\f -> validPosition f (pf tetris)) (map ($rf) mvs)
        rf = rotateFigure' $ f tetris
        mvs = [ \x -> moveFigure x 0    0,
                \x -> moveFigure x 1    0,
                \x -> moveFigure x (-1) 0,
                \x -> moveFigure x 2    0,
                \x -> moveFigure x (-2) 0]

rotateFigure' :: Figure -> Figure
rotateFigure' (ps, ft) = case ft of
  'O' -> (ps, ft)
  'I' -> (ps', ft)
    where ps' = rotatePoints center ps
          [_,(x1,y1),(x2,y2),_] = ps
          center  | x1 < x2 = ((x1+x2)/2, y1-0.5)
                  | x1 > x2 = ((x1+x2)/2, y1+0.5)
                  | y1 > y2 = (x1-0.5, (y1+y2)/2)
                  | y1 < y2 = (x1+0.5, (y1+y2)/2)
  ft   -> (ps', ft)
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