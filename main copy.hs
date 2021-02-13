{-# LANGUAGE OverloadedStrings #-}
module Tetris (runTetris, runCustomTetris) where

import Data.List (genericLength, findIndices)
import System.Random (getStdGen, randomRs)
import Text.Printf (printf)
import Data.Char (isDigit)
import Data.Text (pack)
import Data.Matrix
import CodeWorld


-- IO
-- ----------------------------------------------------------------------------------
runTetris :: IO ()
runTetris = do
  fgen <- generateRandoms
  activityOf (startTetris fgen 20 10) manageEvent drawTetris


runCustomTetris :: IO ()
runCustomTetris = do
  rows <- getMinNum "Number of rows" 5
  cols <- getMinNum "Number of columns" 5
  fgen <- generateRandoms

  activityOf (startTetris fgen rows cols) manageEvent drawTetris


generateRandoms :: IO FigureGenerator
generateRandoms = do
  g <- getStdGen
  return $ randomRs (1, 7) g


getMinNum :: String -> Int -> IO Int
getMinNum q minN = do
  putStrLn $ printf "%s (min %d): " q minN
  xs <- getLine

  if all isDigit xs then do
    let n = read xs :: Int

    if n >= minN then
      return n

    else do
      putStrLn $ printf "\t%s is not a valid number (must be %d or highter), please try again." xs minN
      getMinNum q minN
  else do
    putStrLn $ printf "\t%s is not a natural number, please try again." xs
    getMinNum q minN
-- ----------------------------------------------------------------------------------


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
data State = Start | Normal | Pause | GameOver

data Direction = L | R
-- ----------------------------------------------------------------------------------


-- Init
-- ----------------------------------------------------------------------------------
startTetris :: FigureGenerator -> Int -> Int -> Tetris
startTetris fgen nr nc = tetris {st = Start}
  where tetris = initTetris fgen nr nc

initTetris :: FigureGenerator -> Int -> Int -> Tetris
initTetris fgen@(n:rest) nr nc = Tetris fgen f pf 0 1 1 0 Normal
    where pf = matrix nr nc (\_ -> black)
          f = generateFigure n pf

prb :: Tetris -> Int -> Int -> Tetris
prb tetris x y = Tetris (fgen tetris) (f tetris) pf' 0 1 1 0 Normal
  where pf' = matrix (nr+y) (nc+x) (\_ -> black)
        nr = nrows $ pf tetris
        nc = ncols $ pf tetris

newGame :: Tetris -> Tetris
newGame tetris = initTetris fgen' (nrows pf_) (ncols pf_)
  where fgen' = drop 3 (fgen tetris)
        pf_ = pf tetris
-- ----------------------------------------------------------------------------------


-- Events
-- ----------------------------------------------------------------------------------
manageEvent, manageStart, manageNormal, managePause, manageGameover::
  Event -> Tetris -> Tetris

manageEvent event tetris = manager event tetris
  where manager = case st tetris of
                    Start     -> manageStart
                    Normal    -> manageNormal
                    Pause     -> managePause
                    GameOver  -> manageGameover

manageStart (KeyRelease _) tetris = tetris {st = Normal}
manageStart _ tetris = tetris

manageNormal (TimePassing dt) tetris
  | clk tetris < 0  = moveDown tetris
  | otherwise       = tetris {t = (t tetris)+dt, clk = (clk tetris)-dt}
manageNormal (KeyRelease k) tetris = case k of
  "Esc" -> tetris {st = Pause}
  "C"   -> hardDrop tetris
  "Q"   -> prb tetris 3 0
  "W"   -> prb tetris (-3) 0
  "E"   -> prb tetris 0 3
  "R"   -> prb tetris 0 (-3)
  _     -> tetris
manageNormal (KeyPress k) tetris = case k of
  "Up"    -> tryRotateFigureRight tetris
  "Z"     -> tryRotateFigureLeft tetris
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
-- ----------------------------------------------------------------------------------


-- Draw
-- ----------------------------------------------------------------------------------
screenWidth, screenHeight :: Double
screenWidth = 42
screenHeight = 20

drawTetris :: Tetris -> Picture
drawTetris tetris = draw tetris & bg
  where bg = solidRectangle (screenWidth*1.5) (screenHeight*1.5)
        draw = case st tetris of
                Start     -> drawStart
                Normal    -> drawNormal
                Pause     -> drawPause
                GameOver  -> drawGameOver

-- Start
drawStart :: Tetris -> Picture
drawStart tetris = drawTextLines ls
  where ls = ["Press any key",
              "to start"]

-- Normal
drawNormal :: Tetris -> Picture
drawNormal tetris = scale.center $ ps
  where ps = pictures [ drawFigure (f tetris) pf_,
                        drawPlayfield pf_,
                        drawStats tetris]
        scale = dilated $ 0.75 * (min (screenWidth/nc') (screenHeight/nr'))
        center = translated ((-nc'-1)/2) ((-nr'-1)/2)
        nr' = fromIntegral $ nrows $ pf_
        nc' = fromIntegral $ ncols $ pf_
        pf_ = pf tetris

drawFigure :: Figure -> Playfield -> Picture
drawFigure f@(ps, ft) pf = draw ps c & draw sps (translucent c)
  where c = figuretypeColor ft
        (sps, _) = shadowFigure f pf
        draw ps c = pictures $ foldr f [] ps
          where nr' = fromIntegral $ nrows pf
                f (x,y) ac  | y <= nr'  = (drawSquare (x,y) c):ac
                            | otherwise = ac

drawPlayfield :: Playfield -> Picture
drawPlayfield pf = pictures [if c /= black then drawSquare p c else drawPoint p |
                            row <- [1..nrows pf],
                            col <- [1..ncols pf],
                            let p = (fromIntegral col, fromIntegral row),
                            let c = pf !. p]

drawSquare :: Point -> Color -> Picture
drawSquare (x, y) c = colored c (translated x y (solidRectangle 0.95 0.95))

drawPoint :: Point -> Picture
drawPoint (x, y) = colored pointColor (translated x y (solidRectangle 0.1 0.1))

-- Dado un estado tetris, obtenemos un Picture con las estadísticas que será.
-- dibujado a la izquierda o debajo del playfield, dependiendo de sus dimensiones.
drawStats:: Tetris -> Picture
drawStats tetris
  | nc' >= 2.8*nr'  = drawStatsDown pics (nc'/10)
  | otherwise       = drawStatsLeft pics (nr'/10)
    where pf_ = pf tetris
          nr' = fromIntegral $ nrows pf_
          nc' = fromIntegral $ ncols pf_
          -- convertimos stats en pictures y añadimos la representación de la siguiente figura
          pics = [drawStat t d | (t,d) <- stats] ++ [drawNextFigure $ fgen tetris]
          -- calculamos las estadísticas y añadimos la información de pausa
          stats = [ ("Score", formatScore $ sc tetris), 
                    ("Time played", formatTime $ t tetris), 
                    ("Bonus", formatBonus $ dclk tetris), 
                    ("Pause","ESC")]

-- Estas funciones se encargan de tomar los pictures generados por drawStats
-- y distribuirlos adecuadamente en base a una constante k, útil para
-- colocar un picture a una altura o anchura determinada del playfield,
-- así como redimensionarlo.
drawStatsLeft, drawStatsDown :: [Picture] -> Double -> Picture
-- Translada la lista pictures hacia la izquierda y distribuye verticalmente
drawStatsLeft stats k = pictures [move n p | (p,n) <- zip stats sep]
  where move n = (translated (-k) (n*k)) . (dilated $ 0.4*k)
        sep = [1.5, 2.5, 3.5, 6.5, 8.5]
-- Translada la lista pictures hacia abajo y distribuye horizontalmente
drawStatsDown stats k = pictures [move n p | (p,n) <- zip stats sep]
  where move n = (translated (n*k) (-k*0.6)) . (dilated $ 0.3*k)
        sep = [1, 2.5, 4, 7, 9]

-- Convierte dos Strings al formato utilizado en el texto de las estadísticas.
drawStat :: String -> String -> Picture
drawStat t d = tpic & dpic
  where tpic = translated 0 1 (dilated 0.55 (stringPic t))
        dpic = stringPic d

-- Calcula la siguiente figura a la actual y la convierte en un picture con un
-- estilo determinado. Además, centra el dibujo horizontal y verticalmente.
drawNextFigure :: FigureGenerator -> Picture
drawNextFigure fgen = colored green (translated dx dy (pictures $ map draw ps))
  where draw (x,y) = translated x y (thickRectangle 0.11 0.82 0.82)
        (ps, ft) = spawnFigure $ fst $ nextFgen fgen  -- generamos la siguiente Figure
        (dx, dy) = case ft of -- translación necesaria para centrar la figura vertical y horizontalmente
                    'I' -> (-0.5, -0.5)
                    'O' -> (-0.5, -1)
                    _   -> (0, -1)

-- Dado que, los puntos de una figura se sitúan de modo que toman como centro las coordenadas de posición
-- donde se encuentran y que sus posiciones toman valores discretos sucede que, para figuras que toman posiciones
-- pares del eje horizontal no se encuentran centradas en el (0,0), sino desplazadas 0.5 en la horizontal.
-- Esto ocasiona que, las figuras 'I' y 'O' aparezcan descentradas con respecto al resto de estadísticas.
-- La solución es restar 0.5 en la componente x de cada punto de la figura.
-- centerFigure :: Figure -> Figure
-- centerFigure (ps, ft) = (ps', ft)
--   where ps' = case ft of
--                 'I' -> map (\(x,y) -> (x-0.5, y-0.5)) ps
--                 'O' -> map (\(x,y) -> (x-0.5, y-1)) ps
--                 _   -> map (\(x,y) -> (x, y-1)) ps

-- Pause
drawPause :: Tetris -> Picture
drawPause tetris = drawTitle "PAUSED" & drawControl

drawControl :: Picture
drawControl = drawTextLines ls
  where ls = ["Esc - Resume",
              "N - New game",
              "C - Hard drop",
              "Z - Rotate left",
              "Up arrow - Rotate right",
              "Left arrow - Move left",
              "Right arrow - Move right",
              "Down arrow - Move down"]

-- GameOver
drawGameOver :: Tetris -> Picture
drawGameOver tetris = drawTitle "GAME OVER" & translated 0 (-1.5) text
  where text = drawTextLines ["Score",
                              show $ sc tetris,
                              "",
                              "Time played",
                              formatTime $ t tetris,
                              "", "",
                              "Press N to",
                              "start a new game"]

-- Generic functions
drawTitle :: String -> Picture
drawTitle title = translate.scale $ stringPic title
  where translate = translated 0 6
        scale = dilated 3

drawTextLines :: [String] -> Picture
drawTextLines ls = pictures [translated 0 y (stringPic l) | (l, y) <- zip ls [n, n-1..]]
  where n = (genericLength ls)/2 - 0.5

stringPic :: String -> Picture
stringPic = (colored green).(styledLettering Plain Monospace).pack
-- ----------------------------------------------------------------------------------


-- Color & format
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

pointColor :: Color
pointColor = bright yellow

formatScore :: Score -> String
formatScore = printf "%05d"

-- Recibe Time y retorna un String en formato mm:ss. t se incrementa cada 16.66ms en 0.01666,
-- significa esto que tras 1 segundo habrá sumado 60 veces 0.01666 -> 60*0.01666 = 1.
-- mod t 60 extrae los segundos. div t 60 extrae los minutos. 
formatTime :: Time -> String
formatTime t = printf "%02d:%02d" m s
  where (m, s) = divMod (floor t) 60 :: (Int, Int)

-- El bonus se define como la inversa del dclk. A menor dclk mayor bonus ya que se incrementa la
-- dificultad.
formatBonus :: DefaultClock -> String
formatBonus dclk = printf "x%.2f" (1/dclk)
-- ----------------------------------------------------------------------------------


-- Tetris
-- ----------------------------------------------------------------------------------
(!.) :: Playfield -> Point -> Color
pf !. (x,y) = getElem r c pf
  where r = (nrows pf) - (round y) + 1
        c = round x

setElem' :: Color -> Point -> Playfield -> Playfield
setElem' color (x,y) pf = setElem color (r,c) pf
  where r = (nrows pf) - (round y) + 1
        c = round x

-- Dada una figura y un playfield, comprueba que la figura se encuentre en una posición válida.
-- Esto es comprobar para cada uno de los puntos que forman la figura:
--  -Que no excedan al playfield a excepción de tres filas por arriba (pozo).
--  -Que no colisionen con ninguna ficha ya asentada.
validPosition :: Figure -> Playfield -> Bool
validPosition ([], _) _ = True
validPosition ((x,y):ps, ft) pf = doesNotExceed && doesNotCollide && validPosition (ps, ft) pf
  where doesNotCollide = y > nr' || pf !. (x,y) == black -- No colisiona con ficha asentada.
        doesNotExceed = x >= 1 && x <= nc' && y >= 1 -- No excede al playfield a excepción del pozo.
        nr' = fromIntegral $ nrows pf
        nc' = fromIntegral $ ncols pf

-- Dado un playfield y una figura, introduce la figura en el playfield y elimina las filas llenas.
-- Retorna una tupla con el playfield y el número de filas eliminadas.
updatePlayfield :: Playfield -> Figure -> (Playfield, Int)
updatePlayfield pf ([], _)    = removeFullRows pf -- una vez la ficha se ha situado en el playfield se eliminan las filas llenas.
updatePlayfield pf (p:ps, ft) = updatePlayfield pf' (ps, ft)
  where pf' = setElem' c p pf -- situa el color del punto p de la figura en el lugar del playfield sobre el que se encuentra.
        c = figuretypeColor ft

-- Elimina las filas llenas manteniendo las proporciones iniciales del playfield. Retorna además un número con el número de filas
-- eliminadas.
removeFullRows :: Playfield -> (Playfield, Int)
removeFullRows pf
  | null is   = (pf,0)
  | otherwise = (newRows toAdd nc <-> removeRows is pf, toAdd) -- El playfield resultante es el obtenido tras eliminar las filas
                                                            -- correspondientes y añadir arriba tantas filas en negro como se hayan borrado.
  where is = fullRows pf -- lista de las filas a eliminar
        toAdd = length is
        nc = ncols pf

-- Recibe un número de filas un número de columnas y retorna un playfield vacío de
-- dichas dimensiones.
newRows :: Int -> Int -> Playfield
newRows nr nc = matrix nr nc (\_ -> black)

-- fullRows es la función encargada de obtener una lista de índices con las filas a borrar.
fullRows :: Playfield -> [Int]
fullRows pf = map (+1) (findIndices p (toLists pf))
  where p = all (/= black) -- la fila a borrar es aquella que no tiene ninguna posición en negro.

-- Dada una lista de n índices y un playfield, obtiene el playfield resultante de eliminar dichas filas.
-- El número de filas del playfield resultante será: nr (número de filas del playfield inicial) - n.
removeRows :: [Int] -> Playfield -> Playfield
removeRows [] pf = pf
removeRows toRemove pf = fromLists [row | (i,row) <- zip [1..nr] pfList, notElem i toRemove]
  where nr = nrows pf
        pfList = toLists pf

-- moveDown es una función que dado un estado tetris retorna otro producido tras el intento de bajar en una posición
-- una ficha: Si la ficha bajada se encuentra en una posición válida -> El nuevo estado tendrá como f la ficha bajada.
-- Si la ficha bajada se encuentra en una posición no válida hay dos posibles situaciones:
--    -GameOver si la ficha no bajada se encuentra por encima del playfield.
--    -Un estado con nueva ficha y un playfield actualizado.
moveDown :: Tetris -> Tetris
moveDown tetris
  | validPosition mf pf_  = tetris {f = mf, clk = dclk tetris} -- clk se reinicia a dclk para que tras un tiempo marcado por dclk la ficha baje automáticamente.
  | isGameOver f_ pf_     = tetris {st = GameOver}
  | otherwise             = placeFigure tetris
  where mf = moveFigure f_ 0 (-1) -- figura movida una posición hacia abajo.
        pf_ = pf tetris
        f_ = f tetris

-- Dado un estado tetris, retorna otro con la ficha bajada a la posición más baja posible.
-- También se debe contemplar si la ficha tras su bajada se encuentra por encima del playfield (GameOver).
hardDrop :: Tetris -> Tetris
hardDrop tetris
  | isGameOver f' pf_ = tetris {st = GameOver}
  | otherwise         = placeFigure $ tetris {f = f'}
  where f' = shadowFigure (f tetris) pf_
        pf_ = pf tetris

-- Dado un estado tetris crea otro con una nueva figura, el playfield actualizado con la figura actual, dclk actualizado,
-- clk reiniciado y una actualización sobre los puntos.
placeFigure :: Tetris -> Tetris
placeFigure tetris = tetris {fgen = fgen', f = nf, pf = pf', dclk = dclk', clk = dclk', sc = sc'}
  where pf_ = pf tetris
        dclk_ = dclk tetris
        (nf, fgen') = nextFigure (fgen tetris) pf_
        (pf', delRows) = updatePlayfield pf_ (f tetris)
        dclk' = max 0.15 (dclk_-0.01) -- el dclk se actualiza tras cada ficha asentada, para acelerar el juego.
        sc' = (sc tetris) + (round $ n * 100 * (1/dclk_)) -- a menor dclk mayor cantidad de puntos obtenidos tras borrar n filas.
          where n = fromIntegral $ delRows

-- Dada una figura y un playfield informa si estamos en una situación de GameOver.
-- El GameOver se produce si algunos de los puntos de la figura está por encima del playfield.
isGameOver :: Figure -> Playfield -> Bool
isGameOver (ps, _) pf = any (>ceil) (map snd ps)
  where ceil = fromIntegral $ nrows pf

-- Retorna un estado con la figura movida a la izquierda en caso de poder, sino puede mantiene su posición actual.
moveLeft :: Tetris -> Tetris
moveLeft tetris
  | validPosition mf (pf tetris) = tetris {f = mf}
  | otherwise = tetris
  where mf = moveFigure (f tetris) (-1) 0

-- Retorna un estado con la figura movida a la derecha, en caso de no poder la mantiene en su posición actual.
moveRight :: Tetris -> Tetris
moveRight tetris
  | validPosition mf (pf tetris) = tetris {f = mf}
  | otherwise = tetris
  where mf = moveFigure (f tetris) 1 0

-- Dada una figura y dos números de tipo Double que llamaremos dx y dy se obtiene la figura resultante de sumar a la
-- posición x e y de cada punto que forma la figura dx y dy respectivamente.
moveFigure :: Figure -> Double -> Double -> Figure
moveFigure (ps, ft) dx dy = (move ps, ft)
  where move [] = []
        move ((x,y):r) = (x+dx, y+dy):(move r)
-- ----------------------------------------------------------------------------------


-- Figure
-- ----------------------------------------------------------------------------------
nextFigure :: FigureGenerator -> Playfield -> (Figure, FigureGenerator)
nextFigure fgen pf = (generateFigure n pf, fgen')
  where (n, fgen') = nextFgen fgen

nextFgen :: FigureGenerator -> (Int, FigureGenerator)
nextFgen (current:next:next2:rest)
  | current /= next = (next, next:rest)
  | otherwise       = (next2, next2:rest)

generateFigure :: Int -> Playfield -> Figure
generateFigure n pf = (ps', ft)
  where ps' = map (\(x, y) -> (x+dx, y+dy)) ps
        (ps, ft) = spawnFigure n
        dx  | even nc                 = nc'/2
            | ft == 'I' || ft == 'O'  = nc'/2 - 0.5
            | otherwise               = nc'/2 + 0.5
        dy = nr' + 1
        nc = ncols pf
        nc' = fromIntegral nc
        nr' = fromIntegral $ nrows pf

spawnFigure :: Int -> Figure
spawnFigure n = case n of
  1 -> ([(0,1),   (1,1),    (0,2),    (1,2)],   'O')
  2 -> ([(-1,1),  (0,1),    (1,1),    (2,1)],   'I')
  3 -> ([(0,1),   (-1,1),   (1,1),    (1,2)],   'L')
  4 -> ([(0,1),   (-1,1),   (1,1),    (-1,2)],  'J')
  5 -> ([(0,1),   (-1,1),   (0,2),    (1,2)],   'S')
  6 -> ([(0,1),   (1,1),    (-1,2),   (0,2)],   'Z')
  7 -> ([(0,1),   (-1,1),   (1,1),    (0,2)],   'T')

tryRotateFigureRight, tryRotateFigureLeft :: Tetris -> Tetris
tryRotateFigureRight tetris = tryRotateFigure tetris R
tryRotateFigureLeft tetris = tryRotateFigure tetris L

tryRotateFigure :: Tetris -> Direction -> Tetris
tryRotateFigure tetris dir = case maybef' of
  Nothing -> tetris
  Just f' -> tetris {f = f'}
  where maybef' = safeHead $ filter (\f -> validPosition f (pf tetris)) (map ($rf) mvs)
        rf = rotateFigure (f tetris) dir
        mvs = [ \x -> moveFigure x 0    0,
                \x -> moveFigure x 1    0,
                \x -> moveFigure x (-1) 0,
                \x -> moveFigure x 2    0,
                \x -> moveFigure x (-2) 0]

rotateFigure :: Figure -> Direction -> Figure
rotateFigure (ps, ft) dir = case ft of
  'O' -> (ps, ft)
  'I' -> (ps', ft)
    where ps' = rotatePoints center ps dir
          [_,(x1,y1),(x2,y2),_] = ps
          center  | x1 < x2 = ((x1+x2)/2, y1-0.5)
                  | x1 > x2 = ((x1+x2)/2, y1+0.5)
                  | y1 > y2 = (x1-0.5, (y1+y2)/2)
                  | y1 < y2 = (x1+0.5, (y1+y2)/2)
  ft   -> (ps', ft)
    where ps' = center:(rotatePoints center rest dir)
          (center:rest) = ps

rotatePoints :: Point -> [Point] -> Direction -> [Point]
rotatePoints center ps dir = map (rotate center) ps
  where rotate (xo,yo) (xi,yi) = case dir of
          R -> (yi-yo+xo, -(xi-xo)+yo)
          L -> (-(yi-yo)+xo, xi-xo+yo)

-- Dada una figura y un playfield, retorna otra figura con la posición más baja posible manteniendo la misma posición
-- horizontal de la original. El cálculo se basa en calcular las distancias de cada punto de la figura, con el elemento
-- más alto del playfield, tomar la menor y restársela a la componente y de cada punto.
shadowFigure :: Figure -> Playfield -> Figure
shadowFigure (ps,t) pf = (sps,t)
  where sps = map (\(x,y) -> (x, y-yDif+1)) ps
        yDif = minimum [y - maxNotEmptyRow |
                        (x,y) <- ps,
                        let ys2 = [ y2 |
                                    y2 <- [y, y-1..1],
                                    y2 <= nr',
                                    pf !. (x,y2) /= black],
                        let maxNotEmptyRow = if null ys2 then 0 else head ys2] -- si no hay ningún punto asentado debajo del punto (x,y) de la ficha,
                                                                               -- maxNotEmptyRow estará a 0.
        nr' = fromIntegral $ nrows pf
-- ----------------------------------------------------------------------------------


-- Utils
-- ----------------------------------------------------------------------------------
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead l = Just $ head l
-- ----------------------------------------------------------------------------------
