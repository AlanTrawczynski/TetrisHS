{-# LANGUAGE OverloadedStrings #-}
module Tetris
( runTetris                     -- IO ()
, runCustomTetris               -- Int -> Int -> IO ()
, runCustomTetrisInteractive    -- IO ()
) where

import Data.List (genericLength, findIndices)
import System.Random (getStdGen, randomRs)
import Text.Printf (printf)
import Data.Char (isDigit)
import Data.Text (pack)
import Data.Matrix
import CodeWorld


-- IO
-- ----------------------------------------------------------------------------------
-- Inicia la versión por defecto del tetris con dimensiones 20 filas x 10 columnas
runTetris :: IO ()
runTetris = do
  fgen <- generateRandoms
  let tetris = startTetris fgen 20 10

  activityOf tetris manageEvent drawTetris

-- Inicia una versión personalizada del tetris, utilizando los parámetros de entrada
-- para definir las dimensiones
runCustomTetris :: Int -> Int -> IO ()
runCustomTetris r c = do
  fgen <- generateRandoms
  let tetris = startTetris fgen (max r 5) (max c 5)

  activityOf tetris manageEvent drawTetris

-- Inicia una versión personalizada del tetris, pregunta por las dimensiones con
-- las que se quiere jugar de forma interactiva
runCustomTetrisInteractive :: IO ()
runCustomTetrisInteractive = do
  rows <- getMinNum "Number of rows" 5
  cols <- getMinNum "Number of columns" 5
  fgen <- generateRandoms
  let tetris = startTetris fgen rows cols

  activityOf tetris manageEvent drawTetris

-- Genera la lista infinita de números necesaria para la generación aleatoria de figuras
generateRandoms :: IO FigureGenerator
generateRandoms = do
  g <- getStdGen
  return $ randomRs (1, 7) g

-- Pregunta por un número natural >minN, utilizando q en el print de la pregunta
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
-- Tipo de dato utilizado para representar las diferentes atributos utilizados en el juego
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

-- Tipos sinónimos utilizados por Tetris y en la declaración de funciones
type FigureGenerator = [Int]          -- Es una lista infinita de números
type Figure = ([Point], FigureType)   -- Representa una figura
type FigureType = Char                -- Representa el tipo de una figura
type Playfield = Matrix Color         -- Representa el área de juego
type Time = Double                    -- Representa el tiempo total de juego
type DefaultClock = Double            -- Valor al que se reinicia Clock una vez que su tiempo se ha agotado
type Clock = Double                   -- Contador de tiempo utilizado para bajar la ficha tras un tiempo determinado
type Score = Int                      -- Representa la puntuación
data State =    -- Representa los diferentes estados del juego:
    Start         -- Estado incial, debe de pulsarse cualquier botón para inciar el juego
  | Normal        -- Estado en el que se desarrolla el juego
  | Pause         -- Estado de pausa
  | GameOver      -- Estado intermediario entre el final de un juego y el comienzo del siguiente

-- Tipo de dato utilizado para representar la dirección de rotación
data Direction = L | R
-- ----------------------------------------------------------------------------------


-- Init
-- ----------------------------------------------------------------------------------
-- Función utilizada únicamente en el primer inicio del juego, utiliza initTetris
-- asegurándose de que exista una pantalla inicial que impida comenzar el juego
-- hasta que no se pulse algun botón
startTetris :: FigureGenerator -> Int -> Int -> Tetris
startTetris fgen nr nc = tetris {st = Start}
  where tetris = initTetris fgen nr nc

-- Dado un FigureGenerator y dos números enteros que representan el número de filas y
-- columnas que va a tener el área de juego, inicializa Tetris
initTetris :: FigureGenerator -> Int -> Int -> Tetris
initTetris fgen@(n:rest) nr nc = Tetris fgen f pf 0 1 1 0 Normal
    where pf = matrix nr nc (\_ -> black)   -- el color negro indica posiciones vacías
          f = generateFigure n pf           -- generamos la primera figura con la que jugar

-- Dada una partida del juego inicializa una nueva, reutilizando el FigureGenerator
-- de la anterior. Se utiliza para iniciar una nueva partida desde el menú de pausa
-- o GameOver
newGame :: Tetris -> Tetris
newGame tetris = initTetris fgen' (nrows pf_) (ncols pf_)
  where fgen' = drop 3 (fgen tetris)    -- 3: mínimo número a eliminar para evitar repetición de figuras entre partidas
        pf_ = pf tetris
-- ----------------------------------------------------------------------------------


-- Events
-- ----------------------------------------------------------------------------------
manageEvent, manageStart, manageNormal, managePause, manageGameover::
  Event -> Tetris -> Tetris

-- Manejador de eventos utilizado por CodeWorld, se encarga de delegar el trabajo a
-- un sub-manejador dependiendo del estado en el que se encuentre el juego
manageEvent event tetris = manager event tetris
  where manager = case st tetris of
                    Start     -> manageStart
                    Normal    -> manageNormal
                    Pause     -> managePause
                    GameOver  -> manageGameover

-- Manejador de la pantalla de Start
manageStart (KeyRelease _) tetris = tetris {st = Normal}  -- El juego comienza al pulsarle cualquier tecla
manageStart _ tetris = tetris

-- Manejador de la pantalla principal, donde se juega
manageNormal (TimePassing dt) tetris
  | clk tetris < 0  = moveDown tetris                                     -- Cuando el contador clk llegue a 0 debemos de mover la figura actual hacia abajo
  | otherwise       = tetris {t = (t tetris)+dt, clk = (clk tetris)-dt}   -- Si el contador clk no es 0, lo actualizamos junto al tiempo total
manageNormal (KeyRelease k) tetris = case k of
  "Esc" -> tetris {st = Pause}  -- Paso a la pantalla de pausa
  "C"   -> hardDrop tetris      -- Mover la ficha hacia abajo todo lo posible de una vez
  _     -> tetris
manageNormal (KeyPress k) tetris = case k of
  "Up"    -> tryRotateFigure tetris R     -- Rotación de la figura en sentido horario
  "Z"     -> tryRotateFigure tetris L     -- Rotación de la figura en sentido antihorario
  "Down"  -> moveDown tetris              -- Mover la figura una fila hacia abajo manualmente
  "Left"  -> moveLeft tetris              -- Mover la figura una columna hacia la izquierda
  "Right" -> moveRight tetris             -- Mover la figura una columna hacia la derecha
  _       -> tetris
manageNormal _ tetris = tetris

-- Manejador de la pantalla de pausa
managePause (KeyRelease "Esc") tetris = tetris {st = Normal}  -- Volver a la pantalla de juego
managePause (KeyRelease "N") tetris = newGame tetris          -- Comenzar un juego nuevo
managePause _ tetris = tetris

-- Manejador de la pantalla de GameOver
manageGameover (KeyRelease "N") tetris = newGame tetris   -- Comenzar un juego nuevo
manageGameover _ tetris = tetris
-- ----------------------------------------------------------------------------------


-- Draw
-- ----------------------------------------------------------------------------------
-- Dimensiones aproximadas de una pantalla 16:9 en navegador, utilizados
-- para el escalado de pictures
screenWidth, screenHeight :: Double
screenWidth = 42
screenHeight = 20

-- Delegamos el trabajo del dibujado a sub-dibujadores dependiendo del estado de juego
drawTetris :: Tetris -> Picture
drawTetris tetris = draw tetris & bg
  where bg = solidRectangle (screenWidth*1.5) (screenHeight*1.5)  -- Fondo negro
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
  where ps = pictures [ drawFigure (f tetris) pf_,  -- dibujamos la figura actual
                        drawPlayfield pf_,          -- dibujamos el área de juego
                        drawStats tetris]           -- dibujamos las estadísticas
        scale = dilated $ 0.75 * (min (screenWidth/nc') (screenHeight/nr'))   -- escalamos todo el dibujo para que quepa en pantalla
        center = translated ((-nc'-1)/2) ((-nr'-1)/2)   -- centramos el dibujo
        nr' = fromIntegral $ nrows $ pf_
        nc' = fromIntegral $ ncols $ pf_
        pf_ = pf tetris

-- Función encargada de dibujar la figura actual y su sombra
drawFigure :: Figure -> Playfield -> Picture
drawFigure f@(ps, ft) pf = draw ps c & draw sps (translucent c)
  where c = figuretypeColor ft    -- color de la figura
        (sps, _) = shadowFigure f pf  -- puntos de la sombra de la figura
        draw ps c = pictures $ foldr f [] ps    -- función para dibujar únicamente los puntos de la figura que se encuentren dentro del playfield
          where nr' = fromIntegral $ nrows pf
                f (x,y) ac  | y <= nr'  = (drawSquare (x,y) c):ac
                            | otherwise = ac

-- Función encargada de dibujar el área de juego. Itera la matriz de colores,
-- dibujando cuadrados del color encontrado por cada posición; en caso de encontrar
-- color negro, dibuja un pequeño punto.
drawPlayfield :: Playfield -> Picture
drawPlayfield pf = pictures [if c /= black then drawSquare p c else drawPoint p |
                            row <- [1..nrows pf],
                            col <- [1..ncols pf],
                            let p = (fromIntegral col, fromIntegral row),
                            let c = pf !. p]

-- Función encargada de dibujar los cuadrados que forman las figuras y que pueblan el área de juego
drawSquare :: Point -> Color -> Picture
drawSquare (x, y) c = colored c (translated x y (solidRectangle 0.95 0.95))

-- Función encargada de dibujar los puntos del área de juego para marcar las filas y columnas
drawPoint :: Point -> Picture
drawPoint (x, y) = colored pointColor (translated x y (solidRectangle 0.1 0.1))

-- Dado un estado tetris, obtenemos un Picture con las estadísticas que será
-- dibujado a la izquierda o debajo del playfield, dependiendo de sus dimensiones.
drawStats:: Tetris -> Picture
drawStats tetris
  | nc' >= 2.8*nr'  = drawStatsDown pics (nc'/10)     -- Si el playfield consta de 2.8 veces más columnas que filas, dibujamos los stats debajo
  | otherwise       = drawStatsLeft pics (nr'/10)     -- En caso contrario, dibujamos los stats a la izquierda
    where pf_ = pf tetris
          nr' = fromIntegral $ nrows pf_
          nc' = fromIntegral $ ncols pf_
          pics = [drawStat t d | (t,d) <- stats] ++ [drawNextFigure $ fgen tetris]  -- convertimos stats en pictures y añadimos la representación de la siguiente figura
          stats = [ ("Score", formatScore $ sc tetris),       -- calculamos las estadísticas y añadimos la información de pausa
                    ("Time played", formatTime $ t tetris),
                    ("Bonus", formatBonus $ dclk tetris),
                    ("Pause","ESC")]

-- Estas funciones se encargan de tomar los pictures generados por drawStats
-- y distribuirlos adecuadamente en base a una constante k, útil para
-- colocar un picture a una altura o anchura determinada del playfield,
-- así como redimensionarlo.
drawStatsLeft, drawStatsDown :: [Picture] -> Double -> Picture
-- Translada la lista pictures hacia la izquierda y distribuye verticalmente
drawStatsLeft stats k = pictures [move n (scale p) | (p,n) <- zip stats sep]
  where move n = translated (-k) (n*k)
        scale = dilated $ 0.4*k
        sep = [1.5, 2.5, 3.5, 6.5, 8.5]

-- Translada la lista pictures hacia abajo y distribuye horizontalmente
drawStatsDown stats k = pictures [move n (scale p) | (p,n) <- zip stats sep]
  where move n = translated (n*k) (-k*0.6)
        scale = dilated $ 0.3*k
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

-- Pause
drawPause :: Tetris -> Picture
drawPause tetris = drawTitle "PAUSED" & drawControl

-- Picture que contiene la información de los controles del juego
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
  where text = drawTextLines ["Score",                -- estadísticas a mostrar en la pantalla GameOver
                              show $ sc tetris,
                              "",
                              "Time played",
                              formatTime $ t tetris,
                              "", "",
                              "Press N to",
                              "start a new game"]

-- Generic functions
-- Genera un picture a partir de un String, que nos sirve como título en las pantallas secundarias
drawTitle :: String -> Picture
drawTitle title = translate.scale $ stringPic title
  where translate = translated 0 6
        scale = dilated 3

-- Genera un picture del texto representado por una lista de Strings, donde cada elemento
-- se interpreta como una línea.
drawTextLines :: [String] -> Picture
drawTextLines ls = pictures [translated 0 y (stringPic l) | (l, y) <- zip ls [n, n-1..]]
  where n = (genericLength ls)/2 - 0.5

-- Dado un string genera un picture de este con un formato determinado
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
  'J' -> dark blue
  'S' -> red
  'Z' -> green
  'T' -> purple

pointColor :: Color
pointColor = bright yellow

formatScore :: Score -> String
formatScore = printf "%05d"

-- Recibe Time y retorna un String en formato mm:ss. t se incrementa cada 16.66ms en 0.01666,
-- de forma que tras 1 segundo habrá sumado 60 veces 0.01666 -> 60*0.01666 = 1.
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
-- Operador alternativo a (!) de Data.Matrix para abstraernos en el acceso a la matriz que representa
-- el Playfield. Nos permite considerar que la primera fila y columna de la matriz comienza en la esquina
-- izquierda inferior y no izquierda superior:
--      Data.Matrix             Como queremos acceder
--    (1,1) ... (1,c)               (f,1) ... (f,c)
--      |         |       --->        |         |
--    (f,1) ... (f,c)               (1,1) ... (1,c)
(!.) :: Playfield -> Point -> Color
pf !. (x,y) = getElem r c pf
  where r = (nrows pf) - (round y) + 1
        c = round x

-- Función alternativa a setElem de Data.Matrix. Se aplica la misma idea que en el operador (!.),
-- pero en este caso para abstraernos en la modificación de elementos de la matriz.
setElem' :: Color -> Point -> Playfield -> Playfield
setElem' color (x,y) pf = setElem color (r,c) pf
  where r = (nrows pf) - (round y) + 1
        c = round x

-- Dada una figura y un playfield, comprueba que la figura se encuentre en una posición válida.
-- Esto es, comprobar para cada uno de los puntos que forman la figura:
--  - Que no excedan al playfield a excepción de tres filas por arriba.
--  - Que no colisionen con ninguna ficha ya asentada.
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

-- Dado un playfield elimina las filas llenas, añadiendo una fila vacía en la parte superior por cada una de las borradas.
removeFullRows :: Playfield -> (Playfield, Int)
removeFullRows pf
  | null is   = (pf, 0)
  | otherwise = (newRows toAdd nc <-> removeRows is pf, toAdd)  -- Unión vertical de las filas vacías y la matriz resultante de eliminar las filas llenas
  where is = fullRows pf -- lista de las filas a eliminar
        toAdd = length is
        nc = ncols pf

-- Recibe un número de filas, un número de columnas y retorna un playfield vacío de
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

-- Función que dado un estado tetris retorna otro producido tras el intento de bajar una fila la ficha actual:
--  - Si la ficha bajada se encuentra en una posición válida, pasará a ser la ficha actual.
--  - En caso contrario hay dos posibles situaciones:
--    - GameOver si la ficha no bajada se encuentra por encima del playfield.
--    - Un estado con nueva ficha y un playfield actualizado, habiéndose asentado la ficha que se intentó mover.
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
  where f' = shadowFigure (f tetris) pf_  -- sombra de la figura (posición más baja)
        pf_ = pf tetris

-- Dado un estado tetris crea otro con una nueva figura, el playfield actualizado con la figura actual, dclk actualizado,
-- clk reiniciado y una actualización sobre los puntos.
placeFigure :: Tetris -> Tetris
placeFigure tetris = tetris {fgen = fgen', f = nf, pf = pf', dclk = dclk', clk = dclk', sc = sc'}
  where pf_ = pf tetris
        dclk_ = dclk tetris
        (nf, fgen') = nextFigure (fgen tetris) pf_  -- generamos la que será la nueva ficha
        (pf', delRows) = updatePlayfield pf_ (f tetris)   -- actualizamos el playfield asentando la que era la ficha actual
        dclk' = max 0.15 (dclk_-0.01) -- el dclk disminuye tras cada ficha asentada, para acelerar el juego.
        sc' = (sc tetris) + (round $ n * 100 * (1/dclk_)) -- a menor dclk mayor cantidad de puntos obtenidos tras borrar n filas.
          where n = fromIntegral $ delRows

-- Dada una figura y un playfield informa si estamos en una situación de GameOver.
-- El GameOver se produce si algunos de los puntos de la figura está por encima del playfield.
isGameOver :: Figure -> Playfield -> Bool
isGameOver (ps, _) pf = any (>ceil) (map snd ps)
  where ceil = fromIntegral $ nrows pf

-- Retorna un estado con la figura movida una columna a la izquierda, en caso de que la ficha movida no esté en una
-- posición válida (sobresale por la izquierda del playfield) la mantiene en su posición inicial.
moveLeft :: Tetris -> Tetris
moveLeft tetris
  | validPosition mf (pf tetris) = tetris {f = mf}
  | otherwise = tetris
  where mf = moveFigure (f tetris) (-1) 0

-- Retorna un estado con la figura movida una columna a la derecha, en caso de que la ficha movida no esté en una
-- posición válida (sobresale por la derecha del playfield) la mantiene en su posición inicial.
moveRight :: Tetris -> Tetris
moveRight tetris
  | validPosition mf (pf tetris) = tetris {f = mf}
  | otherwise = tetris
  where mf = moveFigure (f tetris) 1 0

-- Dada una figura y dos números de tipo Double (dx, dy) se obtiene la figura resultante de modificar cada uno de
-- sus puntos p tal que: p@(x,y) = (x+dx, y+dy)
moveFigure :: Figure -> Double -> Double -> Figure
moveFigure (ps, ft) dx dy = (move ps, ft)
  where move [] = []
        move ((x,y):r) = (x+dx, y+dy):(move r)
-- ----------------------------------------------------------------------------------


-- Figure
-- ----------------------------------------------------------------------------------
-- Función que devuelve la siguiente figura con la que se jugará, junto al FigureGenerator
-- actualizado (para que en las próximas iteraciones de generen figuras diferentes).
nextFigure :: FigureGenerator -> Playfield -> (Figure, FigureGenerator)
nextFigure fgen pf = (generateFigure n pf, fgen')
  where (n, fgen') = nextFgen fgen

-- Función que dado un FigureGenerator devuelve el identificador (Int) de la siguiente
-- figura que debe de generarse, junto al FigureGenerator actualizado.
-- Para evitar la repetición de figuras compara el identificador de la siguiente figura (n+1)
-- con el de la actual (n): en caso de ser iguales toma el identificador n+2.
nextFgen :: FigureGenerator -> (Int, FigureGenerator)
nextFgen (current:next:next2:rest)
  | current /= next = (next, next:rest)
  | otherwise       = (next2, next2:rest)

-- Función que dado el identificador de una figura la genera y, dado el playfield, la
-- translada para que quede centrada y 2 filas por encima del área de juego.
generateFigure :: Int -> Playfield -> Figure
generateFigure n pf = (ps', ft)
  where ps' = map (\(x, y) -> (x+dx, y+dy)) ps
        (ps, ft) = spawnFigure n  -- generamos la figura
        dx  -- movimiento en columnas para centrar la figura horizontalmente en el playfield (depende del número de columnas y el tipo de figura)
          | even nc                 = nc'/2
          | ft == 'I' || ft == 'O'  = nc'/2 - 0.5
          | otherwise               = nc'/2 + 0.5
        dy = nr' + 1    -- movimiento en filas necesario para que la figura quede por encima del playfield
        nc = ncols pf
        nc' = fromIntegral nc
        nr' = fromIntegral $ nrows pf

-- Función encargada de generar los diferentes tipos de figuras. Se generan en la fila 1,
-- una columna hacia la izquierda en caso de no poder centrarse horizontalmente.
spawnFigure :: Int -> Figure
spawnFigure n = case n of
  1 -> ([(0,1),   (1,1),    (0,2),    (1,2)],   'O')
  2 -> ([(-1,1),  (0,1),    (1,1),    (2,1)],   'I')
  3 -> ([(0,1),   (-1,1),   (1,1),    (1,2)],   'L')
  4 -> ([(0,1),   (-1,1),   (1,1),    (-1,2)],  'J')
  5 -> ([(0,1),   (-1,1),   (0,2),    (1,2)],   'S')
  6 -> ([(0,1),   (1,1),    (-1,2),   (0,2)],   'Z')
  7 -> ([(0,1),   (-1,1),   (1,1),    (0,2)],   'T')

-- Función encargada de realizar la rotación de la figura actual.
-- El "try" se debe a que cabe la posibilidad de que la rotación no pueda realizarse.
-- Es común que al realizar una rotación la figura choque con las paredes del playfield o
-- con otras piezas asentadas en el área de juego: para minimizar este problema se realizan
-- varios intentos de desplazamiento de la figura rotada.
tryRotateFigure :: Tetris -> Direction -> Tetris
tryRotateFigure tetris dir = case maybef' of
  Nothing -> tetris             -- Si niguno de los desplazamientos es válido no podemos rotar la figura
  Just f' -> tetris {f = f'}    -- Si alguno de los desplazamientos ha sido válido actualizamos la figura
  where maybef' = safeHead $ filter (\f -> validPosition f (pf tetris)) (map ($rf) mvs) -- Tomamos la primera figura rotada y desplazada cuya posición sea válida
        rf = rotateFigure (f tetris) dir    -- Rotamos la figura actual en el sentido que se nos indica
        mvs = [ \x -> moveFigure x 0    0,  -- Figura rotada sin desplazar
                \x -> moveFigure x 1    0,  -- Figura rotada y desplazada 1 columna hacia la derecha
                \x -> moveFigure x (-1) 0,  -- Figura rotada y desplazada 1 columna hacia la izquierda
                \x -> moveFigure x 2    0,  -- Figura rotada y desplazada 2 columnas hacia la derecha
                \x -> moveFigure x (-2) 0]  -- Figura rotada y desplazada 2 columnas hacia la izquierda

-- Dada una figura y un sentido de rotación, devuelve la figura rotada según el sistema SRS.
rotateFigure :: Figure -> Direction -> Figure
rotateFigure (ps, ft) dir = case ft of
  'O' -> (ps, ft)     -- La figura O es igual independientemente de la rotación
  'I' -> (ps', ft)    -- Para rotar la figura I debemos de hallar su centro, ya que este cambia en cada rotación
    where ps' = rotatePoints center ps dir
          [_,(x1,y1),(x2,y2),_] = ps
          center  | x1 < x2 = ((x1+x2)/2, y1-0.5)
                  | x1 > x2 = ((x1+x2)/2, y1+0.5)
                  | y1 > y2 = (x1-0.5, (y1+y2)/2)
                  | y1 < y2 = (x1+0.5, (y1+y2)/2)
  ft   -> (ps', ft)   -- Para el resto de figuras rotaremos todos sus puntos menos el primero, que será tomado como centro de rotación
    where ps' = center:(rotatePoints center rest dir)
          (center:rest) = ps

-- Dado un centro de rotación, una lista de puntos y una sentido de rotación, devuelve la
-- lista de puntos rotados.
rotatePoints :: Point -> [Point] -> Direction -> [Point]
rotatePoints center ps dir = map (rotate center) ps
  where rotate (xo,yo) (xi,yi) = case dir of
          R -> (yi-yo+xo, -(xi-xo)+yo)
          L -> (-(yi-yo)+xo, xi-xo+yo)

-- Dada una figura y un playfield, retorna otra figura con la posición más baja posible, manteniendo la misma posición
-- horizontal de la original. El cálculo se basa en calcular las distancias de cada punto de la figura, con el elemento
-- más alto del playfield, tomar la menor y restársela a la componente (y) de cada punto.
shadowFigure :: Figure -> Playfield -> Figure
shadowFigure (ps,t) pf = (sps,t)
  where sps = map (\(x,y) -> (x, y-yDif+1)) ps
        yDif = minimum [y - maxNotEmptyRow |
                        (x,y) <- ps,
                        let ys2 = [ y2 |
                                    y2 <- [y, y-1..1],
                                    y2 <= nr',
                                    pf !. (x,y2) /= black],
                        let maxNotEmptyRow = if null ys2 then 0 else head ys2] -- si no hay ningún punto asentado debajo del punto (x,y) de la ficha, la fila más alta con elemento no vacío será 0.
        nr' = fromIntegral $ nrows pf
-- ----------------------------------------------------------------------------------


-- Utils
-- ----------------------------------------------------------------------------------
safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead l = Just $ head l
-- ----------------------------------------------------------------------------------
