# Memoria de trabajo: TetrisHS



## Autores
| Nombre    | Apellidos             | UVUS      | Correo                    |
| ---------:|-----------------------|-----------|---------------------------|
| Alan      | Trawczynski           | alatra    | alantrawcz@gmail.com      |
| Enrique   | Fernández Corrales    | enrfercor | enrique.fdezco@gmail.com  |



## Temática



## Estructura
Todo el código se encuentra en un único archivo, el módulo Tetris, que exporta las funciones *runTetris* y *runCustomTetris*. Podemos diferenciar ocho partes:

1. **IO**: es el punto de partida para interactuar con el módulo, define las 2 funciones que exporta, así como otras funciones tipo *IO* utilizadas internamente.  
    ```
    runTetris :: IO ()
    runCustomTetris :: IO ()
    generateRandoms :: IO FigureGenerator
    getMinNum:: String -> Int -> IO Int
    ```
2. **Types**: define todos los tipos de datos que se utilizan posteriormente.  
    ```
    data Tetris = Tetris {...}
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
    ```
3. **Init**: define las funciones necesarias para iniciar el juego, tanto por primera vez como para las partidas posteriores. 
    ```
    startTetris :: FigureGenerator -> Int -> Int -> Tetris
    initTetris :: FigureGenerator -> Int -> Int -> Tetris
    newGame:: Tetris -> Tetris
    ```
4. **Events**: define los manejadores de eventos utilizados por *CodeWorld* que modificarán el estado de *Tetris* y permitirán el avance del juego.
    ```
    manageEvent :: Event -> Tetris -> Tetris
    manageStart :: Event -> Tetris -> Tetris
    manageNormal :: Event -> Tetris -> Tetris
    managePause :: Event -> Tetris -> Tetris
    manageGameover :: Event -> Tetris -> Tetris
    ```
5. **Draw**: define las funciones de dibujado utilizados por *CodeWorld* encargadas de representar gráficamente el estado de *Tetris*.
    ```
    screenWidth, screenHeight :: Double
    drawTetris :: Tetris -> Picture
    drawStart :: Tetris -> Picture
    drawNormal :: Tetris -> Picture
    drawFigure :: Figure -> Playfield -> Picture
    drawPlayfield :: Playfield -> Picture
    drawSquare :: Point -> Color -> Picture
    drawPoint :: Point -> Picture
    drawStats :: Tetris -> Picture
    drawNextFigure :: Figure -> Picture
    centerAxis :: Figure -> Figure
    drawPause :: Tetris -> Picture
    drawControl:: Picture
    drawGameOver :: Tetris -> Picture
    drawTitle :: String -> Picture
    drawTextLines:: [String] -> Picture
    stringPic:: String -> Picture
    ```
6. **Color & format**: define funciones que definen el color y formato utilizado en el dibuajdo.
    ```
    figuretypeColor :: FigureType -> Color
    pointColor:: Color
    formatScore:: Score -> String
    formatTime:: Time -> String
    formatBonus:: DefaultClock -> String
    ```
7. **Tetris**: define funciones y operadores encargados de interactuar con los diferentes atributos que conforman *Tetris*. Modifica los estados del juego, procesa los movimientos de la figura actual, elimina filas llenas, comprueba si el juego ha acabado, etc.
    ```
    (!.) :: Playfield -> Point -> Color
    setElem' :: Color -> Point -> Playfield -> Playfield
    validPosition :: Figure -> Playfield -> Bool
    updatePlayfield :: Playfield -> Figure -> (Playfield, [Int])
    removeFullRows :: Playfield -> (Playfield, [Int])
    newRows :: Int -> Int -> Playfield
    fullRows :: Playfield -> [Int]
    removeRows :: [Int] -> Playfield -> Playfield
    moveDown :: Tetris -> Tetris
    instantDown :: Tetris -> Tetris
    placeFigure :: Tetris -> Tetris
    isGameOver:: Figure -> Playfield -> Bool
    moveLeft :: Tetris -> Tetris
    moveRight :: Tetris -> Tetris
    moveFigure :: Figure -> Double -> Double -> Figure
    ```
8. **Figure**: define las funciones encargadas de la generación y rotación de figuras, así como el cálculo de las sombra de una figura.
    ```
    nextFigure :: FigureGenerator -> Playfield -> (Figure, FigureGenerator)
    nextFgen :: FigureGenerator -> (Int, FigureGenerator)
    generateFigure :: Int -> Playfield -> Figure
    spawnFigure :: Int -> Figure
    tryRotateFigureRight, tryRotateFigureLeft :: Tetris -> Tetris
    tryRotateFigure :: Tetris -> Direction -> Tetris
    rotateFigure :: Figure -> Direction -> Figure
    rotatePoints :: Point -> [Point] -> Direction -> [Point]
    obtainMaxDown :: Figure -> Playfield -> Figure
    ```



## Requisitos
### Funciones de Prelude
1. ...  
    `code`
2. ...  
    `code`

### Funciones de Data.List

### Funciones recursivas

### Funciones por patrones

### Funciones con guardas

### Funciones con case of

### Funciones con listas por comprensión

### Funciones con orden superior

### Funciones con evaluación perezosa

### ...

### Módulo

### Tipos de datos nuevos

### Datos abstractos

### Librerías



## Compilación



## Uso



## Librerías
