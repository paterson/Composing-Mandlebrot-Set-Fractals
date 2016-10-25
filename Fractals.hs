type Point = (Float, Float)
type Fractal = Point -> [Point]
type Image color = Point -> color
type Grid a = [[a]]

next :: Point -> Point -> Point
next (u, v) (x, y) = (x * x - y * y + u, 2 * x * y + v)

generatedSequence :: Fractal
generatedSequence p = iterate (next p) (0, 0)

fairlyClose :: Point -> Bool
fairlyClose (u,v) = (u * u + v * v) < 100

inMandlebrotSet :: Point -> Bool
inMandlebrotSet p = all fairlyClose (generatedSequence p)

approxTest :: Int -> Point -> Bool
approxTest n p = all fairlyClose (take n (generatedSequence p))

chooseColor :: [color] -> [Point] -> color
chooseColor palette = (palette !!) . length . take n . takeWhile fairlyClose
  where n = length palette - 1
  
fracImage :: Fractal -> [color] -> Image color
fracImage fractal palette = chooseColor palette . fractal

grid :: Int -> Int -> Point -> Point -> Grid Point
grid c r (xmin, ymin) (xmax, ymax) = [[(x,y) | x <- for c xmin xmax] | y <- for r ymin ymax]

for :: Int -> Float -> Float -> [Float]
for n min max = take n [min, min + delta ..]
                where delta = (max - min) / fromIntegral(n - 1)
                
sample :: Grid Point -> Image color -> Grid color
sample points image = map (map image) points

draw :: Grid Point -> Fractal -> [color] -> (Grid color -> image) -> image
draw points fractal palette render = render (sample points (fracImage fractal palette))

-- Character based Pictures
charPalette :: [Char]
charPalette = " ,.‘\"~:;o-!|?/<>X+={^O#%&@8*$"

charRender :: Grid Char -> IO ()
charRender = putStr . unlines

figure1 = draw points generatedSequence charPalette charRender
        where points = grid 79 37 (-2.25, -1.5) (0.75, 1.5)