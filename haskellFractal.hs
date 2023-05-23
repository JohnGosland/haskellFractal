import Diagrams.Prelude
import Diagrams.Backend.SVG.CmdLine

branch :: Double -> Double -> Double -> Int -> Diagram B
branch x y angle depth
  | depth < 1 = mempty
  | otherwise =
    let x2 = x + cos angle * fromIntegral depth * 10
        y2 = y + sin angle * fromIntegral depth * 10
        line = fromVertices [p2 (x, y), p2 (x2, y2)]
    in line <> branch x2 y2 (angle - 0.4) (depth - 1) <> branch x2 y2 (angle + 0.4) (depth - 1)

fractal :: Diagram B
fractal =
  let x = 50
      y = 50
      angle = -pi/2
  in mconcat [branch x y (angle + (i * pi * 0.01)) 9 | i <- [0..200]]

main :: IO ()
main = mainWith (pad 1 fractal)