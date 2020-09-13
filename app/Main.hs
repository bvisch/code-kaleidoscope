module Main where

-- import Lib
import Reanimate
import Reanimate.Builtin.Documentation
import Control.Applicative
import Control.Monad
import Test.QuickCheck.Gen
import Test.QuickCheck.Random

type Point = (Double, Double)
data Line = Line { slope :: Double, intercept :: Double }

mkLn :: (Double, Double) -> (Double, Double) -> Line
mkLn (x1, y1) (x2, y2) = Line m b
    where
        m = (y2 - y1) / (x2 - x1)
        b = y2 - m*x2

reflectPoint :: Point -> Line -> Point
reflectPoint (px, py) (Line m b) = (x, y)
    where
        x = ((1 - m*m)*px + 2*m*py - 2*m*b) / (m*m + 1)
        y = ((m*m - 1)*py + 2*m*px + 2*b) / (m*m + 1)

reflectLine :: (Point, Point) -> Line -> (Point, Point)
reflectLine (p1, p2) line = (reflectPoint p1 line, reflectPoint p2 line)


evenRadialPoints :: Int -> [Point]
evenRadialPoints n = points
    where
        fromPolar r theta = (r * cos theta, r * sin theta)
        angle = pi / (fromIntegral n)
        segments = take n $ iterate (+ angle) 0.0
        points = map (fromPolar 1.0) segments


defineRadialSymmetries :: Int -> [Line]
defineRadialSymmetries n = map (mkLn (0.0, 0.0)) (evenRadialPoints n)

randomPoint :: (Double, Double) -> Gen Point
randomPoint bounds = liftM2 (,) (choose bounds) (choose bounds)

randomPoints :: Int -> (Double, Double) -> [Point]
randomPoints n bounds = unGen (vectorOf n $ randomPoint bounds) (mkQCGen 1) n

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs [x] = [(x, x)]
pairs (x1:x2:xs) = [(x1, x2)] ++ pairs xs


main :: IO ()
main = reanimate $ docEnv $ pauseAtEnd 3 $ mkAnimation 5 $ \t ->
    partialSvg t $ pathify $ mkGroup $
    map (uncurry mkLine) (reflectedLines ++ unreflectedLines)
    where
        radialPoints = evenRadialPoints 9
        reflectedPoints = map (\(x, y) -> (-x, -y)) radialPoints
        lines = map (uncurry mkLn) $ zip radialPoints reflectedPoints
        randomLines = pairs $ randomPoints 8 (0.5, 4.0)
        -- randomLines = [((2.0, 3.3), (1.1, 2.6)), ((1.4, 4.7), (1.6, 1.1))]
        reflectedLines = liftM2 (reflectLine) randomLines lines -- concatMap (map (flip reflectLine) lines) randomLines
        unreflectedLines = liftM2 (reflectLine) reflectedLines lines
        
