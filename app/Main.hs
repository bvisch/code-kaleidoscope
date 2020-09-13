module Main where

-- import Lib
import Reanimate
import Reanimate.Builtin.Documentation
import Control.Applicative
import Control.Monad
import Test.QuickCheck.Gen
import Test.QuickCheck.Random
import Data.List

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

reflectPath :: [Point] -> Line -> [Point]
reflectPath path line = map (flip reflectPoint line) path


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

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = [take n xs] ++ (chunk n $ drop n xs)

main :: IO ()
main = reanimate $ docEnv $ pauseAtEnd 3 $ mkAnimation 5 $ \t ->
    partialSvg t $ pathify $ mkGroup $
    map mkLinePath (reflectedPath ++ unreflectedPath)
    where
        radialPoints = evenRadialPoints 5
        reflectedPoints = map (\(x, y) -> (-x, -y)) radialPoints
        -- lines = map (uncurry mkLn) $ zip radialPoints reflectedPoints
        lines = defineRadialSymmetries 5
        randomPath = randomPoints 8 (0.5, 4.0)
        reflectedPath = map (reflectPath randomPath) lines
        unreflectedPath = map (reflectPath (head reflectedPath)) lines
        
