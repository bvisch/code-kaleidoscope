module Main where

-- import Lib
import Reanimate
import Reanimate.Builtin.Documentation
import Control.Applicative
import Control.Monad
import Control.Monad.State
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

fromPolar r theta = (r * cos theta, r * sin theta)

evenRadialPoints :: Int -> [Point]
evenRadialPoints n = points
    where
        angle = pi / (fromIntegral n)
        segments = take n $ iterate (+ angle) 0.0
        points = map (fromPolar 1.0) segments


defineRadialSymmetries :: Int -> [Line]
defineRadialSymmetries n = map (mkLn (0.0, 0.0)) (evenRadialPoints n)

randomPoint :: (Double, Double) -> Gen Point
randomPoint bounds = liftM2 (,) (choose bounds) (choose bounds)

randomPoints :: Int -> (Double, Double) -> [Point]
randomPoints n bounds = unGen (vectorOf n $ randomPoint bounds) (mkQCGen 1) n

randomTranslation :: (Double, Double) -> [Double] -> Gen Point
randomTranslation radiusRange angles = liftM2 fromPolar (choose radiusRange) (elements angles)
        
applyTranslation :: State (Point, [Point]) Point
applyTranslation = do
    ((px, py), translations) <- get
    let (tx, ty) = head translations
    let next = (px + tx, py + ty)
    put $ (next, tail translations)
    return next

applyTranslations :: [Point] -> Point -> [Point]
applyTranslations translations start = result
    where
        (result, _) = runState (replicateM (length translations) applyTranslation) (start, translations)

randomPath :: Int -> (Double, Double) -> [Double] -> [Point]
randomPath n bounds angles = path
    where
        gen = mkQCGen 7
        start = unGen (randomPoint bounds) gen 1
        translations = unGen (vectorOf n $ randomTranslation bounds angles) gen n
        path = applyTranslations translations start



chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = [take n xs] ++ (chunk n $ drop n xs)


getAngles :: Double -> [Double]
getAngles angle = anglesBelowPi ++ anglesAbovePi
    where
        anglesBelowPi = take (n `div` 2 - 1) $ map (* angle) [1..]
        anglesAbovePi = take (n `div` 2 - 1) $ map ((+ pi) . (* angle)) [1..]
        n = floor $ (2*pi) / angle

main :: IO ()
main = reanimate $ docEnv $ pauseAtEnd 3 $ mkAnimation 5 $ \t ->
    partialSvg t $ pathify $ mkGroup $
    map mkLinePath (reflectedPath ++ unreflectedPath)
    where
        lines = defineRadialSymmetries 9
        pathLength = 20
        pathSegmentBounds = (0.5, 2.5)
        angles = getAngles (pi/3)
        path = randomPath pathLength pathSegmentBounds angles
        reflectedPath = map (reflectPath path) lines
        unreflectedPath = map (reflectPath (head reflectedPath)) lines
        
