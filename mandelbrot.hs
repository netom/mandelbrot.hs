import Graphics.GD
import Control.Concurrent.ParallelIO

-- Function for determining if a complex number is in the set
-- Uses checks for the main cardioid and the period-2 bulb, and
-- uses the escape time algorithm
inTheSet :: (Double, Double) -> Int -> Bool
inTheSet (x1, x2) maxiter
    | x1 < sqrt p-2*p+1/4 || (x1+1)*(x1+1) + x2_2 < 1/16 = True
    | otherwise = inTheSetW (0, 0) (x1, x2) maxiter
    where
        p = ((x1-1/4)*(x1-1/4)+x2_2)
        x2_2 = x2*x2

-- A number is in the set if the x(n+1) = x(n)^2 + c, x(0) = 0
-- series is bounded. A point is certainly not in the set, when
-- it's absolute value is > 2. We check the series up to a given
-- limit (maxiter) to see, if there is a value whose abs. is > 2.
-- If there is, the point is not in the set, if there is none,
-- it _probably_ is.
inTheSetW :: (Double, Double) -> (Double, Double) -> Int -> Bool
inTheSetW (x1, x2) (c1, c2) maxiter
    | flew_away    = False
    | maxiter <= 0 = not flew_away
    | otherwise    = inTheSetW (x1_2-x2_2+c1, 2*x1*x2+c2) (c1, c2) (maxiter-1)
    where
        flew_away = x1_2+x2_2 > 4
        x1_2 = x1*x1
        x2_2 = x2*x2

-- Compute a rectangle part of the image. The sides are parallel to the
-- image's sides. The rectangle is given with the upper left and lower
-- right corners' coordinates respectively. The center is a complex
-- number at the center of the image. The scale is a positive real
-- number, and it's the size of one pixel on the complex plane.
mandelbrotCU :: Image -> (Int, Int) -> (Int, Int) -> (Double, Double) -> Double -> Int -> IO ()
mandelbrotCU image (x1, y1) (x2, y2) (xc, yc) scale maxiter = do
    mapM_ (\(x, y) -> setPixel (x, y) 0xffffff image) [(x, y) | x <- [x1..x2], y <- [y1..y2], not $ inTheSet (xp x, yp y) maxiter]
    where
          xp x = (fromIntegral x - ((fromIntegral x2 + fromIntegral x1)/2))*scale + xc
          yp y = (fromIntegral y - ((fromIntegral y2 + fromIntegral y1)/2))*scale - yc

-- Auxilliary function that splits a list into well sized chunks
chunks :: [a] -> Int -> [[a]]
chunks list size
    | length list <= size = [list]
    | otherwise           = (take size list) : chunks (drop size list) size

-- Extracts a list of (head,last) pairs from lists of lists.
ranges :: [a] -> Int -> [(a,a)]
ranges list size = map (\x -> (head x, last x)) (chunks list size)

-- Renders a mandelbrot set onto a GD image. The center, scale and
-- maxiter parameters are described at mandelbrotCU
mandelbrot :: Image -> (Double, Double) -> Double -> Int -> IO ()
mandelbrot image (cx, cy) scale maxiter = do
    (w, h) <- imageSize image
    -- Calculate each 100 lines in parallel
    parallel_ [mandelbrotCU image (0, y1) (w-1, y2) (cx, cy + (fromIntegral h)/2*scale - (fromIntegral y2 + fromIntegral y1) / 2 * scale) scale maxiter | (y1, y2) <- ranges [0..h-1] 100]

main = do
    image <- newImage (800, 800)
    mandelbrot image (-0.7, 0) 0.004 10000
    stopGlobalPool
    savePngFile "mandelbrot.png" image
