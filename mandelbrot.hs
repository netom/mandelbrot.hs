import Graphics.GD
import Control.Concurrent.ParallelIO

inTheSet :: (Num a, Ord a) => (a, a) -> Int -> Bool
inTheSet x maxiter = inTheSetW (0, 0) x maxiter

inTheSetW :: (Num a, Ord a) => (a, a) -> (a, a) -> Int -> Bool
inTheSetW (x1, x2) (c1, c2) maxiter
    | flew_away    = False
    | maxiter <= 0 = not flew_away
    | otherwise    = inTheSetW (x1*x1-x2*x2+c1, 2*x1*x2+c2) (c1, c2) (maxiter-1)
    where
        flew_away = x1*x1+x2*x2 > 4

mandelbrotCU :: Image -> (Int, Int) -> (Int, Int) -> (Float, Float) -> Float -> Int -> IO ()
mandelbrotCU image (x1, y1) (x2, y2) (xc, yc) scale maxiter = do
    mapM_ (\(x, y) -> setPixel (x, y) (if inTheSet (xp x, yp y) maxiter then 0x000000 else 0xffffff) image) [(x, y)|x <- [x1..x2], y <- [y1..y2]]
    where
        --xp x = xc-((fromIntegral x2 + fromIntegral x1)/2-(fromIntegral x2 - fromIntegral x))*scale
        --yp y = yc+((fromIntegral y2 + fromIntegral y1)/2-(fromIntegral y2 - fromIntegral y))*scale
          xp x = (fromIntegral x - ((fromIntegral x2 + fromIntegral x1)/2))*scale + xc
          yp y = (fromIntegral y - ((fromIntegral y2 + fromIntegral y1)/2))*scale + yc

chunks :: [a] -> Int -> [[a]]
chunks list size
    | length list <= size = [list]
    | otherwise           = (take size list) : chunks (drop size list) size

ranges :: [a] -> Int -> [(a,a)]
ranges list size = map (\x -> (head x, last x)) (chunks list size)

mandelbrot :: Image -> (Float, Float) -> Float -> Int -> IO ()
mandelbrot image (cx, cy) scale maxiter = do
    (w, h) <- imageSize image
    -- Calculate each 20 lines in parallel
    parallel_ [mandelbrotCU image (0, y1) (w-1, y2) (cx, cy - (fromIntegral h)/2*scale + (fromIntegral y2 + fromIntegral y1) / 2 * scale) scale maxiter | (y1, y2) <- ranges [0..h-1] 20]

main = do
    image <- newImage (800, 800)
    mandelbrot image (-0.7, 0) 0.004 5000
    stopGlobalPool
    savePngFile "mandelbrot.png" image
