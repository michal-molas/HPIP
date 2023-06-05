import Codec.Picture
import System.Environment (getArgs)
import System.Exit (die)

-- Gaussian blur kernel generation
gaussianKernelHelp :: Int -> Double -> Int -> [Double]
gaussianKernelHelp idx sigma size
  | idx == size = []
  | otherwise =
    let center = div (size - 1) 2
        diff_x = idx - center
        sigmasq = sigma * sigma
        expo = -(fromIntegral (diff_x * diff_x) / (2 * sigmasq))
        coefficient = 1 / (sigma * sqrt (2 * pi))
     in ((coefficient * exp expo):gaussianKernelHelp (idx + 1) sigma size)

gaussianKernel :: Double -> Int -> [Double]
gaussianKernel = gaussianKernelHelp 0

defaultPixelAt :: Pixel a => a -> Image a -> Int -> Int -> a
defaultPixelAt default_px img x y =
    let width = imageWidth img
        height = imageHeight img
            in
                if x < width && x >= 0 && y < height && y >= 0
                    then pixelAt img x y
                    else default_px

safeAdd :: Pixel8 -> Pixel8 -> Pixel8
safeAdd p1 p2 = if fromIntegral p2 + fromIntegral p1 > 255 then 255 else p1 + p2

castToPixel8 :: Double -> Pixel8
castToPixel8 value = clamp (round value)
  where
    clamp :: Int -> Pixel8
    clamp val
      | val < 0 = 0
      | val > 255 = 255
      | otherwise = fromIntegral val

convolveHorizontalHelp :: [Double] -> Image PixelRGB8 -> Int -> Int -> (Double, Double, Double)
convolveHorizontalHelp [] _ _ _ = (0.0, 0.0, 0.0)
convolveHorizontalHelp (a:gauss) img x y =
    let PixelRGB8 r g b = defaultPixelAt (PixelRGB8 0 0 0) img x y
        (r', g', b') = convolveHorizontalHelp gauss img (x + 1) y
    in (a * fromIntegral r + r', a * fromIntegral g + g', a * fromIntegral b + b') 

convolveHorizontal :: Int -> [Double] -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
convolveHorizontal gauss_size gauss img x y =
    let center = div (gauss_size - 1) 2
        (r, g, b) = convolveHorizontalHelp gauss img (x - center) y
    in PixelRGB8 (castToPixel8 r) (castToPixel8 g) (castToPixel8 b)

convolveVerticalHelp :: [Double] -> Image PixelRGB8 -> Int -> Int -> (Double, Double, Double)
convolveVerticalHelp [] _ _ _ = (0.0, 0.0, 0.0)
convolveVerticalHelp (a:gauss) img x y =
    let PixelRGB8 r g b = defaultPixelAt (PixelRGB8 0 0 0) img x y
        (r', g', b') = convolveVerticalHelp gauss img x (y + 1)
    in (a * fromIntegral r + r', a * fromIntegral g + g', a * fromIntegral b + b') 

convolveVertical :: Int -> [Double] -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
convolveVertical gauss_size gauss img x y =
    let center = div (gauss_size - 1) 2
        (r, g, b) = convolveVerticalHelp gauss img x (y - center)
    in PixelRGB8 (castToPixel8 r) (castToPixel8 g) (castToPixel8 b)

convolve :: Int -> [Double] -> Image PixelRGB8 -> Image PixelRGB8
convolve gauss_size gauss img =
    let width = imageWidth img
        height = imageHeight img
        horizontal = generateImage (convolveHorizontal gauss_size gauss img) width height
            in generateImage (convolveVertical gauss_size gauss horizontal) width height

safeSub :: Pixel8 -> Pixel8 -> Pixel8
safeSub p1 p2 = if p2 > p1 then 0 else p1 - p2

subtractImages :: Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
subtractImages img1 img2 =
    let f = (\x y ->
                let PixelRGB8 r g b = pixelAt img1 x y
                    PixelRGB8 r' g' b' = pixelAt img2 x y
                in PixelRGB8 (safeSub r r') (safeSub g g') (safeSub b b'))
    in generateImage f (imageWidth img1) (imageHeight img1)

addImages :: Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
addImages img1 img2 =
    let f = (\x y ->
                let PixelRGB8 r g b = pixelAt img1 x y
                    PixelRGB8 r' g' b' = pixelAt img2 x y
                in PixelRGB8 (safeAdd r r') (safeAdd g g') (safeAdd b b'))
    in generateImage f (imageWidth img1) (imageHeight img1)



-- pomysł: 
-- 1. zmienić na czarnobiałe
-- (2). podzielić obraz na ileś części (np 9/16) za pomocą crop
-- 3. na każdej z części wykonać to gaussian blur
main :: IO ()
main = do
    args <- System.Environment.getArgs
    (path, sigma, factor) <- case args of
            [path_, sigma_, factor_] -> return (path_, read sigma_ :: Double, read factor_ :: Pixel8)
            _ -> die "No file given"

    -- let factor = 3
    -- let sigma = 3.0
    let gauss_size = let x = ceiling (6 * sigma) in if even x then x + 1 else x
    print gauss_size
    let gauss = gaussianKernel sigma gauss_size
    either_img <- readImage path
    case either_img of
        Left s -> die s
        Right dynamic_img -> do
            let img = convertRGB8 dynamic_img
            savePngImage "orig.png" (ImageRGB8 img)
            let blurred = convolve gauss_size gauss img
            savePngImage "blur.png" (ImageRGB8 blurred)
            let diff = subtractImages img blurred
            savePngImage "diff.png" (ImageRGB8 diff)
            let enhanceddiff = pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 (r * factor) (g * factor) (b * factor)) diff
            savePngImage "enhanceddiff.png" (ImageRGB8 enhanceddiff)
            let out = addImages img enhanceddiff
            savePngImage "out.png" (ImageRGB8 out)
            -- let grey_img = convertToGS img
            -- savePngImage "grey.png" (ImageY8 grey_img)
            -- let conv_img = convolve gauss_size gauss grey_img
            -- savePngImage "conv.png" (ImageY8 conv_img)
            -- let diff_img = subtractImagesG grey_img conv_img
            -- savePngImage "diff.png" (ImageY8 diff_img)
            -- let stronger_img = diff_img--pixelMap (\x -> if x > 20 then x else 255) diff_img
            -- savePngImage "adjusted.png" (ImageY8 stronger_img)
            -- let final_img = subtractImagesRGB img stronger_img
            -- savePngImage "final.png" (ImageRGB8 final_img)