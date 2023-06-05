import Codec.Picture
import Codec.Picture.Types
import System.Environment (getArgs)
import System.Exit (die)

-- converting pixel to greyscale
pixelToGS :: PixelRGB8 -> Pixel8
pixelToGS (PixelRGB8 r g b) = fromIntegral $ div (toInteger r + toInteger g + toInteger b) 3

-- converting image to greyscale
convertToGS :: Image PixelRGB8 -> Image Pixel8
convertToGS = pixelMap pixelToGS

-- Gaussian blur kernel generation
gaussianKernelHelp :: Int -> Double -> Int -> [Double]
gaussianKernelHelp idx sigma size
  | idx == size = []
  | otherwise =
    let center = div (size - 1) 2
        diff_x = idx - center
        sigmasq = sigma * sigma
        exponent = -(fromIntegral (diff_x * diff_x) / (2 * sigmasq))
        coefficient = 1 / (sigma * sqrt (2 * pi))
     in ((coefficient * exp exponent):gaussianKernelHelp (idx + 1) sigma size)

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

convolveHorizontalHelp :: [Double] -> Image Pixel8 -> Int -> Int -> Double
convolveHorizontalHelp [] _ _ _ = 0.0
convolveHorizontalHelp (a:gauss) img x y =
    let g = defaultPixelAt 0 img x y
        rest = convolveHorizontalHelp gauss img (x + 1) y
    in a * fromIntegral g + rest

convolveHorizontal :: Int -> [Double] -> Image Pixel8 -> Int -> Int -> Pixel8
convolveHorizontal gauss_size gauss img x y =
    let center = div (gauss_size - 1) 2
        in castToPixel8 $ convolveHorizontalHelp gauss img (x - center) y

convolveVerticalHelp :: [Double] -> Image Pixel8 -> Int -> Int -> Double
convolveVerticalHelp [] _ _ _ = 0
convolveVerticalHelp (a:gauss) img x y =
    let g = defaultPixelAt 0 img x y
        rest = convolveVerticalHelp gauss img x (y + 1)
    in a * fromIntegral g + rest

convolveVertical :: Int -> [Double] -> Image Pixel8 -> Int -> Int -> Pixel8
convolveVertical gauss_size gauss img x y =
    let center = div (gauss_size - 1) 2
        in castToPixel8 $ convolveVerticalHelp gauss img x (y - center)

convolve :: Int -> [Double] -> Image Pixel8 -> Image Pixel8
convolve gauss_size gauss img =
    let width = imageWidth img
        height = imageHeight img
        horizontal = generateImage (convolveHorizontal gauss_size gauss img) width height
            in generateImage (convolveVertical gauss_size gauss horizontal) width height

convolveHorizontalRGBHelp :: [Double] -> Image PixelRGB8 -> Int -> Int -> (Double, Double, Double)
convolveHorizontalRGBHelp [] _ _ _ = (0.0, 0.0, 0.0)
convolveHorizontalRGBHelp (a:gauss) img x y =
    let PixelRGB8 r g b = defaultPixelAt (PixelRGB8 0 0 0) img x y
        (r', g', b') = convolveHorizontalRGBHelp gauss img (x + 1) y
    in (a * fromIntegral r + r', a * fromIntegral g + g', a * fromIntegral b + b') 

convolveHorizontalRGB :: Int -> [Double] -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
convolveHorizontalRGB gauss_size gauss img x y =
    let center = div (gauss_size - 1) 2
        (r, g, b) = convolveHorizontalRGBHelp gauss img (x - center) y
    in PixelRGB8 (castToPixel8 r) (castToPixel8 g) (castToPixel8 b)

convolveVerticalRGBHelp :: [Double] -> Image PixelRGB8 -> Int -> Int -> (Double, Double, Double)
convolveVerticalRGBHelp [] _ _ _ = (0.0, 0.0, 0.0)
convolveVerticalRGBHelp (a:gauss) img x y =
    let PixelRGB8 r g b = defaultPixelAt (PixelRGB8 0 0 0) img x y
        (r', g', b') = convolveVerticalRGBHelp gauss img x (y + 1)
    in (a * fromIntegral r + r', a * fromIntegral g + g', a * fromIntegral b + b') 

convolveVerticalRGB :: Int -> [Double] -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
convolveVerticalRGB gauss_size gauss img x y =
    let center = div (gauss_size - 1) 2
        (r, g, b) = convolveVerticalRGBHelp gauss img x (y - center)
    in PixelRGB8 (castToPixel8 r) (castToPixel8 g) (castToPixel8 b)

convolveRGB :: Int -> [Double] -> Image PixelRGB8 -> Image PixelRGB8
convolveRGB gauss_size gauss img =
    let width = imageWidth img
        height = imageHeight img
        horizontal = generateImage (convolveHorizontalRGB gauss_size gauss img) width height
            in generateImage (convolveVerticalRGB gauss_size gauss horizontal) width height

subtractImagesG :: Image Pixel8 -> Image Pixel8 -> Image Pixel8
subtractImagesG img1 img2 =
    generateImage (\x y -> pixelAt img1 x y - pixelAt img2 x y) (imageWidth img1) (imageHeight img1)

safeSub :: Pixel8 -> Pixel8 -> Pixel8
safeSub p1 p2 = if p2 > p1 then 0 else p1 - p2

subtractImagesRGB :: Image PixelRGB8 -> Image Pixel8 -> Image PixelRGB8
subtractImagesRGB img1 img2 =
    let f = (\x y ->
                let PixelRGB8 r g b = pixelAt img1 x y
                    grey = pixelAt img2 x y
                in PixelRGB8 (safeSub r grey) (safeSub g grey) (safeSub b grey))
    in generateImage f (imageWidth img1) (imageHeight img1)

subtractImagesRGB2 :: Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
subtractImagesRGB2 img1 img2 =
    let f = (\x y ->
                let PixelRGB8 r g b = pixelAt img1 x y
                    PixelRGB8 r' g' b' = pixelAt img2 x y
                in PixelRGB8 (safeSub r r') (safeSub g g') (safeSub b b'))
    in generateImage f (imageWidth img1) (imageHeight img1)

addImagesRGB :: Image PixelRGB8 -> Image Pixel8 -> Image PixelRGB8
addImagesRGB img1 img2 =
    let f = (\x y ->
                let PixelRGB8 r g b = pixelAt img1 x y
                    grey = pixelAt img2 x y
                in PixelRGB8 (safeAdd r grey) (safeAdd g grey) (safeAdd b grey))
    in generateImage f (imageWidth img1) (imageHeight img1)

addImagesRGB2 :: Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
addImagesRGB2 img1 img2 =
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
            let blurred = convolveRGB gauss_size gauss img
            savePngImage "blur.png" (ImageRGB8 blurred)
            let diff = subtractImagesRGB2 img blurred
            savePngImage "diff.png" (ImageRGB8 diff)
            let enhanceddiff = pixelMap (\(PixelRGB8 r g b) -> PixelRGB8 (r * factor) (g * factor) (b * factor)) diff
            savePngImage "enhanceddiff.png" (ImageRGB8 enhanceddiff)
            let out = addImagesRGB2 img enhanceddiff
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