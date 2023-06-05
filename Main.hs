import Codec.Picture
import System.Environment (getArgs)
import System.Exit (die)
import Control.Parallel.Strategies

import qualified Gaussian as G
import qualified Median as M
import Utils

genAndSave :: Image PixelRGB8 -> String -> IO (Image PixelRGB8)
genAndSave img name = do
    savePngImage ("images/" ++ name) (ImageRGB8 img)
    return img

genAndSave_ :: Image PixelRGB8 -> String -> IO ()
genAndSave_ img name = savePngImage ("images/" ++ name) (ImageRGB8 img)

sharpenGauss :: DynamicImage -> IO ()
sharpenGauss dynamic_img = do
    putStrLn "Input sigma:"
    sigmastr <- getLine
    let sigma = read sigmastr :: Double
    putStrLn "Input enhancement factor:"
    factorstr <- getLine
    let factor = read factorstr :: Pixel8

    original <- genAndSave (convertRGB8 dynamic_img) "original.png"
    blurred <- genAndSave (G.convolute sigma original) "blurred.png"
    diff <- genAndSave (subtractImages original blurred) "diff.png"
    enhanced_diff <- genAndSave (pixelMap (enhancePixel factor) diff) "enhanced_diff.png"
    genAndSave_ (addImages original enhanced_diff) "final.png"

sharpenMedian :: DynamicImage -> IO ()
sharpenMedian dynamic_img = do
    putStrLn "Input radius:"
    radiusstr <- getLine
    let radius = read radiusstr :: Int
    putStrLn "Input enhancement factor:"
    factorstr <- getLine
    let factor = read factorstr :: Pixel8

    original <- genAndSave (convertRGB8 dynamic_img) "original.png"
    blurred <- genAndSave (M.convolute radius original) "blurred.png"
    diff <- genAndSave (subtractImages original blurred) "diff.png"
    enhanced_diff <- genAndSave (pixelMap (enhancePixel factor) diff) "enhanced_diff.png"
    genAndSave_ (addImages original enhanced_diff) "final.png"

copyPixel :: Pixel a => Int -> Int -> Image a -> Int -> Int -> a
copyPixel x_offset y_offset img x y = pixelAt img (x_offset + x) (y_offset + y)

splitImage :: Pixel a => Image a -> Int -> Eval [Image a]
splitImage img side_length = do
    let width = imageWidth img
        sub_img_width = div width side_length
        height = imageHeight img
        sub_img_height = div height side_length
        -- this is neccassary for cases when image is not divided correctly
        -- in order to compare it with original later
        -- im adding extra pixels to subimages in last row/column
        wpixels = mod width side_length
        hpixels = mod height side_length
        cpfoo = \x y -> copyPixel (x * sub_img_width) (y * sub_img_height) img
        siw = \x -> sub_img_width + (if x == side_length - 1 then wpixels else 0)
        sih = \y -> sub_img_height + (if y == side_length - 1 then hpixels else 0)
        generateSubImage x y = generateImage (cpfoo x y) (siw x) (sih y)
    return $ parMap rpar (uncurry generateSubImage) [(x, y) | y <- [0 .. side_length - 1], x <- [0 .. side_length - 1]]

copyPixelFromSubImgs :: Pixel a => Int -> Int -> Int -> [Image a] -> Int -> Int -> a
copyPixelFromSubImgs sub_width sub_height side_length sub_imgs x y =
    let cropped_width = side_length * sub_width
        cropped_height = side_length * sub_height
        -- subimages in last row/colum have extra pixels
        img_row = div x sub_width - if x >= cropped_width then 1 else 0
        img_col = div y sub_height - if y >= cropped_height then 1 else 0
        img_idx = img_col * side_length + img_row
        relative_x = mod x sub_width + if x >= cropped_width then sub_width else 0
        relative_y = mod y sub_height + if y >= cropped_height then sub_height else 0
    in pixelAt (sub_imgs !! img_idx) relative_x relative_y

concatImages :: Pixel a => Int -> Int -> Int -> [Image a] -> Image a
concatImages _ _ _ [] = undefined
concatImages wpixels hpixels side_length (img:imgs) =
    let sub_width = imageWidth img
        sub_height = imageHeight img
        width = side_length * sub_width + wpixels
        height = side_length * sub_height + hpixels
    in generateImage (copyPixelFromSubImgs sub_width sub_height side_length (img:imgs)) width height


sharpenMedianPar :: DynamicImage -> IO ()
sharpenMedianPar dynamic_img = do
    putStrLn "Input radius:"
    radiusstr <- getLine
    let radius = read radiusstr :: Int
    putStrLn "Input enhancement factor:"
    factorstr <- getLine
    let factor = read factorstr :: Pixel8


    let side_length = 2
    original <- genAndSave (convertRGB8 dynamic_img) "original.png"
    let sub_imgs = runEval $ splitImage original side_length
    let sub_blurred = parMap rpar (M.convolute radius) sub_imgs
    genAndSave_ (sub_blurred !! 0) "img1.png"
    genAndSave_ (sub_blurred !! 1) "img2.png"
    genAndSave_ (sub_blurred !! 2) "img3.png"
    genAndSave_ (sub_blurred !! 3) "img4.png"

    let wpixels = mod (imageWidth original) side_length
        hpixels = mod (imageHeight original) side_length

    blurred <- genAndSave (concatImages wpixels hpixels side_length sub_blurred) "blurred.png"
    diff <- genAndSave (subtractImages original blurred) "diff.png"
    enhanced_diff <- genAndSave (pixelMap (enhancePixel factor) diff) "enhanced_diff.png"
    genAndSave_ (addImages original enhanced_diff) "final.png"

main :: IO ()
main = do
    args <- System.Environment.getArgs
    path <- case args of
            [path_] -> return path_
            _ -> die "Usage: ./hpip <path>"

    either_img <- readImage path
    case either_img of
        Left s -> die s
        Right dynamic_img -> do
            putStrLn "G — Gaussian kernel"
            putStrLn "M — Median filter"
            putStrLn "MP — Median filter parallel"
            putStrLn "Choose sharpening method:"
            flag <- getLine
            case flag of
                "G" -> sharpenGauss dynamic_img
                "M" -> sharpenMedian dynamic_img
                "MP" -> sharpenMedianPar dynamic_img
                _ -> die "No such method"