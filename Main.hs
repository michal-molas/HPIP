{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Codec.Picture
import System.Environment (getArgs)
import System.Exit (die)
import Control.Parallel.Strategies
import Control.Concurrent (setNumCapabilities)
-- import GHC.Conc (getNumProcessors)

import qualified Gaussian as G
import qualified Median as M
import Utils

genAndSave :: Image PixelRGB8 -> String -> IO (Image PixelRGB8)
genAndSave img name = do
    savePngImage ("images/" ++ name) (ImageRGB8 img)
    return img

genAndSave_ :: Image PixelRGB8 -> String -> IO ()
genAndSave_ img name = savePngImage ("images/" ++ name) (ImageRGB8 img)

getInput :: Read a => String -> IO a
getInput name = do
    putStrLn $ "Input " ++ name ++ ":"
    read <$> getLine

sharpenGauss :: DynamicImage -> IO ()
sharpenGauss dynamic_img = do
    sigma <- getInput @Double "sigma"
    factor <- getInput @Pixel8 "enhancement factor"

    original <- genAndSave (convertRGB8 dynamic_img) "original.png"
    blurred <- genAndSave (G.convolute sigma original) "blurred.png"
    diff <- genAndSave (subtractImages original blurred) "diff.png"
    enhanced_diff <- genAndSave (pixelMap (enhancePixel factor) diff) "enhanced_diff.png"
    genAndSave_ (addImages original enhanced_diff) "final.png"

sharpenMedian :: DynamicImage -> IO ()
sharpenMedian dynamic_img = do
    radius <- getInput @Int "radius"
    factor <- getInput @Pixel8 "enhancement factor"

    original <- genAndSave (convertRGB8 dynamic_img) "original.png"
    blurred <- genAndSave (M.convolute radius original) "blurred.png"
    diff <- genAndSave (subtractImages original blurred) "diff.png"
    enhanced_diff <- genAndSave (pixelMap (enhancePixel factor) diff) "enhanced_diff.png"
    genAndSave_ (addImages original enhanced_diff) "final.png"

copyPixel :: Pixel a => Int -> Int -> Image a -> Int -> Int -> a
copyPixel x_offset y_offset img x y = pixelAt img (x_offset + x) (y_offset + y)

splitImage :: Pixel a => Image a -> Int -> Eval [Image a]
splitImage img num_sub = do
    let width = imageWidth img
        height = imageHeight img
        sub_img_height = div height num_sub
        -- this is neccassary for cases when image is not divided correctly
        -- in order to compare it with original later
        -- im adding extra pixels to subimage in last column
        extra_pixels = mod height num_sub
        cpfoo = \sub_idx -> copyPixel 0 (sub_idx * sub_img_height) img
        sih = \sub_idx -> sub_img_height + (if sub_idx == num_sub - 1 then extra_pixels else 0)
        generateSubImage = \sub_idx -> generateImage (cpfoo sub_idx) width (sih sub_idx)
    return $ parMap rpar generateSubImage [sub_idx | sub_idx <- [0 .. num_sub - 1]]

copyPixelFromSubImgs :: Pixel a => Int -> Int -> [Image a] -> Int -> Int -> a
copyPixelFromSubImgs sub_height num_sub sub_imgs x y =
    let cropped_height = num_sub * sub_height
        -- subimages in last row have extra pixels
        img_idx = div y sub_height - if y >= cropped_height then 1 else 0
        relative_y = mod y sub_height + if y >= cropped_height then sub_height else 0
    in pixelAt (sub_imgs !! img_idx) x relative_y

concatImages :: Pixel a => Int -> Int -> [Image a] -> Image a
concatImages _ _ [] = undefined
concatImages extra_pixels num_sub (img:imgs) =
    let sub_height = imageHeight img
        width = imageWidth img
        height = num_sub * sub_height + extra_pixels
    in generateImage (copyPixelFromSubImgs sub_height num_sub (img:imgs)) width height

sharpenMedianPar :: DynamicImage -> IO ()
sharpenMedianPar dynamic_img = do
    num_threads <- getInput @Int "number of threads"

    case num_threads of
        1 -> sharpenMedian dynamic_img
        _ -> do setNumCapabilities num_threads
                radius <- getInput @Int "radius"
                factor <- getInput @Pixel8 "enhancement factor"

                original <- genAndSave (convertRGB8 dynamic_img) "original.png"

                let num_sub = num_threads
                    sub_imgs = runEval $ splitImage original num_sub
                    sub_blurred = parMap rpar (M.convolute radius) sub_imgs
                    extra_pixels = mod (imageHeight original) num_sub

                blurred <- genAndSave (concatImages extra_pixels num_sub sub_blurred) "blurred.png"
                diff <- genAndSave (subtractImages original blurred) "diff.png"
                enhanced_diff <- genAndSave (pixelMap (enhancePixel factor) diff) "enhanced_diff.png"
                genAndSave_ (addImages original enhanced_diff) "final.png"

main :: IO ()
main = do
    args <- System.Environment.getArgs
    path <- case args of
            [path_] -> return path_
            _ -> die "Usage: ./hpip <path>"

    -- putStrLn $ "number of cores: " ++ show getNumProcessors

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