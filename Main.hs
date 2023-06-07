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

gaussianBlur :: Image PixelRGB8 -> IO (Image PixelRGB8)
gaussianBlur img = do
    sigma <- getInput @Double "sigma"

    genAndSave (G.convolute sigma img) "blurred.png"

medianBlur :: Int -> Image PixelRGB8 -> IO (Image PixelRGB8)
medianBlur num_threads img = do
    radius <- getInput @Int "radius"

    let sub_blurred = parMap rpar (M.convolute radius num_threads img) [0 .. num_threads - 1]
        extra_pixels = mod (imageHeight img) num_threads
        blurred = concatImages extra_pixels num_threads sub_blurred 

    genAndSave blurred "blurred.png"

sharpen :: DynamicImage -> (Image PixelRGB8 -> IO (Image PixelRGB8)) -> IO ()
sharpen dynamic_img blur_method = do
    original <- genAndSave (convertRGB8 dynamic_img) "original.png"
    blurred <- blur_method original
    diff <- genAndSave (subtractImages original blurred) "diff.png"

    factor <- getInput @Pixel8 "enhancement factor"

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
            putStrLn "Choose method:"
            flag <- getLine
            num_threads <- getInput @Int "number of threads"
            setNumCapabilities num_threads

            method <- case flag of
                "G" -> return gaussianBlur
                "M" -> return $ medianBlur num_threads
                _ -> die "No such method"

            sharpen dynamic_img method