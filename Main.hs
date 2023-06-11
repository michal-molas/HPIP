{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Picture
import Codec.Picture.Gif
import System.Environment (getArgs)
import System.Exit (die)
import Control.Parallel.Strategies
import Control.Concurrent (setNumCapabilities, getNumCapabilities)
import GHC.Conc (getNumProcessors)
import qualified Data.ByteString as BS
import Data.List

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
        -- last subimage has extra pixels
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

gaussianBlur :: Double -> Image PixelRGB8 -> Image PixelRGB8
gaussianBlur = G.convolute

medianBlur :: Int -> Int -> Image PixelRGB8 -> Image PixelRGB8
medianBlur radius 1 img = M.convolute radius 1 img 0
medianBlur radius num_threads img =
    let sub_blurred = parMap rseq (M.convolute radius num_threads img) [0 .. num_threads - 1]
        extra_pixels = mod (imageHeight img) num_threads
    in concatImages extra_pixels num_threads sub_blurred

sharpenVerbose :: Double -> (Image PixelRGB8 -> Image PixelRGB8) -> Image PixelRGB8 -> IO ()
sharpenVerbose factor blur_method img = do
    genAndSave_ img "original.png"
    blurred <- genAndSave (blur_method img) "blurred.png"
    diff <- genAndSave (subtractImages img blurred) "diff.png"
    enhanced_diff <- genAndSave (if factor == 1.0 then diff else pixelMap (enhancePixel factor) diff) "enhanced_diff.png"
    genAndSave_ (addImages img enhanced_diff) "final.png"

sharpen :: Double -> (Image PixelRGB8 -> Image PixelRGB8) -> Image PixelRGB8 -> Image PixelRGB8
sharpen factor blur_method img =
    let blurred = blur_method img
        diff = subtractImages img blurred
        enhanced_diff = if factor == 1.0 then diff else pixelMap (enhancePixel factor) diff
        in addImages img enhanced_diff

sharpenChunk :: Double -> (Image PixelRGB8 -> Image PixelRGB8) -> [Image PixelRGB8] -> [Image PixelRGB8]
sharpenChunk factor blur_method = map (sharpen factor blur_method)


isGif :: [String] -> Bool
isGif [] = undefined
isGif (path:args) = elem "--gif" args || isSuffixOf ".gif" path

splitIntoParts :: Int -> [a] -> [[a]]
splitIntoParts 1 xs = [xs]
splitIntoParts n xs = take first_length xs : splitIntoParts (n - 1) (drop first_length xs)
  where
    total_length = length xs
    first_length = total_length `div` n + if total_length `mod` n > 0 then 1 else 0


mainGif :: String -> IO ()
mainGif path = do
    gif_bs <- BS.readFile path
    either_img <- readGifImages path
    case either_img of
        Left s -> die s
        Right dynamic_imgs -> do
            num_threads <- getNumCapabilities

            factor <- getInput @Double "enhancement factor"
            -- sigma <- getInput @Double "sigma"
            radius <- getInput @Int "radius"

            let imgs = map convertRGB8 dynamic_imgs
                imgs_chunks = splitIntoParts num_threads imgs
                sharp_imgs_chunks = parMap rdeepseq (sharpenChunk factor (medianBlur radius 1)) imgs_chunks
                sharp_imgs = concat sharp_imgs_chunks

            case getDelaysGifImages gif_bs of
                Left s -> die s
                Right delays -> do
                    case writeGifAnimation "images/final.gif" (head delays) LoopingForever sharp_imgs of
                        Left s -> die s
                        Right a -> a

mainImg :: String -> Bool -> IO ()
mainImg path verbose = do
    either_img <- readImage path
    case either_img of
        Left s -> die s
        Right dynamic_img -> do
            let img = convertRGB8 dynamic_img

            putStrLn "G — Gaussian kernel"
            putStrLn "M — Median filter"
            putStrLn "Choose method:"
            flag <- getLine

            num_threads <- getNumCapabilities

            factor <- getInput @Double "enhancement factor"
            method <- case flag of
                    "G" -> do
                        sigma <- getInput @Double "sigma"
                        return $ gaussianBlur sigma
                    "M" -> do
                        radius <- getInput @Int "radius"
                        return $ medianBlur radius num_threads
                    _ -> return id

            if verbose
                then sharpenVerbose factor method img
                else genAndSave_ (sharpen factor method img) "final.png"

main :: IO ()
main = do
    args <- System.Environment.getArgs
    path <- case args of
            (path_:_) -> return path_
            _ -> die "Usage: ./hpip <path>"

    if isGif args
        then mainGif path
        else mainImg path ("--verbose" `elem` args)
