{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Codec.Picture
import Codec.Picture.Gif
import System.Environment (getArgs)
import System.Exit (die)
import Control.Parallel.Strategies
import Control.Concurrent (getNumCapabilities)
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

gaussianBlur :: Double -> Int -> Image PixelRGB8 -> Image PixelRGB8
gaussianBlur sigma 1 img = 
    let horizontal = G.convoluteHorizontal sigma 1 img 0
    in G.convoluteVertical sigma 1 horizontal 0
gaussianBlur sigma num_threads img =
    let sub_blurred_hor = parMap rseq (G.convoluteHorizontal sigma num_threads img) [0 .. num_threads - 1]
        extra_pixels = mod (imageHeight img) num_threads
        horizontal = concatImages extra_pixels num_threads sub_blurred_hor

        sub_blurred_ver = parMap rseq (G.convoluteVertical sigma num_threads horizontal) [0 .. num_threads - 1]
    in concatImages extra_pixels num_threads sub_blurred_ver


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


mainGif :: String -> String -> IO ()
mainGif path flag = do
    gif_bs <- BS.readFile path
    either_img <- readGifImages path
    case either_img of
        Left s -> die s
        Right dynamic_imgs -> do
            num_threads <- getNumCapabilities

            factor <- getInput @Double "enhancement factor"

            blur_method <- case flag of
                    "G" -> do
                        sigma <- getInput @Double "sigma"
                        return $ gaussianBlur sigma 1
                    "M" -> do
                        radius <- getInput @Int "radius"
                        return $ medianBlur radius 1
                    _ -> die "No such method"

            let imgs = map convertRGB8 dynamic_imgs
                imgs_chunks = splitIntoParts num_threads imgs
                sharp_imgs_chunks = parMap rdeepseq (sharpenChunk factor blur_method) imgs_chunks
                sharp_imgs = concat sharp_imgs_chunks

            case getDelaysGifImages gif_bs of
                Left s -> die s
                Right delays ->
                    case writeGifAnimation "images/final.gif" (head delays) LoopingForever sharp_imgs of
                        Left s -> die s
                        Right a -> a

mainImg :: String -> String -> Bool -> IO ()
mainImg path flag verbose = do
    either_img <- readImage path
    case either_img of
        Left s -> die s
        Right dynamic_img -> do
            let img = convertRGB8 dynamic_img

            num_threads <- getNumCapabilities

            factor <- getInput @Double "enhancement factor"

            blur_method <- case flag of
                    "G" -> do
                        sigma <- getInput @Double "sigma"
                        return $ gaussianBlur sigma num_threads
                    "M" -> do
                        radius <- getInput @Int "radius"
                        return $ medianBlur radius num_threads
                    _ -> die "No such method"

            if verbose
                then sharpenVerbose factor blur_method img
                else genAndSave_ (sharpen factor blur_method img) "final.png"

main :: IO ()
main = do
    args <- System.Environment.getArgs
    path <- case args of
            (path_:_) -> return path_
            _ -> die "Usage: ./hpip <path>"

    putStrLn "G — Gaussian kernel"
    putStrLn "M — Median filter"
    putStrLn "Choose method:"
    flag <- getLine

    if isGif args
        then mainGif path flag
        else mainImg path flag ("--verbose" `elem` args)
