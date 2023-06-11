module Gaussian where

import Codec.Picture
import Utils

gaussianKernel :: Double -> [Double]
gaussianKernel sigma =
    let size_ = (let x = ceiling (6 * sigma) in if even x then x + 1 else x)
    in go 0 size_
    where
        go :: Int -> Int -> [Double]
        go idx size
            | idx == size = []
            | otherwise =
                let center = div (size - 1) 2
                    diff_x = idx - center
                    sigmasq = sigma * sigma
                    expo = -(fromIntegral (diff_x * diff_x) / (2 * sigmasq))
                    coefficient = 1 / (sigma * sqrt (2 * pi))
                in ((coefficient * exp expo):go (idx + 1) size)

convoluteHorizontalHelp :: [Double] -> Image PixelRGB8 -> Int -> Int -> (Double, Double, Double)
convoluteHorizontalHelp [] _ _ _ = (0.0, 0.0, 0.0)
convoluteHorizontalHelp (a:gauss) img x y =
    let width = imageWidth img
        height = imageHeight img
        PixelRGB8 r g b = pixelAt img (adjustIdx width x) (adjustIdx height y)
        (r', g', b') = convoluteHorizontalHelp gauss img (x + 1) y
    in (a * fromIntegral r + r', a * fromIntegral g + g', a * fromIntegral b + b')

convoluteHorizontal :: Int -> [Double] -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
convoluteHorizontal gauss_size gauss img x y =
    let center = div (gauss_size - 1) 2
        (r, g, b) = convoluteHorizontalHelp gauss img (x - center) y
    in PixelRGB8 (castToPixel8 r) (castToPixel8 g) (castToPixel8 b)

convoluteVerticalHelp :: [Double] -> Image PixelRGB8 -> Int -> Int -> (Double, Double, Double)
convoluteVerticalHelp [] _ _ _ = (0.0, 0.0, 0.0)
convoluteVerticalHelp (a:gauss) img x y =
    let width = imageWidth img
        height = imageHeight img
        PixelRGB8 r g b = pixelAt img (adjustIdx width x) (adjustIdx height y)
        (r', g', b') = convoluteVerticalHelp gauss img x (y + 1)
    in (a * fromIntegral r + r', a * fromIntegral g + g', a * fromIntegral b + b')

convoluteVertical :: Int -> [Double] -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
convoluteVertical gauss_size gauss img x y =
    let center = div (gauss_size - 1) 2
        (r, g, b) = convoluteVerticalHelp gauss img x (y - center)
    in PixelRGB8 (castToPixel8 r) (castToPixel8 g) (castToPixel8 b)

convolute :: Double -> Image PixelRGB8 -> Image PixelRGB8
convolute sigma img =
    let width = imageWidth img
        height = imageHeight img
        gauss = gaussianKernel sigma
        gauss_size = length gauss
        horizontal = generateImage (convoluteHorizontal gauss_size gauss img) width height
            in generateImage (convoluteVertical gauss_size gauss horizontal) width height