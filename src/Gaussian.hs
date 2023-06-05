module GaussianBlur where

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

convolve :: Double -> Image PixelRGB8 -> Image PixelRGB8
convolve sigma img =
    let width = imageWidth img
        height = imageHeight img
        gauss = gaussianKernel sigma
        gauss_size = length gauss
        horizontal = generateImage (convolveHorizontal gauss_size gauss img) width height
            in generateImage (convolveVertical gauss_size gauss horizontal) width height