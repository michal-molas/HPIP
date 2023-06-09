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

convoluteHorizontal :: Double -> Int -> Image PixelRGB8 -> Int -> Image PixelRGB8
convoluteHorizontal sigma num_sub img sub_idx =
    let width = imageWidth img
        height = imageHeight img
        extra_pixels = if sub_idx == num_sub - 1 then mod height num_sub else 0
        sub_height = div height num_sub
        
        kernel = gaussianKernel sigma
        kernel_size = length kernel
    in generateImage (convolutePixel kernel_size kernel sub_height) width (sub_height + extra_pixels)
    where
        convolutePixel :: Int -> [Double] -> Int -> Int -> Int -> PixelRGB8
        convolutePixel kernel_size kernel sub_height x y =
            let center = div (kernel_size - 1) 2
                x_ = x - center
                y_ = sub_idx * sub_height + y
                (r, g, b) = convolutePixelHelp kernel x_ y_
            in PixelRGB8 (castToPixel8 r) (castToPixel8 g) (castToPixel8 b)

        convolutePixelHelp :: [Double] -> Int -> Int -> (Double, Double, Double)
        convolutePixelHelp [] _ _ = (0.0, 0.0, 0.0)
        convolutePixelHelp (a:kernel) x y =
            let width = imageWidth img
                height = imageHeight img
                PixelRGB8 r g b = pixelAt img (adjustIdx width x) (adjustIdx height y)
                (r', g', b') = convolutePixelHelp kernel (x + 1) y
            in (a * fromIntegral r + r', a * fromIntegral g + g', a * fromIntegral b + b')

convoluteVertical :: Double -> Int -> Image PixelRGB8 -> Int -> Image PixelRGB8
convoluteVertical sigma num_sub img sub_idx =
    let width = imageWidth img
        height = imageHeight img
        extra_pixels = if sub_idx == num_sub - 1 then mod height num_sub else 0
        sub_height = div height num_sub
        
        kernel = gaussianKernel sigma
        kernel_size = length kernel
    in generateImage (convolutePixel kernel_size kernel sub_height) width (sub_height + extra_pixels)
    where
        convolutePixel :: Int -> [Double] -> Int -> Int -> Int -> PixelRGB8
        convolutePixel kernel_size kernel sub_height x y =
            let center = div (kernel_size - 1) 2
                x_ = x
                y_ = sub_idx * sub_height + y - center
                (r, g, b) = convolutePixelHelp kernel x_ y_
            in PixelRGB8 (castToPixel8 r) (castToPixel8 g) (castToPixel8 b)

        convolutePixelHelp :: [Double] -> Int -> Int -> (Double, Double, Double)
        convolutePixelHelp [] _ _ = (0.0, 0.0, 0.0)
        convolutePixelHelp (a:kernel) x y =
            let width = imageWidth img
                height = imageHeight img
                PixelRGB8 r g b = pixelAt img (adjustIdx width x) (adjustIdx height y)
                (r', g', b') = convolutePixelHelp kernel x (y + 1)
            in (a * fromIntegral r + r', a * fromIntegral g + g', a * fromIntegral b + b')
        