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
convoluteHorizontalHelp (a:kernel) img x y =
    let width = imageWidth img
        height = imageHeight img
        PixelRGB8 r g b = pixelAt img (adjustIdx width x) (adjustIdx height y)
        (r', g', b') = convoluteHorizontalHelp kernel img (x + 1) y
    in (a * fromIntegral r + r', a * fromIntegral g + g', a * fromIntegral b + b')

convolutePixelHorizontal :: Int -> [Double] -> Int -> Int -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
convolutePixelHorizontal kernel_size kernel sub_idx sub_height img x y =
    let center = div (kernel_size - 1) 2
        x_ = x - center
        y_ = sub_idx * sub_height + y
        (r, g, b) = convoluteHorizontalHelp kernel img x_ y_
    in PixelRGB8 (castToPixel8 r) (castToPixel8 g) (castToPixel8 b)

convoluteVerticalHelp :: [Double] -> Image PixelRGB8 -> Int -> Int -> (Double, Double, Double)
convoluteVerticalHelp [] _ _ _ = (0.0, 0.0, 0.0)
convoluteVerticalHelp (a:kernel) img x y =
    let width = imageWidth img
        height = imageHeight img
        PixelRGB8 r g b = pixelAt img (adjustIdx width x) (adjustIdx height y)
        (r', g', b') = convoluteVerticalHelp kernel img x (y + 1)
    in (a * fromIntegral r + r', a * fromIntegral g + g', a * fromIntegral b + b')

convolutePixelVertical :: Int -> [Double] -> Int -> Int -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
convolutePixelVertical kernel_size kernel sub_idx sub_height img x y =
    let center = div (kernel_size - 1) 2
        x_ = x
        y_ = sub_idx * sub_height + y - center
        (r, g, b) = convoluteVerticalHelp kernel img x_ y_
    in PixelRGB8 (castToPixel8 r) (castToPixel8 g) (castToPixel8 b)

convoluteHorizontal :: Double -> Int -> Image PixelRGB8 -> Int -> Image PixelRGB8
convoluteHorizontal sigma num_sub img sub_idx =
    let width = imageWidth img
        height = imageHeight img
        extra_pixels = mod height num_sub
        sub_height = div height num_sub + if sub_idx == num_sub - 1 then extra_pixels else 0
        
        kernel = gaussianKernel sigma
        kernel_size = length kernel
    in generateImage (convolutePixelHorizontal kernel_size kernel sub_idx sub_height img) width sub_height

convoluteVertical :: Double -> Int -> Image PixelRGB8 -> Int -> Image PixelRGB8
convoluteVertical sigma num_sub img sub_idx =
    let width = imageWidth img
        height = imageHeight img
        extra_pixels = mod height num_sub
        sub_height = div height num_sub + if sub_idx == num_sub - 1 then extra_pixels else 0
        
        kernel = gaussianKernel sigma
        kernel_size = length kernel
    in generateImage (convolutePixelVertical kernel_size kernel sub_idx sub_height img) width sub_height