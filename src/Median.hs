module Median where

import Codec.Picture
import Data.List
import Utils

getWindowMedian :: Int -> Int -> Int -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
getWindowMedian radius sub_idx sub_height img center_x center_y =
    let start_x = center_x - radius
        start_y = sub_idx * sub_height + center_y - radius
        side = 2 * radius + 1
    in windowMedian $ go start_x start_y 0 side
    where
        go :: Int -> Int -> Int -> Int -> ([Pixel8], [Pixel8], [Pixel8])
        go start_x start_y idx side
            | idx == side * side = ([], [], [])
            | otherwise =
                let width = imageWidth img
                    height = imageHeight img
                    x = adjustIdx width (start_x + mod idx side)
                    y = adjustIdx height (start_y + div idx side)
                    PixelRGB8 r g b = pixelAt img x y
                    (rrest, grest, brest) = go start_x start_y (idx + 1) side
                in (r:rrest, g:grest, b:brest)

        windowMedian :: ([Pixel8], [Pixel8], [Pixel8]) -> PixelRGB8
        windowMedian (rs, gs, bs) =
            let sorted_rs = sort rs
                sorted_gs = sort gs
                sorted_bs = sort bs
                mid = 2 * radius * (radius + 1)
            in PixelRGB8 (sorted_rs !! mid) (sorted_gs !! mid) (sorted_bs !! mid)

convolute :: Int -> Int -> Image PixelRGB8 -> Int -> Image PixelRGB8
convolute radius num_sub img sub_idx =
    let width = imageWidth img
        extra_pixels = mod (imageHeight img) num_sub
        height = div (imageHeight img) num_sub + if sub_idx == num_sub - 1 then extra_pixels else 0
    in generateImage (getWindowMedian radius sub_idx height img) width height