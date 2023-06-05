module Median where

import Codec.Picture
import Data.List
import Utils

getWindowMedian :: Int -> Image PixelRGB8 -> Int -> Int -> PixelRGB8
getWindowMedian radius img center_x center_y =
    let start_x = center_x - radius
        start_y = center_y - radius
        side = 2 * radius + 1
    in windowMedian $ go start_x start_y 0 side
    where
        go :: Int -> Int -> Int -> Int -> ([Pixel8], [Pixel8], [Pixel8])
        go start_x start_y idx side
            | idx == side * side = ([], [], [])
            | otherwise =
                let x = start_x + mod idx side
                    y = start_y + div idx side
                    PixelRGB8 r g b = defaultPixelAt (PixelRGB8 0 0 0) img x y
                    (rrest, grest, brest) = go start_x start_y (idx + 1) side
                in (r:rrest, g:grest, b:brest)

        windowMedian :: ([Pixel8], [Pixel8], [Pixel8]) -> PixelRGB8
        windowMedian (rs, gs, bs) =
            let sorted_rs = sort rs
                sorted_gs = sort gs
                sorted_bs = sort bs
                mid = 2 * radius * (radius + 1)
            in PixelRGB8 (sorted_rs !! mid) (sorted_gs !! mid) (sorted_bs !! mid)

convolute :: Int -> Image PixelRGB8 -> Image PixelRGB8
convolute radius img =
    let width = imageWidth img
        height = imageHeight img
    in generateImage (getWindowMedian radius img) width height