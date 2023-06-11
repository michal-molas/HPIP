module Utils where

import Codec.Picture

adjustIdx :: Int -> Int -> Int
adjustIdx max_border idx
    | idx < 0 = 0
    | idx >= max_border = max_border - 1
    | otherwise = idx

safeAdd :: Pixel8 -> Pixel8 -> Pixel8
safeAdd p1 p2 = if p1 > 255 - p2 then 255 else p1 + p2

safeSub :: Pixel8 -> Pixel8 -> Pixel8
safeSub p1 p2 = if p2 > p1 then 0 else p1 - p2

safeMul :: Pixel8 -> Double -> Pixel8
safeMul p x = let px = x * fromIntegral p in if px > 255.0 then 255 else castToPixel8 px

castToPixel8 :: Double -> Pixel8
castToPixel8 value = clamp (round value)
  where
    clamp :: Int -> Pixel8
    clamp val
      | val < 0 = 0
      | val > 255 = 255
      | otherwise = fromIntegral val

subtractImages :: Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
subtractImages img1 img2 =
    let f = (\x y ->
                let PixelRGB8 r g b = pixelAt img1 x y
                    PixelRGB8 r' g' b' = pixelAt img2 x y
                in PixelRGB8 (safeSub r r') (safeSub g g') (safeSub b b'))
    in generateImage f (imageWidth img1) (imageHeight img1)

addImages :: Image PixelRGB8 -> Image PixelRGB8 -> Image PixelRGB8
addImages img1 img2 =
    let f = (\x y ->
                let PixelRGB8 r g b = pixelAt img1 x y
                    PixelRGB8 r' g' b' = pixelAt img2 x y
                in PixelRGB8 (safeAdd r r') (safeAdd g g') (safeAdd b b'))
    in generateImage f (imageWidth img1) (imageHeight img1)

enhancePixel :: Double -> PixelRGB8 -> PixelRGB8
enhancePixel x (PixelRGB8 r g b) = PixelRGB8 (safeMul r x) (safeMul g x) (safeMul b x)