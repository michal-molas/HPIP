import Codec.Picture
import System.Environment (getArgs)
import System.Exit (die)

import qualified GaussianBlur as GB
import Utils

genAndSave :: Image PixelRGB8 -> String -> IO (Image PixelRGB8)
genAndSave img name = do
    savePngImage ("images/" ++ name) (ImageRGB8 img)
    return img

main :: IO ()
main = do
    args <- System.Environment.getArgs
    (path, sigma, factor) <- case args of
            [path_, sigma_, factor_] -> return (path_, read sigma_ :: Double, read factor_ :: Pixel8)
            _ -> die "No file given"

    either_img <- readImage path
    case either_img of
        Left s -> die s
        Right dynamic_img -> do
            original <- genAndSave (convertRGB8 dynamic_img) "original.png"
            blurred <- genAndSave (GB.convolve sigma original) "blurred.png"
            diff <- genAndSave (subtractImages original blurred) "diff.png"
            enhanced_diff <- genAndSave (pixelMap (enhancePixel factor) diff) "enhanced_diff.png"
            _ <- genAndSave (addImages original enhanced_diff) "final.png"
            return ()