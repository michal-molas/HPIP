import Codec.Picture
import System.Environment (getArgs)
import System.Exit (die)

import qualified GaussianBlur as GB
import qualified MedianBlur as MB
import Utils

genAndSave :: Image PixelRGB8 -> String -> IO (Image PixelRGB8)
genAndSave img name = do
    savePngImage ("images/" ++ name) (ImageRGB8 img)
    return img

genAndSave_ :: Image PixelRGB8 -> String -> IO ()
genAndSave_ img name = savePngImage ("images/" ++ name) (ImageRGB8 img)

sharpenGauss :: DynamicImage -> IO ()
sharpenGauss dynamic_img = do
    putStrLn "Input sigma:"
    sigmastr <- getLine
    let sigma = read sigmastr :: Double
    putStrLn "Input enhancement factor:"
    factorstr <- getLine
    let factor = read factorstr :: Pixel8

    original <- genAndSave (convertRGB8 dynamic_img) "original.png"
    blurred <- genAndSave (GB.convolve sigma original) "blurred.png"
    diff <- genAndSave (subtractImages original blurred) "diff.png"
    enhanced_diff <- genAndSave (pixelMap (enhancePixel factor) diff) "enhanced_diff.png"
    genAndSave_ (addImages original enhanced_diff) "final.png"

sharpenMedian :: DynamicImage -> IO ()
sharpenMedian dynamic_img = do
    putStrLn "Input radius:"
    radiusstr <- getLine
    let radius = read radiusstr :: Int
    putStrLn "Input enhancement factor:"
    factorstr <- getLine
    let factor = read factorstr :: Pixel8

    original <- genAndSave (convertRGB8 dynamic_img) "original.png"
    blurred <- genAndSave (MB.convolve radius original) "blurred.png"
    diff <- genAndSave (subtractImages original blurred) "diff.png"
    enhanced_diff <- genAndSave (pixelMap (enhancePixel factor) diff) "enhanced_diff.png"
    genAndSave_ (addImages original enhanced_diff) "final.png"

main :: IO ()
main = do
    args <- System.Environment.getArgs
    (path, flag) <- case args of
            [path_, flag_] -> return (path_, flag_)
            _ -> die "Usage: ./hpip <path> <flag>"

    either_img <- readImage path
    case either_img of
        Left s -> die s
        Right dynamic_img -> do
            case flag of
                "G" -> sharpenGauss dynamic_img
                "M" -> sharpenMedian dynamic_img
                _ -> die "No such option"