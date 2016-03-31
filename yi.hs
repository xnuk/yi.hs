import Data.List (minimumBy)
import Data.Monoid ((<>))

import Data.Bits (shiftR)
import Data.Word (Word8, Word32)

import Data.Prototype (override)

import Yi.Boot (yi)
import Yi.Config
import Yi.Config.Misc (ScrollStyle(SingleLine))
import Yi.Config.Default (defaultVimConfig)

import Yi.Style.Library (Theme, defaultTheme)
import Yi.Style (UIStyle(..), Color(RGB), emptyAttributes, Attributes(..), withFg, withBg)

main :: IO ()
main = yi $ defaultVimConfig {
    configUI = (configUI defaultVimConfig) {
        configScrollStyle = Just SingleLine
      , configScrollWheelAmount = 3
      , configLeftSideScrollBar = False
      , configAutoHideScrollBar = True
      , configAutoHideTabBar = True
      , configLineWrap = True
      , configCursorStyle = FatWhenFocusedAndInserting
      , configWindowFill = ' '
      , configTheme = tomorrowTheme
    }
}

rgb :: Word32 -> Color
rgb x = rgb' (fromIntegral (x `shiftR` 16)) (fromIntegral (x `shiftR` 8)) (fromIntegral x)

apxGreyTone, apxChannelTone :: Integral a => a -> a
apxGreyTone x
    | x <= 3    = 0
    | x >= 245  = 0xff
    | x >= 240  = 0xee
    | otherwise = ((x-3) `div` 10) * 10 + 8

apxChannelTone x
    | x <= 75 = 0
    | otherwise = ((x-75) `div` 40) * 40 + 95

rgb' :: Word8 -> Word8 -> Word8 -> Color
rgb' r g b = if greyError < chanError
                then let a = fromIntegral apxGrey in RGB a a a
                else let [x, y, z] = map fromIntegral chans in RGB x y z
    where rgbs = map fromIntegral [r, g, b] :: [Int]
          greys = map apxGreyTone rgbs
          chans = map apxChannelTone rgbs
          (apxGrey, greyError) = minimumBy (\(_, x) (_, y) -> compare x y) $ do
              a <- greys
              return (a, sum $ map ((^(2 :: Int)) . (subtract a)) rgbs)
          chanError = sum . map (^(2 :: Int)) $ zipWith (-) chans rgbs

tmBackground = rgb 0x1d1f21
tmCurrentLine = rgb 0x282a2e
tmSelection = rgb 0x373b41
tmForeground = rgb 0xc5c8c6
tmComment = rgb 0x969896
tmRed = rgb 0xcc6666
tmOrange = rgb 0xde935f
tmYellow = rgb 0xf0c674
tmGreen = rgb 0xb5bd68
tmAqua = rgb 0x8abeb7
tmBlue = rgb 0x81a2be
tmPurple = rgb 0xb294bb
tmWindow = rgb 0x4d5057
tmBrightWindow = tmWindow

tmBrightBackground = rgb 0x000000
tmBrightCurrentLine = rgb 0x2a2a2a
tmBrightSelection = rgb 0x424242
tmBrightForeground = rgb 0xeaeaea
tmBrightComment = rgb 0x969896
tmBrightRed = rgb 0xd54e53
tmBrightOrange = rgb 0xe78c45
tmBrightYellow = rgb 0xe7c547
tmBrightGreen = rgb 0xb9ca4a
tmBrightAqua = rgb 0x70c0b1
tmBrightBlue = rgb 0x7aa6da
tmBrightPurple = rgb 0xc397d8

tomorrowTheme :: Theme
tomorrowTheme = defaultTheme `override` \sets _ -> sets
    { modelineAttributes = emptyAttributes { foreground = tmBrightWindow, background = tmForeground , reverseAttr = True }
    , modelineFocusStyle = withBg tmBrightYellow
    , tabBarAttributes = emptyAttributes { foreground = tmBrightWindow, background = tmForeground, reverseAttr = True }
--  , tabInFocusStyle
--  , tabNotFocusedStyle
    , baseAttributes = emptyAttributes { foreground = tmForeground, background = tmBackground }
    , selectedStyle = withBg tmBrightSelection
--  , eofStyle
--  , errorStyle
    , hintStyle = withBg tmBrightSelection
    , strongHintStyle = withBg tmBrightYellow <> withFg tmBackground
    , commentStyle = withFg tmBrightComment
    , blockCommentStyle = withFg tmBrightComment
    , keywordStyle = withFg tmBrightOrange
    , numberStyle = withFg tmBrightOrange
    , preprocessorStyle = withFg tmBrightPurple
    , stringStyle = withFg tmBrightGreen
    , longStringStyle = withFg tmBrightGreen
    , typeStyle = withFg tmBrightBlue
    , dataConstructorStyle = withFg tmBrightPurple
    , importStyle = withFg tmBrightBlue
    , builtinStyle = withFg tmBrightAqua -- ?
    , regexStyle = withFg tmBrightGreen
    , variableStyle = withFg tmBrightRed
    , operatorStyle = withFg tmBrightAqua
    , quoteStyle = withFg tmBrightGreen
--  , makeFileAction
--  , makeFileRuleHead
    }
