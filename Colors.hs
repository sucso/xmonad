module Colors
  ( base00
  , base01
  , base02
  , base03
  , base04
  , base05
  , base06
  , base07
  , base08
  , base09
  , base0A
  , base0B
  , base0C
  , base0D
  , base0E
  , base0F
  , black
  , gray
  , white
  , brightWhite
  , red
  , orange
  , yellow
  , green
  , cyan
  , blue
  , magenta
  , brown
  , primary
  , secondary
  ) where

import Data.List (dropWhile, dropWhileEnd, elemIndex, find, isPrefixOf)
import Data.Char (isSpace, toLower)
import Data.Map (Map, (!?), fromList)
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Bifunctor

import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

import XMonad.Util.Run

-- Credits to whoever wrote this function (not me)
getFromXRes :: String -> IO (Maybe String)
getFromXRes key = findValue key <$> runProcessWithInput "xrdb" ["-query"] ""
  where
    findValue :: String -> String -> Maybe String
    findValue xresKey xres =
      snd <$> find ((== xresKey) . fst) (mapMaybe splitAtColon (lines xres))

    splitAtColon :: String -> Maybe (String, String)
    splitAtColon str = splitAtTrimming str <$> elemIndex ':' str

    splitAtTrimming :: String -> Int -> (String, String)
    splitAtTrimming str idx = bimap trim (trim . tail) $ splitAt idx str

    trim :: String -> String
    trim = dropWhileEnd isSpace . dropWhile isSpace

fromXRes :: String -> Maybe String
fromXRes = unsafePerformIO . getFromXRes

colors :: IO (Map String String)
colors = do
  {-
    NOTE: this expects "#define" directives to NOT have any excess whitespace:
          (eg.: "  #define base00" and "#define   base00" won't be handled)
    NOTE: using Data.Text would be more efficient,
          but I do not expect to be parsing a lot of data.
  -}
  path <- lookupEnv "XRESOURCES_THEME"
  contents <- case path of
        Just path -> readFile path
        Nothing   -> return ""

  let defines = filter (isPrefixOf "#define base") (lines contents)
  let bases = map (drop $ length "#define ") defines

  let pairs :: [(String, String)]
      pairs = map (split ' ') bases
        where
          split :: Char -> String -> (String, String)
          split char string = (lhs, rhs)
            where
              (lhs, _:rhs) = span (/= char) string
  return $ fromList pairs

myColors :: String -> Maybe String
-- HACK: using unsafePerformIO (don't know how to do otherwise)
myColors color = unsafePerformIO colors !? color

base00    = fromMaybe "#000000" $ myColors "base00"
base01    = fromMaybe "#111111" $ myColors "base01"
base02    = fromMaybe "#222222" $ myColors "base02"
base03    = fromMaybe "#333333" $ myColors "base03"
base04    = fromMaybe "#444444" $ myColors "base04"
base05    = fromMaybe "#555555" $ myColors "base05"
base06    = fromMaybe "#666666" $ myColors "base06"
base07    = fromMaybe "#777777" $ myColors "base07"
base08    = fromMaybe "#888888" $ myColors "base08"
base09    = fromMaybe "#999999" $ myColors "base09"
base0A    = fromMaybe "#AAAAAA" $ myColors "base0A"
base0B    = fromMaybe "#BBBBBB" $ myColors "base0B"
base0C    = fromMaybe "#CCCCCC" $ myColors "base0C"
base0D    = fromMaybe "#DDDDDD" $ myColors "base0D"
base0E    = fromMaybe "#EEEEEE" $ myColors "base0E"
base0F    = fromMaybe "#FFFFFF" $ myColors "base0F"

black        = base00
gray         = base03
white        = base05
brightWhite  = base07
red          = base08
orange       = base09
yellow       = base0A
green        = base0B
cyan         = base0C
blue         = base0D
magenta      = base0E
brown        = base0F

primary   = fromMaybe "#F0F080" $ fromXRes "*primaryColor"
secondary = fromMaybe "#F08000" $ fromXRes "*secondaryColor"
