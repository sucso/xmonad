module XResources
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
  ) where

import Data.List (isPrefixOf, dropWhileEnd)
import Data.Char (toLower)
import Data.Map (Map, (!?), fromList)
import Data.Maybe (fromMaybe)

import System.Environment (lookupEnv)
import System.IO.Unsafe (unsafePerformIO)

xresColors :: IO (Map String String)
xresColors = do
  {-
    NOTE: this expects "#define" directives to NOT have any excess whitespace:
          (eg.: "  #define base00" and "#define   base00" won't be parsed)
    NOTE: using Data.Text would be more efficient,
          but I do not expect to be parsing a lot of data.
  -}
  xresPath <- lookupEnv "XRESOURCES_THEME"
  xresContents <- case xresPath of
        Just path -> readFile path
        Nothing -> return ""

  let xresLines = lines xresContents
  let xresDefines = filter (isPrefixOf "#define base") xresLines
  let xresBases = map (drop $ length "#define ") xresDefines

  let xresPairs :: [(String, String)]
      xresPairs = map (split ' ') xresBases
        where
          split :: Char -> String -> (String, String)
          split char string = (lhs, rhs)
            where
              (lhs, _:rhs) = span (/= char) string

  return $ fromList xresPairs

myColors :: String -> Maybe String
-- HACK: using unsafePerformIO (don't know how to do otherwise)
myColors color = unsafePerformIO xresColors !? color

base00 = fromMaybe "#000000" $ myColors "base00"
base01 = fromMaybe "#111111" $ myColors "base01"
base02 = fromMaybe "#222222" $ myColors "base02"
base03 = fromMaybe "#333333" $ myColors "base03"
base04 = fromMaybe "#444444" $ myColors "base04"
base05 = fromMaybe "#555555" $ myColors "base05"
base06 = fromMaybe "#666666" $ myColors "base06"
base07 = fromMaybe "#777777" $ myColors "base07"
base08 = fromMaybe "#888888" $ myColors "base08"
base09 = fromMaybe "#999999" $ myColors "base09"
base0A = fromMaybe "#AAAAAA" $ myColors "base0A"
base0B = fromMaybe "#BBBBBB" $ myColors "base0B"
base0C = fromMaybe "#CCCCCC" $ myColors "base0C"
base0D = fromMaybe "#DDDDDD" $ myColors "base0D"
base0E = fromMaybe "#EEEEEE" $ myColors "base0E"
base0F = fromMaybe "#FFFFFF" $ myColors "base0F"
