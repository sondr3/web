module Utils where

import Prelude

import Data.String (joinWith, split, toLower, trim)
import Data.String.Pattern (Pattern(..))
import Data.String.Regex (replace)
import Data.String.Regex.Flags (global, ignoreCase)
import Data.String.Regex.Unsafe (unsafeRegex)

-- | Strip any characters that are not ascii lowercase letters or numbers.
strip :: String -> String
strip = replace (unsafeRegex "[^\\d+a-z]+" (global <> ignoreCase)) ""

-- | Strip any repeating dashes from a string.
stripDash :: String -> String
stripDash = replace (unsafeRegex "--+" global) "-"

-- | Creates a slug for some string.
-- |
-- | ```purescript
-- | >>> slugify "foo bar baz"
-- | "foo-bar-baz"
-- | >>> slugify "fo''o b%$!ar baz,  " 
-- | "foo-bar-baz"
-- | ```
slugify :: String -> String
slugify slug = stripDash <<< joinWith "-" <<< map strip <<< split (Pattern " ") <<< toLower $ trim slug