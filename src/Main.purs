module Main where

import Prelude

type Color
  = { normal :: Int, bright :: Int }

data ColorType
  = Log
  | Error
  | Debug
  | Info
  | Success

color :: ColorType -> Boolean -> Int
color colorType bright = case colorType of
  Log -> if bright then 97 else 39
  Error -> if bright then 91 else 31
  Debug -> if bright then 93 else 33
  Info -> if bright then 96 else 36
  Success -> if bright then 92 else 32

type Format
  = { start :: Int, end :: Int }

data FormatType
  = Reset
  | Bold
  | Underline
  | Strike

formatStart :: FormatType -> Int
formatStart formatType = case formatType of
  Reset -> 0
  Bold -> 1
  Underline -> 4
  Strike -> 9

formatEnd :: FormatType -> Int
formatEnd formatType = case formatType of
  Reset -> 0
  Bold -> 22
  Underline -> 24
  Strike -> 29

format :: String -> ColorType -> String
format input colorType = start <> input <> reset
  where
  start = "\x1b[" <> (show $ color colorType false) <> "m"
  reset = "\x1b[" <> (show $ formatEnd Reset) <> "m"
