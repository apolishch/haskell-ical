{-# LANGUAGE OverloadedStrings #-}


module Text.ICal where


import Control.Applicative
import Text.Trifecta
import Data.Char
import Text.ICal.Params
import Text.ICal.Combinators


data ContentLine =
  ContentLine { name :: String
              , params :: [Param]
              , value :: String
              }
  deriving (Show, Eq, Ord)


type Schedule =
  [ContentLine]



schedule :: Parser Schedule
schedule = do
  result <- contentLine `sepEndBy` crlf
  eof
  return result


contentLine :: Parser ContentLine
contentLine = do
  lineName <- nameParser
  params <- many param
  char ':'
  result <- valueParser
  return
    ContentLine { name=lineName
                , params=params
                , value=result
                }



param :: Parser Param
param = do
  char ';'
  paramName <- nameParser
  char '='
  paramValue $ map toUpper paramName


valueParser :: Parser String
valueParser =
  many (unfold (noneOf nonTextual))




