module Text.ICal.Combinators where


import Control.Applicative
import Text.Trifecta
import Data.Char


crlf :: Parser String
crlf =
  string "\r\n"


wsp :: Parser Char
wsp =
  oneOf " \t"


fws :: Parser String
fws =
  string "\r\n " <|> string "\r\n\t"


unfold :: Parser a -> Parser a
unfold =
  between (optional fws) (optional fws)


nameParser :: Parser String
nameParser =
  xName <|> unfold ianaToken


ianaChar :: Parser Char
ianaChar =
  alphaNum <|> char '-'


ianaToken :: Parser String
ianaToken =
  some (unfold ianaChar)


xName :: Parser String
xName = do
  char 'X' >> char '-'
  rest <- ianaToken
  return ("X-" ++ rest)


controls :: String
controls =
  chr <$> ([0x00..0x08] ++ [0x0A..0x1F] ++ [0x7F])


safeChars :: Parser String
safeChars = many (unfold (noneOf $ controls ++ "\";:,"))


quoteSafeChars :: Parser String
quoteSafeChars = many (unfold (noneOf $ controls ++ "\""))


betweenDquotes :: Parser a -> Parser a
betweenDquotes = between (char '"') (char '"')


nonTextual :: String
nonTextual =
  chr <$> [0x00..0x08] ++ [0x0A..0x1F] ++ [0x7F..0x9F] ++ [0xA1..0xFF]

