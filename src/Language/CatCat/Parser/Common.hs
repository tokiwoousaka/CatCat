module Language.CatCat.Parser.Common where
import Control.Applicative
import Text.Trifecta

character :: Parser Char
character = letter <|> digit

word :: Parser a -> Parser a
word p = spaces *> p <* spaces

many1 :: Parser a -> Parser [a]
many1 p = (:) <$> p <*> many p

----------

uWord :: Parser String
uWord = word $ (:) <$> upper <*> many character

lWord :: Parser String
lWord = word $ (:) <$> lower <*> many character
