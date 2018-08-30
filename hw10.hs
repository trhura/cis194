
import AParser
import Data.Char
import Control.Applicative

first :: (a -> b) -> Maybe (a,c) -> Maybe (b,c)
first fn Nothing = Nothing
first fn (Just (x, y)) = Just (fn x, y)

instance Functor Parser where
    fmap fn parser = Parser { runParser =  (first fn) . (runParser parser) }

instance Applicative Parser where
    pure a = Parser { runParser = \s -> Just (a, s) }
    p1 <*> p2 = Parser {
        runParser =  \s -> case (runParser p1 s) of
                                Nothing -> Nothing
                                Just (fn, xs) -> first fn (runParser p2 xs)
        }

abParser :: Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> (char 'a') <*> (char 'b')

ignoreParser :: Parser a -> Parser ()
ignoreParser  = fmap (const ())

spaceParser :: Parser ()
spaceParser = ignoreParser (char ' ')

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> spaceParser <*> posInt

instance Alternative Parser where
    empty = Parser { runParser = \s -> Nothing }
    p1 <|> p2 = Parser {
        runParser =  \s -> case (runParser p1 s) of
                                Nothing ->  (runParser p2 s)
                                Just (a, xs) ->  Just (a, xs)
        }

intOrUppercase :: Parser ()
intOrUppercase = ignoreParser posInt <|> ignoreParser (satisfy isUpper)