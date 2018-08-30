
import AParser

first :: (a -> b) -> Maybe (a,c) -> Maybe (b,c)
first fn Nothing = Nothing
first fn (Just (x, y)) = Just (fn x, y)

instance Functor Parser where
    fmap fn parser = Parser { runParser =  (first fn) . (runParser parser) }

instance Applicative Parser where
    pure a = Parser { runParser = \s -> Just (a, s) }
    p1 <*> p2 = Parser {
        runParser =  \x -> case (runParser p1 x) of
                                Nothing -> Nothing
                                Just (fn, xs) -> first fn (runParser p2 xs)
        }

abParser :: Parser (Char, Char)
abParser = (,) <$> (char 'a') <*> (char 'b')

abParser_ :: Parser ()
abParser_ = (\x y -> ()) <$> (char 'a') <*> (char 'b')

space :: Parser ()
space = fmap (const ()) (char ' ')

-- inList :: a -> [a]
-- inList a =  a:[]

intPair :: Parser [Integer]
intPair = (\x _ y -> [x, y]) <$> posInt <*> space <*> posInt