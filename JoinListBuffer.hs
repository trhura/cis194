{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module JoinListBuffer where

import Sized
import Buffer
import Data.Char
import Data.List

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _) =  m
tag (Append m _ _) = m

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a Empty = a
(+++) Empty a = a
(+++) a b = Append ((tag a) <> (tag b)) a b

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ 0 (Single _ a)   = Just a
indexJ _ (Single _ a)   = Nothing
indexJ n (Append s l r)
    | n > (getSize . size) s = Nothing
    | otherwise = case (indexJ n l) of
                        Just a -> Just a
                        Nothing -> indexJ (n - (getSize . size . tag) l) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty   = Empty
dropJ 0 j       = j
dropJ _ (Single _ _) = Empty
dropJ n (Append s l r)
    | n > (getSize . size) s = Empty
    | n > (getSize . size . tag) l = dropJ (n - (getSize . size . tag) l) r
    | otherwise = dropJ n l +++ r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ _ Empty   = Empty
takeJ 0 j       = Empty
takeJ _ j@(Single _ _) = j
takeJ n j@(Append s l r)
    | n > (getSize . size) s = j
    | n > (getSize . size . tag) l = l +++ takeJ (n - (getSize . size . tag) l) r
    | otherwise = takeJ n l

newtype Score = Score Int
    deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Semigroup Score where
    (<>) = (+)

instance Monoid Score where
    mempty  = Score 0
    mappend = (+)

score :: Char -> Score
score c = case (toLower c) of
    'a' -> 1
    'e' -> 1
    'i' -> 1
    'o' -> 1
    'u' -> 1
    'l' -> 1
    'n' -> 1
    's' -> 1
    't' -> 1
    'r' -> 1
    'd' -> 2
    'g' -> 2
    'b' -> 3
    'c' -> 3
    'm' -> 3
    'p' -> 3
    'f' -> 4
    'h' -> 4
    'v' -> 4
    'w' -> 4
    'y' -> 4
    'k' -> 5
    'j' -> 8
    'x' -> 8
    'q' -> 10
    'z' -> 10
    _ -> 0

scoreString :: String -> Score
scoreString = sum . (map score)

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

bufferLine :: String -> JoinList (Score, Size) String
bufferLine s = Single (scoreString s, 1) s

jlfromLines :: [String] -> JoinList (Score, Size) String
jlfromLines [] = Empty
jlfromLines (x:[]) = bufferLine x
jlfromLines xs = jlfromLines r +++ jlfromLines l
    where mid = ceiling ((fromIntegral . length) xs / 2)
          (r, l) = splitAt mid xs

jltoLines :: JoinList (Score, Size) String -> [String]
jltoLines Empty = []
jltoLines (Single _ s) = [s]
jltoLines (Append _ l r) = jltoLines l ++ jltoLines r

instance Buffer (JoinList (Score, Size) String) where
    toString = unlines . jltoLines
    fromString = jlfromLines . lines
    line = indexJ
    numLines Empty = 0
    numLines (Single (_, s) _) = getSize s
    numLines (Append (_, s) _ _) = getSize s
    value Empty = 0
    value (Single (v, _) _) = getScore v
    value (Append (v, _) _ _) = getScore v
    replaceLine n l b
        | n >= (getSize . size. tag) b = b
        | otherwise = takeJ n b +++ bufferLine l +++ dropJ (n+1) b


