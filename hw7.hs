import Sized

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



