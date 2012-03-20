data YTree a = Nil | Cons a (YTree a) (YTree a) deriving (Show)

insertTree :: (Ord a) => a -> YTree a -> YTree a
insertTree x Nil = Cons a Nil Nil
insertTree x (Cons y left right) = Cons

fromList :: [a] -> YTree a
fromList [] = Nil
fromList (x:xs) = Cons x (fromList xs) (fromList xs)

take' :: Integer -> YTree a -> YTree a
take' _ Nil = Nil
take' 0 _ = Nil
take' n (Cons x left right) = let 
    nl = take' (n-1) left 
    nr = take' (n-1) right
    in
        Cons x nl nr

main = do
    print $ take' 3 (fromList [1..])
