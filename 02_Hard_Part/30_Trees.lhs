### Trees

Now we'll just give another typical example, binary trees.

> import Data.List
>
> data BinTree a = Empty 
>                  | Node a (BinTree a) (BinTree a) 
>                               deriving (Show)

To generate tree easily, we create a function who add an element to a `BinTree`.

> treeInsert :: (Ord a) => BinTree a -> a -> BinTree a
> treeInsert Empty x    = Node x Empty Empty
> treeInsert (Node y left right) x
>           | x == y    = (Node y left right)
>           | x < y     = (Node y (treeInsert left x) right)
>           | otherwise = (Node y left (treeInsert right x))

Now try this:

> main = print $ foldl' treeInsert Empty [7,2,4,8]

You should obtain the following:

~~~
Node 7 (Node 2 Empty (Node 4 Empty Empty)) (Node 8 Empty Empty)
~~~

This is an informative but quite unpleasant representation of our tree.
