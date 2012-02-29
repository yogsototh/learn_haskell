### Recurive type

You already encountered recursive types.
Typically, you can re-create lists, but with a more verbose syntax:

~~~
data List a = Empty | Cons a (List a)
~~~

If you really want to use an easier syntax you can use infix name for constructors.

> data List a = <> | a >< List a

> data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

#### Trees

> import Data.List
>
> data BinTree a = Empty 
>                  | Node a (BinTree a) (BinTree a) 
>                               deriving (Show)

Now let's create a function to add an element to a `BinTree`.

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

This is an informative but quite unpleasant represntation of our tree.
