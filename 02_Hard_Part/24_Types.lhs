Just for the fun, as it is tedious to write down a tree like we did, here is another way.
Suppose we want to declare binary tree (this is the standard example).
A first things to note is we cannot use any type for its container. 
We should be able to compare two elements.
As with `Num a` is a type class, there is a type class for type which can compare.
This type class is `(Ord a)`.
Now:

> data BinTree a = Leaf | Node a (BinTree a) (BinTree a) 
>                    deriving (Read, Eq)

Now let's create a function to add an element to a `BinTree`.

> treeInsert :: (Ord a) => a -> BinTree a -> BinTree a
> treeInsert x Leaf = Node x Leaf Leaf
> treeInsert x (Node y left right) 
>           | x == y = (Node y left right)
>           | x < y  = (Node y (treeInsert x left) right)
>           | otherwise = (Node y left (treeInsert x right))

We can create instance of some kind of typeclass like this:

> instance (Show a) => Show (BinTree a) where
>   show Leaf = ""
>   show (Node x left right) = "(" ++ 
>                              (show x) ++ 
>                              (show left) ++ 
>                              (show right) ++ ")"

Now try this.

> main = print $ foldr treeInsert Leaf [7,8,1,3,6]

~~~
(6(3(1))(8(7)))
~~~
