Just for the fun, as it is tedious to write down a tree like we did, here is another way.
Suppose we want to declare binary tree (this is the standard example).
A first things to note is we cannot use any type for its container. 
We should be able to compare two elements.
As with `Num a` is a type class, there is a type class for type which can compare.
This type class is `(Ord a)`.
Now:

> data BinTree a = (Ord a) => Leaf
>                            | Node a (BinTree a) (BinTree a)

Now let's create a function to add an element to a `BinTree`.

> treeInsert :: a -> BinTree a -> BinTree a
> treeInsert x Leaf = Node x Leaf Leaf
> treeInsert x (Node y left right) 
>           | x == y = (Node y left right)
>           | x < y  = (Node y (treeInsert x left) right)
>           | otherwise = (Node y left (treeInsert x right))

Now try this.

> main = foldr treeInsert [7,8,1,3,6]
