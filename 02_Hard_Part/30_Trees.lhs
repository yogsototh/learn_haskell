<h4 id="trees">Trees</h4>

Now we'll just give another typical example, binary trees.

> import Data.List
>
> data BinTree a = Empty 
>                  | Node a (BinTree a) (BinTree a) 
>                               deriving (Show)

Also we create a function which transform a list into a binary tree.

> treeFromList :: (Ord a) => [a] -> BinTree a
> treeFromList [] = Empty
> treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
>                              (treeFromList (filter (>x) xs))

Look at how elegant this function is.
In plain English: 

- an empty list will be converted to an empty tree.
- a list `(x:xs)` will be converted to the tree where:
  - The root is `x`
  - Its left subtree is the tree created from the list of the remaining element of `xs` which are strictly inferior to `x` and 
  - the right subtree is the tree created from the elements strictly superior to `x` of the list `xs`.

> main = print $ treeFromList [7,2,4,8]

You should obtain the following:

~~~
Node 7 (Node 2 Empty (Node 4 Empty Empty)) (Node 8 Empty Empty)
~~~

This is an informative but quite unpleasant representation of our tree.
