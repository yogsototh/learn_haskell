<h4 id="trees">Trees</h4>

blogimage("magritte-l-arbre.jpg","Magritte, l'Arbre")

We'll just give another standard example: binary trees.

> import Data.List
>
> data BinTree a = Empty
>                  | Node a (BinTree a) (BinTree a)
>                               deriving (Show)

We will also create a function which turns a list into an ordered binary tree.

> treeFromList :: (Ord a) => [a] -> BinTree a
> treeFromList [] = Empty
> treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
>                              (treeFromList (filter (>x) xs))

Look at how elegant this function is.
In plain English:

- an empty list will be converted to an empty tree.
- a list `(x:xs)` will be converted to a tree where:
  - The root is `x`
  - Its left subtree is the tree created from members of the list `xs` which are strictly inferior to `x` and
  - the right subtree is the tree created from members of the list `xs` which are strictly superior to `x`.

> main = print $ treeFromList [7,2,4,8]

You should obtain the following:

~~~
Node 7 (Node 2 Empty (Node 4 Empty Empty)) (Node 8 Empty Empty)
~~~

This is an informative but quite unpleasant representation of our tree.
