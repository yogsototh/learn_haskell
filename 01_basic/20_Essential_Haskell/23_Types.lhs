And of course you can create recursive type.
Here is a typical binary tree definition:

> data Tree a =   Leaf
>               | Node {  value :: a 
>                      ,  left  :: Tree a
>                      ,  right :: Tree a }

And how you use it. For example a function to sum all element of a tree:

> treeSum :: Num a => Tree a -> a
> treeSum  Leaf =  0
> treeSum  (Node n left right) = 
>               n + (treeSum left) + (treeSum right)

Here is how we declare this binary tree:

<graph title="The tree">
1 -> 2
1 -> 3
3 -> 4
</graph>

> tree = Node 1 
>       (Node 2 Leaf Leaf) 
>       (Node 3 
>           (Node 4 Leaf Leaf) 
>           Leaf)

This notation is verbose, but you'll gain more than you lose.
Furthermore, generally when you create complex datastructures,
you fill them from some another format using a parser 
or with some generative function.

Finally we print the result:

> main = print $ treeSum tree
