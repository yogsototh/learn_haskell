And of course you can create recursive type.
Here is a typical binary tree definition:

> data Tree a =   Leaf
>               | Node {
>                  value :: a 
>               ,  left :: Tree a
>               ,  right :: Tree a }

And how you use it. For example a function to sum all element of a tree:

> treeSum :: Num a => Tree a -> a
> treeSum  Leaf =  0
> treeSum  (Node n left right) = 
>               n + (treeSum left) + (treeSum right)

~~~
   1
  / \
 2   3
    /  
   4
~~~

> tree = Node 1 
>       (Node 2 Leaf Leaf) 
>       (Node 3 
>           (Node 4 Leaf Leaf) 
>           Leaf)

> main = print $ treeSum tree

This notation is verbose, but you'll gain more than you lose.
