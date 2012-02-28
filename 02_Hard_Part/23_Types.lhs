Just for the fun, as it is tedious to write down a tree like we did, here is another way.
Suppose we want to declare binary tree (this is the standard example).
A first things to note is we cannot use any type for its container. 
We should be able to compare two elements.
As with `Num a` is a type class, there is a type class for type which can compare.
This type class is `(Ord a)`.
Now:

> data BinTree a = (Ord a) => Leaf
>                            | Node a (BinTree a) (BinTree a)

I personnaly had difficulty to understand very well this notation and structure.
Then I will write it in English.

`BinTree` is a type which take another type as parameter.

A `BinTree` can be a `Leaf`. The `Leaf` type does not exists, in fact `Leaf` is a type constructor. As `Leaf` has no argument it can be considered simply as a name.
On the other case, a `BinTree a` can be a:

~~~
Node a (BinTree a) (BinTree a)
~~~

Understand that `Node` is a type construction. It is a function of type:

~~~
Node :: a -> BinTree a -> BinTree a -> BinTree a
~~~

For example, 

~~~
Node 3 Leaf Leaf ⇔ the binary tree 3 without child.
Node 3 (Node 0 Leaf Leaf) Leaf ⇔ the binary tree 
                                      3
                                     /
                                    4
~~~

Now let's create a function to add an element to a `BinTree`.

> treeInsert :: a -> BinTree a -> BinTree a
> treeInsert x Leaf = Node x Leaf Leaf
> treeInsert x (Node y left right) 
>           | x == y = (Node y left right)
>           | x < y  = (Node y (treeInsert x left) right)
>           | otherwise = (Node y left (treeInsert x right))

Now try this.

> main = foldr treeInsert [7,8,1,3,6]
