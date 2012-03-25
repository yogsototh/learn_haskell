
<div style="display:none">

This code is mostly the same as the preceeding one.

> import Debug.Trace (trace)
> import Data.List
> data BinTree a = Empty 
>                  | Node a (BinTree a) (BinTree a) 
>                   deriving (Eq,Ord)

> -- declare BinTree a to be an instance of Show
> instance (Show a) => Show (BinTree a) where
>   -- will start by a '<' before the root
>   -- and put a : a begining of line
>   show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
>     where
>     treeshow pref Empty = ""
>     treeshow pref (Node x Empty Empty) = 
>                   (pshow pref x)
> 
>     treeshow pref (Node x left Empty) = 
>                   (pshow pref x) ++ "\n" ++
>                   (showSon pref "`--" "   " left)
> 
>     treeshow pref (Node x Empty right) = 
>                   (pshow pref x) ++ "\n" ++
>                   (showSon pref "`--" "   " right)
> 
>     treeshow pref (Node x left right) = 
>                   (pshow pref x) ++ "\n" ++
>                   (showSon pref "|--" "|  " left) ++ "\n" ++
>                   (showSon pref "`--" "   " right)
> 
>     -- show a tree using some prefixes to make it nice
>     showSon pref before next t = 
>                   pref ++ before ++ treeshow (pref ++ next) t
> 
>     -- pshow replace "\n" by "\n"++pref
>     pshow pref x = replace '\n' ("\n"++pref) (" " ++ show x)
> 
>     -- replace on char by another string
>     replace c new string =
>       concatMap (change c new) string
>       where
>           change c new x 
>               | x == c = new
>               | otherwise = x:[] -- "x"
> 
> treeTakeDepth _ Empty = Empty
> treeTakeDepth 0 _     = Empty
> treeTakeDepth n (Node x left right) = let
>           nl = treeTakeDepth (n-1) left
>           nr = treeTakeDepth (n-1) right
>           in
>               Node x nl nr

</div>

Suppose we don't mind having an ordered binary tree.
Here is an infinite binary tree:

> voidTree = Node 0 voidTree voidTree

A complete binary tree were each node is equal to 0:

<code class="haskell">
main = print $ treeTakeDepth 4 voidTree
</code>

And yes, this code run and stop giving the following result:

~~~
<  0
: |-- 0
: |  |-- 0
: |  |  |-- 0
: |  |  `-- 0
: |  `-- 0
: |     |-- 0
: |     `-- 0
: `-- 0
:    |-- 0
:    |  |-- 0
:    |  `-- 0
:    `-- 0
:       |-- 0
:       `-- 0
~~~

But we could also make a more interresting tree here:

> iTree = Node 0 (dec iTree) (inc iTree)
>               where
>                  dec (Node x l r) = Node (x-1) (dec l) (dec r) 
>                  inc (Node x l r) = Node (x+1) (inc l) (inc r) 

But don't you remark this kind of construction look a lot like `map`?
Execpt it works for `BinTree` instead of lists? 
We then create the `treeMap` function:

> treeMap :: (a -> b) -> BinTree a -> BinTree b
> treeMap f Empty = Empty
> treeMap f (Node x left right) = Node (f x) 
>                                      (treeMap f left) 
>                                      (treeMap f right)

_Hint_: I won't talk more about this here. 
But there is a pattern for the generalization of `map` to other data structures. 
Look at Functor and `fmap` if you are interrested.

Our definition is now:

> infTreeTwo :: BinTree Int
> infTreeTwo = Node 0 (treeMap (\x -> x-1) infTreeTwo) 
>                     (treeMap (\x -> x+1) infTreeTwo) 

here is the result for 

<code class="haskell">
main = print $ treeTakeDepth 4 infTreeTwo
</code>

~~~
<  0
: |-- -1
: |  |-- -2
: |  |  |-- -3
: |  |  `-- -1
: |  `-- 0
: |     |-- -1
: |     `-- 1
: `-- 1
:    |-- 0
:    |  |-- -1
:    |  `-- 1
:    `-- 2
:       |-- 1
:       `-- 3
~~~

> main = do
>   print $ treeTakeDepth 4 voidTree
>   print $ treeTakeDepth 4 infTreeTwo
