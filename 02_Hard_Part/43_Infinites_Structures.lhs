
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

Now, suppose we don't mind having an ordered binary tree.
Here is an infinite binary tree:

> infiniTree = Node 0 (subTree (-) 0) (subTree (+) 0)
>       where
>           subTree op n = let 
>                       newVal = op n 1 
>                       in
>                         Node newVal 
>                              (subTree (-) newVal) 
>                              (subTree (+) newVal) 

<code class="haskell">
main = print $ treeTakeDepth 5 infiniTree
</code>

But don't you remark this kind of construction look a lot like `map`?
Execpt it works for `BinTree` and not only for list.
We then create the `treeMap` function:

> treeMap :: (a -> b) -> BinTree a -> BinTree b
> treeMap f Empty = Empty
> treeMap f (Node x left right) = Node (f x) 
>                                      (treeMap f left) 
>                                      (treeMap f right)

And our definition is better.

> infiniTreeTwo :: BinTree Int
> infiniTreeTwo = Node 0 (treeMap (\x -> x-1) infiniTreeTwo) 
>                        (treeMap (\x -> x+1) infiniTreeTwo) 
>
> main = print $ treeTakeDepth 5 infiniTreeTwo

_Hint_: I won't talk more about this here. 
But there is a pattern for the generalization of `map` to other data structures. 
Look at Functor and `fmap`.
