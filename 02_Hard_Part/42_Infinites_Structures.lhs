<h4 id="infinite-tree">Infinite Tree</h4>

<div style="display:none">

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

</div>

In order to resolve these problem we will modify slightly our
`treeFromList` and `shuffle` function.

A first problem, is the lack of infinite different number in our implementation of `shuffle`.
We generated only `4331` different numbers.
To resolve this we make a slightly better `shuffle` function.

> shuffle = map rand [1..]
>           where 
>               rand x = ((p x) `mod` (x+c)) - ((x+c) `div` 2)
>               p x = m*x^2 + n*x + o -- some polynome
>               m = 3123    
>               n = 31
>               o = 7641
>               c = 1237

This shuffle function as the property to generate number both positive and negative and further more there is no more bound.

But even with this version of shuffle, there is always a depth such that our function enter an infinite loop.
This is the time to resolve the most complex part. 
Depending of the shuffle implementation we cannot decide whether
`filter (<x) xs` is empty.
Then I will decide the following: 

 > if no element is inferior to `x` for a certain number of the firsts `xs`, we decide that the branch is empty.

The problem is that now, we can't assume that or tree is a coherent binary tree, even if in general it will be the case.
Here is our new version of `treeFromList`

> treeFromList :: (Ord a, Show a) => [a] -> BinTree a
> treeFromList []    = Empty
> treeFromList (x:xs) = Node x left right
>                   where 
>                       sample = take 8 xs
>                       failTest tst = [] == filter tst sample
>                       left = 
>                           trace (
>                                  (show (failTest (<x))) ++ 
>                                  " (<"++(show x) ++") " ++ 
>                                  show sample ++
>                                   if (failTest (<x))
>                                     then "[]"
>                                     else  show $ head $ filter (<x) sample
>                                  ) $ 
>                                 makesubtree (<x)
>                       right = 
>                           trace (
>                                  (show (failTest (>x))) ++ 
>                                  " (>"++(show x) ++") " ++ 
>                                  show sample ++
>                                   if (failTest (>x))
>                                     then "[]"
>                                     else  show $ head $ filter (>x) sample
>                                  ) $ 
>                                 makesubtree (>x)
>                       makesubtree tst = 
>                           if failTest tst 
>                               then Empty 
>                               else treeFromList $ filter tst xs

Look at how elegant this code is. 
Also we use a shuffle function to create pseudo-random number list:


and a function that troncate a tree up to a certain depth

> treeTakeDepth _ Empty = Empty
> treeTakeDepth 0 _     = Empty
> treeTakeDepth n (Node x left right) = let
>           nl = treeTakeDepth (n-1) left
>           nr = treeTakeDepth (n-1) right
>           in
>               Node x nl nr

Now let's see the result:

> main = do
>       putStrLn "take 10 shuffle"
>       print $ take 10 shuffle
>       putStrLn "\ntreeTakeDepth 5 (treeFromList shuffle)"
>       print $ treeTakeDepth 6 (treeFromList $ shuffle)

