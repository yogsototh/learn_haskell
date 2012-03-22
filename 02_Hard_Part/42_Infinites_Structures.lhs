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
> 
> treeTakeDepth _ Empty = Empty
> treeTakeDepth 0 _     = Empty
> treeTakeDepth n (Node x left right) = let
>           nl = treeTakeDepth (n-1) left
>           nr = treeTakeDepth (n-1) right
>           in
>               Node x nl nr

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

 > if no element is inferior to `x` for the firsts `xs` element, we decide that the branch is empty.

The problem is that now, we can't assume that or tree is a coherent binary tree, even if in general it will be the case.

Here is our new version of `treeFromList`. We simply have replaced `filter` by `safefilter`.

> treeFromList :: (Ord a, Show a) => [a] -> BinTree a
> treeFromList []    = Empty
> treeFromList (x:xs) = Node x left right
>                   where 
>                       left = treeFromList $ safefilter (<x) xs
>                       right = treeFromList $ safefilter (>x) xs

This new function `safefilter` is almost equivalent to `filter`.
If it cannot find an element for which the test is true after 1000 consecutive steps,
then it considers to be the end of the search.

> safefilter :: (a -> Bool) -> [a] -> [a]
> safefilter f l = safefilter' f l nbTry
>   where
>       nbTry = 1000
>       safefilter' _ _ 0 = []
>       safefilter' _ [] _ = []
>       safefilter' f (x:xs) n = if f x 
>                                then x : safefilter' f xs nbTry 
>                                else safefilter' f xs (n-1) 

Now let's see the result:

> main = do
>       putStrLn "take 10 shuffle"
>       print $ take 10 shuffle
>       putStrLn "\ntreeTakeDepth 5 (treeFromList shuffle)"
>       print $ treeTakeDepth 6 (treeFromList $ shuffle)

Left as an exercise to the reader:

- Let's consider `shuffle` to be a real random list with growing bounds.
  If you study a bit this structure, you'll discover that with probability 1,
  this structure is finite.
  Explain how to make the structure infinite by using directly `safefilter'` 
  instead of `safefilter` inside `treeFromList`.
