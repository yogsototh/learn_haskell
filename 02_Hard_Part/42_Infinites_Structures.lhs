
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

This shuffle function as two properties the preceeding one didn't had:

- It contains both positive and negative numbers
- It (hopefully) don't have an upper nor lower bound.

But having a better shuffle list isn't enough not to enter an infinite loop.

Generally, we cannot decide whether `filter (<x) xs` is empty.
Then to resolve this problem, I'll authorize some error in the creation of our binary tree.
This new version of code can create binary tree which don't have the following property for some of its nodes: 

 > Any element of the left (resp. right) branch must all be strictly inferior (resp. superior) to the label of the root.

Remark it will remains _mostly_ an ordered binary tree.

Here is our new version of `treeFromList`. We simply have replaced `filter` by `safefilter`.

> treeFromList :: (Ord a, Show a) => [a] -> BinTree a
> treeFromList []    = Empty
> treeFromList (x:xs) = Node x left right
>           where 
>               left = treeFromList $ safefilter (<x) xs
>               right = treeFromList $ safefilter (>x) xs

This new function `safefilter` is almost equivalent to `filter` but don't enter infinite loop if the result is a finite list.
If it cannot find an element for which the test is true after 10000 consecutive steps, then it considers to be the end of the search.

> safefilter :: (a -> Bool) -> [a] -> [a]
> safefilter f l = safefilter' f l nbTry
>   where
>       nbTry = 10000
>       safefilter' _ _ 0 = []
>       safefilter' _ [] _ = []
>       safefilter' f (x:xs) n = 
>                   if f x 
>                      then x : safefilter' f xs nbTry 
>                      else safefilter' f xs (n-1) 

Now run the program and be happy:

> main = do
>       putStrLn "take 10 shuffle"
>       print $ take 10 shuffle
>       putStrLn "\ntreeTakeDepth 8 (treeFromList shuffle)"
>       print $ treeTakeDepth 8 (treeFromList $ shuffle)

You should realize the time to print each value is different.
This is because Haskell compute each value when it needs it.
And in this case, this is when asked to print it on the screen.

Impressively enough, try to replace the depth from `8` to `100`.
It will work without killing your RAM! 
The flow and the memory management is done naturally by Haskell.

Left as an exercise to the reader:

- I first tried to implement `safefilter` as follow:
  <pre>
  safefilter' f l = if filter f (take 10000 l) == []
                    then []
                    else filter f l
  </pre>
  Explain why it doesn't work and can enter into an infinite loop.
- Let's consider `shuffle` to be a real random list with growing bounds.
  If you study a bit this structure, you'll discover that with probability 1,
  this structure is finite.
  Explain how to make the structure theoretically infinite by using directly `safefilter'` 
  instead of `safefilter` inside `treeFromList`.
  Also what consequences will this modification have one the execution time?
