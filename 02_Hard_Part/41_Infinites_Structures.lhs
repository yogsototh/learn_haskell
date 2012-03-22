<h4 id="infinite-tree">Infinite Tree</h4>

For this example, we will use the same code as in the [tree section](#trees). 

<div style="display:none">

This code is mostly the same as the one in the [tree section](#trees).

> import Data.List
> data BinTree a = Empty 
>                  | Node a (BinTree a) (BinTree a) 
>                   deriving (Eq,Ord)
> 
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
>     pshow pref x = replace '\n' ("\n"++pref) (show x)
> 
>     -- replace on char by another string
>     replace c new string =
>       concatMap (change c new) string
>       where
>           change c new x 
>               | x == c = new
>               | otherwise = x:[] -- "x"
> treeFromList :: (Ord a) => [a] -> BinTree a
> treeFromList []    = Empty
> treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
>                              (treeFromList (filter (>x) xs))

</div>


We will use a pseudo-random number list:

> shuffle = map (\x -> (x*3123) `mod` 4331) [1..]

and a function that troncate a tree up to a certain depth

> treeTakeDepth _ Empty = Empty
> treeTakeDepth 0 _     = Empty
> treeTakeDepth n (Node x left right) = let
>           nl = treeTakeDepth (n-1) left
>           nr = treeTakeDepth (n-1) right
>           in
>               Node x nl nr

See the result of:

> main = do
>       putStrLn "take 10 shuffle"
>       print $ take 10 shuffle
>       putStrLn "\ntreeTakeDepth 4 (treeFromList shuffle)"
>       print $ treeTakeDepth 4 (treeFromList shuffle)

~~~
% runghc 02_Hard_Part/41_Infinites_Structures.lhs
take 10 shuffle
[3123,1915,707,3830,2622,1414,206,3329,2121,913]
treeTakeDepth 4 (treeFromList shuffle)

< 3123
: |--1915
: |  |--707
: |  |  |--206
: |  |  `--1414
: |  `--2622
: |     |--2121
: |     `--2828
: `--3830
:    |--3329
:    |  |--3240
:    |  `--3535
:    `--4036
:       |--3947
:       `--4242
~~~

Yay! It ends! 
Beware thought, it will only work if you always have something to put into a branch.

For example 

<code class="haskell">
treeTakeDepth 4 (treeFromList [1..]) 
</code>

will loop forever. 
Simply because, it will try to access the head of `filter (<1) [2..]`.
But filter is not smart enought to understand that the result is the empty list.

Nonetheless, it is still a very cool example of what non strict program has to offer.

Left as an exercise to the reader:

- Could you prove that there exists some number `n` such that `treeTakeDepth n (treeFromList shuffle)` will enter in an infinite loop.
- Find an upper bound for `n`.
- Prove there is no `shuffle` list such that, for any depth, the program ends.
