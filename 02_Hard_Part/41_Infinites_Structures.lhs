<h4 id="infinite-tree">Infinite Tree</h4>

<div class="hidden">

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
>     pshow pref x = replace '\n' ("\n"++pref) (show x)
> 
>     -- replace on char by another string
>     replace c new string =
>       concatMap (change c new) string
>       where
>           change c new x 
>               | x == c = new
>               | otherwise = x:[] -- "x"

</div>

For this example, using a fold of `treeInsert` won't work.
Instead we will construct directly a `treeFromList`.


> treeFromList :: (Ord a) => [a] -> BinTree a
> treeFromList []    = Empty
> treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
>                              (treeFromList (filter (>x) xs))

Also we use a shuffle function to create pseudo-random number list:

> shuffle = map (\x -> (x*3123) `mod` 4331) [1..]

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
>       print $ treeTakeDepth 5 (treeFromList shuffle)

~~~
% runghc 02_Hard_Part/41_Infinites_Structures.lhs
take 10 shuffle
[3123,1915,707,3830,2622,1414,206,3329,2121,913]

treeTakeDepth 5 (treeFromList shuffle)
< 3123
: |--1915
: |  |--707
: |  |  |--206
: |  |  |  |--117
: |  |  |  `--412
: |  |  `--1414
: |  |     |--913
: |  |     `--1620
: |  `--2622
: |     |--2121
: |     |  |--2032
: |     |  `--2327
: |     `--2828
: |        |--2739
: |        `--3034
: `--3830
:    |--3329
:    |  |--3240
:    |  |  |--3151
:    |  |  `--3268
:    |  `--3535
:    |     |--3446
:    |     `--3741
:    `--4036
:       |--3947
:       |  |--3858
:       |  `--3975
:       `--4242
:          |--4153
:          `--4270
~~~

Yay! It ends! 
Beware thought, it will only work if you alway have something to put into a branch.

For example 

<code class="haskell">
treeTakeDepth 5 (treeFromList [1..]) 
</code>

will loop forever. 
Simply because, it will try to access the head of `filter (<1) [2..]`.
But filter is not smart enought to understand that the result is the empty list.

Nonetheless, it is still a very cool example of what non strict program has to offer.
