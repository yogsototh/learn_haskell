Just for fun, let's code a better display for our trees.
I simply had fun into making a nice function to display tree in a general way.
You can safely pass this part if you find it too difficult to follow.

We have few changes to make.

> import Data.List (foldl')

We remove the `deriving (Show)` in the declaration of our `BinTree` type.
And it also might be useful to make our BinTree an instance of (Eq and Ord).
Now we can test equality and compare trees.

> data BinTree a = Empty 
>                  | Node a (BinTree a) (BinTree a) 
>                   deriving (Eq,Ord)

Without the `deriving (Show)`, Haskell doesn't create a show method for us.
Now, we will create our version of show.
For this, we want our newly created type `BinTree a` to be an instance of
the type class `Show`.
To achieve this, the general syntax is:

<code class="haskell">
instance Show (BinTree a) where
   show t = ... -- You declare your function here
</code>

Here is my version on how to show a binary tree.
Don't worry about the apparent complexity.
I made a lot of improvement in order to display even strange objects.

> -- declare BinTree a to be an instance of Show
> instance (Show a) => Show (BinTree a) where
>   -- will start by a '<' before the root
>   -- and put a : a begining of line
>   show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
>     where
>     -- treeshow pref Tree 
>     --   show a tree and start each line with pref
>     -- We don't display Empty tree
>     treeshow pref Empty = ""
>     -- Leaf
>     treeshow pref (Node x Empty Empty) = 
>                   (pshow pref x)
>
>     -- Right branch is empty
>     treeshow pref (Node x left Empty) = 
>                   (pshow pref x) ++ "\n" ++
>                   (showSon pref "`--" "   " left)
>
>     -- Left branch is empty
>     treeshow pref (Node x Empty right) = 
>                   (pshow pref x) ++ "\n" ++
>                   (showSon pref "`--" "   " right)
>
>     -- Tree with left and right sons non empty
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


The `treeFromList` method remain identical.

> treeFromList :: (Ord a) => [a] -> BinTree a
> treeFromList [] = Empty
> treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
>                              (treeFromList (filter (>x) xs))

And now, we can play:

> main = do
>   putStrLn "Int binary tree:"
>   print $ treeFromList [7,2,4,8,1,3,6,21,12,23]

~~~
Int binary tree:
< 7
: |--2
: |  |--1
: |  `--4
: |     |--3
: |     `--6
: `--8
:    `--21
:       |--12
:       `--23
~~~

Now it is far better! 
The root is shown by starting by the `<` character.
And each other line start by a `:`.
But we could also use another type.

>   putStrLn "\nString binary tree:"
>   print $ treeFromList ["foo","bar","baz","gor","yog"]

~~~
String binary tree:
< "foo"
: |--"bar"
: |  `--"baz"
: `--"gor"
:    `--"yog"
~~~

As we can test equality and order trees, we can
make tree of trees!

>   putStrLn "\nBinary tree of Char binary trees:"
>   print ( treeFromList 
>            (map treeFromList ["baz","zara","bar"]))

~~~
Binary tree of Char binary trees:
< < 'b'
: : |--'a'
: : `--'z'
: |--< 'b'
: |  : |--'a'
: |  : `--'r'
: `--< 'z'
:    : `--'a'
:    :    `--'r'
~~~

This is why I chosen to prefix each line of tree display by `:` (except for the root).

<%= blogimage("yo_dawg_tree.jpg","Yo Dawg Tree") %>

>   putStrLn "\nTree of Binary trees of Char binary trees:"
>   print $ (treeFromList . map (treeFromList . map treeFromList))
>                [ ["YO","DAWG"]
>                , ["I","HEARD"]
>                , ["I","HEARD"]
>                , ["YOU","LIKE","TREES"] ]

Which is equivalent to

<code class="haskell">
print ( treeFromList (
          map treeFromList 
             [ map treeFromList ["YO","DAWG"]
             , map treeFromList ["I","HEARD"]
             , map treeFromList ["I","HEARD"]
             , map treeFromList ["YOU","LIKE","TREES"] ]))
</code>

and gives:

~~~
Binary tree of Binary trees of Char binary trees:
< < < 'Y'
: : : `--'O'
: : `--< 'D'
: :    : |--'A'
: :    : `--'W'
: :    :    `--'G'
: |--< < 'I'
: |  : `--< 'H'
: |  :    : |--'E'
: |  :    : |  `--'A'
: |  :    : |     `--'D'
: |  :    : `--'R'
: `--< < 'Y'
:    : : `--'O'
:    : :    `--'U'
:    : `--< 'L'
:    :    : `--'I'
:    :    :    |--'E'
:    :    :    `--'K'
:    :    `--< 'T'
:    :       : `--'R'
:    :       :    |--'E'
:    :       :    `--'S'
~~~

Remark how you can't insert two identical tree;
there is only one tree corresponding to `"I","HEARD"`.

Note how awesome this structure is.
We can make tree containing not only integer, string and char, but also other trees.
And we can even make a tree containing a tree of trees!
