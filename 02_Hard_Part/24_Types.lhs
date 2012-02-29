Just for fun, let's code a better display for our trees.
I simply had fun into making a nice function to display tree in a general way.
You can safely pass this part if you find it too difficult to follow.

We have few to change to make. 
First, as we will play a bit with string, we import the function `replace`
from `Data.String.Utils`.

> import Data.List

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
Don't worry about the apparent complexity, I made a lot of improvement in order to display strange objects.
Here is how you achieve this:

> -- declare BinTree a to be an instance of Show
> instance (Show a) => Show (BinTree a) where
>   show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
>     where
>     treeshow pref Empty = ""
>     treeshow pref (Node x Empty Empty) = (safeshow pref x)
>     treeshow pref (Node x left Empty) = (safeshow pref x) ++ "\n" ++
>                   (showSon pref "`--" "   " left)
>     treeshow pref (Node x Empty right) = (safeshow pref x) ++ "\n" ++
>                   (showSon pref "`--" "   " right)
>     treeshow pref (Node x left right) = 
>                   (safeshow pref x) ++ "\n" ++
>                   (showSon pref "|--" "|  " left) ++ "\n" ++
>                   (showSon pref "`--" "   " right)
>     -- show a son tree using some specific strings to make it nicer
>     showSon pref before next t = 
>                   pref ++ before ++ treeshow (pref ++ next) t
>     -- safeshow replace "\n" by "\n"++pref
>     safeshow pref x = replace '\n' ("\n"++pref) (show x)
>     -- replace on char by another string
>     replace c new string =
>       concatMap (change c new) string
>       where
>           change c new x 
>               | x == c = new
>               | otherwise = x:[] -- "x"


The `treeInsert` method remain identical.

> treeInsert :: (Ord a) => BinTree a -> a -> BinTree a
> treeInsert Empty x    = Node x Empty Empty
> treeInsert (Node y left right) x
>           | x == y    = (Node y left right)
>           | x < y     = (Node y (treeInsert left x) right)
>           | otherwise = (Node y left (treeInsert right x))

And now, we can play:

<%= blogimage("yo_dawg_tree.jpg","Yo Dawg Tree") %>

> treeFromList list = foldl' treeInsert Empty list
> 
> main = do
>           putStrLn "Int binary tree:"
>           print $ treeFromList [7,2,4,8,1,3,6,21,12,23]
>           putStrLn "\nString binary tree:"
>           print $ treeFromList ["foo","bar","baz","gorilla","yogsototh"]
>           putStrLn "\nBinary tree of Char binary trees:"
>           print $ treeFromList (map treeFromList ["baz","zara","bar"])
>           putStrLn "\nBinary tree of Binary trees of Char binary trees:"
>           print $ treeFromList (map treeFromList 
>                                   [ map treeFromList ["Ia!","Ia!"]
>                                   , map treeFromList ["cthul","hu"]
>                                   , map treeFromList ["Fhtagn!"] ])


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

String binary tree:
< "foo"
: |--"bar"
: |  `--"baz"
: `--"gorilla"
:    `--"yogsototh"

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

Binary tree of Binary trees of Char binary trees:
< < < 'I'
: : : |--'!'
: : : `--'a'
: |--< < 'F'
: |  : : |--'!'
: |  : : `--'h'
: |  : :    |--'a'
: |  : :    |  `--'g'
: |  : :    `--'t'
: |  : :       `--'n'
: `--< < 'c'
:    : : `--'t'
:    : :    |--'h'
:    : :    |  `--'l'
:    : :    `--'u'
:    : `--< 'h'
:    :    : `--'u'
~~~

Yeah, Now it is far better!

There are some trick in the code to handle object that show themselve on many lines (like trees).

Remark how you can't insert two identical tree;
there is only one tree corresponding to "Ia!" in the last example.

Note how awesome this structure is.
We can make tree containing not only integer, string and char, but also other trees.
And we can even make a tree containing a tree of trees!
