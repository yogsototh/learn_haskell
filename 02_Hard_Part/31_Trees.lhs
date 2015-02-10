en: Just for fun, let's code a better display for our trees.
en: I simply had fun making a nice function to display trees in a general way.
en: You can safely skip this part if you find it too difficult to follow.
fr: Juste pour le plaisir, codons un meilleur affichage pour nos arbres.
fr: Je me suis simplement amusé à faire une belle fonction pour afficher les arbres de façon générale.
fr: Vous pouvez passer cette partie si vous la trouvez difficile à suivre.

en: We have a few changes to make.
en: We remove the `deriving (Show)` from the declaration of our `BinTree` type.
en: And it might also be useful to make our BinTree an instance of (`Eq` and `Ord`) so we will be able to test equality and compare trees.
fr: Nous avons quelques changements à faire.
fr: Enlevons le `deriving (Show)` de la déclaration de notre type `BinTree`.
fr: Il serait aussi utile de faire de BinTree une instance de (`Eq` et `Ord`), nous serons ainsi capable de tester l'égalité et de comparer des arbres.

> data BinTree a = Empty
>                  | Node a (BinTree a) (BinTree a)
>                   deriving (Eq,Ord)

en: Without the `deriving (Show)`, Haskell doesn't create a `show` method for us.
en: We will create our own version of `show`.
en: To achieve this, we must declare that our newly created type `BinTree a`
en: is an instance of the type class `Show`.
en: The general syntax is:
fr: Sans le `deriving (Show)`, Haskell ne crée pas de méthode `show` pour nous.
fr: Nous allons créer notre propre version.
fr: Pour accomplir cela, nous devons déclarer que notre type `BinTree a` 
fr: est une instance de la classe de type `Show`.
fr: 

<code class="haskell">
instance Show (BinTree a) where
   show t = ... -- You declare your function here
</code>

Here is my version of how to show a binary tree.
Don't worry about the apparent complexity.
I made a lot of improvements in order to display even stranger objects.

> -- declare BinTree a to be an instance of Show
> instance (Show a) => Show (BinTree a) where
>   -- will start by a '<' before the root
>   -- and put a : a begining of line
>   show t = "< " ++ replace '\n' "\n: " (treeshow "" t)
>     where
>     -- treeshow pref Tree
>     --   shows a tree and starts each line with pref
>     -- We don't display the Empty tree
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
>     -- Tree with left and right children non empty
>     treeshow pref (Node x left right) =
>                   (pshow pref x) ++ "\n" ++
>                   (showSon pref "|--" "|  " left) ++ "\n" ++
>                   (showSon pref "`--" "   " right)
>
>     -- shows a tree using some prefixes to make it nice
>     showSon pref before next t =
>                   pref ++ before ++ treeshow (pref ++ next) t
>
>     -- pshow replaces "\n" by "\n"++pref
>     pshow pref x = replace '\n' ("\n"++pref) (show x)
>
>     -- replaces one char by another string
>     replace c new string =
>       concatMap (change c new) string
>       where
>           change c new x
>               | x == c = new
>               | otherwise = x:[] -- "x"


The `treeFromList` method remains identical.

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
The root is shown by starting the line with the `<` character.
And each following line starts with a `:`.
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

This is why I chose to prefix each line of tree display by `:` (except for the root).

blogimage("yo_dawg_tree.jpg","Yo Dawg Tree")

>   putStrLn "\nTree of Binary trees of Char binary trees:"
>   print $ (treeFromList . map (treeFromList . map treeFromList))
>              [ ["YO","DAWG"]
>              , ["I","HEARD"]
>              , ["I","HEARD"]
>              , ["YOU","LIKE","TREES"] ]

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

Notice how duplicate trees aren't inserted;
there is only one tree corresponding to `"I","HEARD"`.
We have this for (almost) free, because we have declared Tree to be an instance of `Eq`.

See how awesome this structure is:
We can make trees containing not only integers, strings and chars, but also other trees.
And we can even make a tree containing a tree of trees!
