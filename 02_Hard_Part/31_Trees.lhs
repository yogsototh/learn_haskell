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
fr: La syntaxe générale est:

<code class="haskell">
instance Show (BinTree a) where
en:    show t = ... -- You declare your function here
fr:    show t = ... -- Déclarez votre fonction ici
</code>

en: Here is my version of how to show a binary tree.
en: Don't worry about the apparent complexity.
en: I made a lot of improvements in order to display even stranger objects.
fr: Voici ma version pour afficher un arbre binaire.
fr: Ne vous inquiétez pas de sa complexité apparente.
fr: J'ai fais beaucoup d'améliorations pour afficher même les objets les plus étranges.

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


en: The `treeFromList` method remains identical.
fr: La méthode `treeFromList` reste identique.

> treeFromList :: (Ord a) => [a] -> BinTree a
> treeFromList [] = Empty
> treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
>                              (treeFromList (filter (>x) xs))

en: And now, we can play:
fr: Et maintenant, nous pouvons jouer:

> main = do
>   putStrLn "Int binary tree:"
>   print $ treeFromList [7,2,4,8,1,3,6,21,12,23]

~~~
en: Int binary tree:
fr: Arbre binaire d'Int:
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

en: Now it is far better!
en: The root is shown by starting the line with the `<` character.
en: And each following line starts with a `:`.
en: But we could also use another type.
fr: Maintenant c'est beaucoup mieux!
fr: La racine est montré en commençant la ligne avec le caractère `<`.
fr: Et chaqeue ligne suivante commence avec un `:`.
fr: Mais nous pourrions aussi utiliser un autre type.

>   putStrLn "\nString binary tree:"
>   print $ treeFromList ["foo","bar","baz","gor","yog"]

~~~
en: String binary tree:
fr: Arbre binaire de chaïnes de caractères
< "foo"
: |--"bar"
: |  `--"baz"
: `--"gor"
:    `--"yog"
~~~

en: As we can test equality and order trees, we can
en: make tree of trees!
fr: Commme nous pouvons tester l'égalité et ordonner des arbres,
fr: nous pouvons aussi faire des arbres d'arbres!

>   putStrLn "\nBinary tree of Char binary trees:"
>   print ( treeFromList
>            (map treeFromList ["baz","zara","bar"]))

~~~
en: Binary tree of Char binary trees:
fr: Arbre binaire d'arbres binaires de Char:
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

en: This is why I chose to prefix each line of tree display by `:` (except for the root).
fr: C'est pour cela que j'ai choisi de préfixer chaque ligne par un `:` (sauf pour la racine).

blogimage("yo_dawg_tree.jpg","Yo Dawg Tree")

>   putStrLn "\nTree of Binary trees of Char binary trees:"
>   print $ (treeFromList . map (treeFromList . map treeFromList))
>              [ ["YO","DAWG"]
>              , ["I","HEARD"]
>              , ["I","HEARD"]
>              , ["YOU","LIKE","TREES"] ]

en: Which is equivalent to
fr: Qui est équivalent à

<code class="haskell">
print ( treeFromList (
          map treeFromList
             [ map treeFromList ["YO","DAWG"]
             , map treeFromList ["I","HEARD"]
             , map treeFromList ["I","HEARD"]
             , map treeFromList ["YOU","LIKE","TREES"] ]))
</code>

en: and gives:
fr: et donne:

~~~
en: Binary tree of Binary trees of Char binary trees:
fr: Arbre d'arbres d'arbres de char:
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

en: Notice how duplicate trees aren't inserted;
en: there is only one tree corresponding to `"I","HEARD"`.
en: We have this for (almost) free, because we have declared Tree to be an instance of `Eq`.
fr: Remarquez que les arbres en double ne sont pas insérés.
fr: Il n'y a qu'un seul arbre correspondant à `"I","HEARD"`.
fr: Nous avons ceci presque gratuitement, car nous avons déclaré Tree comme instance de `Eq`.

en: See how awesome this structure is:
en: We can make trees containing not only integers, strings and chars, but also other trees.
en: And we can even make a tree containing a tree of trees!
fr:Voyez à quel point cette structure est formidable: 
fr: Nous pouvons faire des arbres contenant seulement des entiers, des chaînes de caractères, mais aussi d'autres arbres. 
fr: Et nous pouvons même faire un arbre contenant un arbre d'arbres!
