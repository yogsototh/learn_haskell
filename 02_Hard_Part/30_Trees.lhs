en: <h4 id="trees">Trees</h4>
fr: <h4 id="trees">Les arbres</h4>

blogimage("magritte-l-arbre.jpg","Magritte, l'Arbre")

en: We'll just give another standard example: binary trees.
fr: Voici une autre exemple standard : les arbres binaires.

> import Data.List
>
> data BinTree a = Empty
>                  | Node a (BinTree a) (BinTree a)
>                               deriving (Show)

en: We will also create a function which turns a list into an ordered binary tree.
fr: Créons aussi une fonctions qui transforme une liste en un arbre binaire ordonné.

> treeFromList :: (Ord a) => [a] -> BinTree a
> treeFromList [] = Empty
> treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
>                              (treeFromList (filter (>x) xs))

en: Look at how elegant this function is.
en: In plain English:
fr: Remarquez à quel point cette fonction est élégante.
fr: En français:

en: - an empty list will be converted to an empty tree.
en: - a list `(x:xs)` will be converted to a tree where:
en:   - The root is `x`
en:   - Its left subtree is the tree created from members of the list `xs` which are strictly inferior to `x` and
en:   - the right subtree is the tree created from members of the list `xs` which are strictly superior to `x`.
fr: - une liste vide est convertie en un arbre vide
fr: - une liste `(x:xs)` sera convertie en un arbre où:
fr:   - La racine est `x`
fr:   - Le "sous-arbre" de gauche est l'arbre créé à partir des membres de la liste `xs` strictement inférieurs à `x`
fr:   - Le "sous-arbre" de droite est l'arbre créé à partir des membres de la liste `xs` strictement superieurs à `x`

> main = print $ treeFromList [7,2,4,8]

en: You should obtain the following:
fr: Vious devriez obtenir:

~~~
Node 7 (Node 2 Empty (Node 4 Empty Empty)) (Node 8 Empty Empty)
~~~

en: This is an informative but quite unpleasant representation of our tree.
fr: C'est représentation de notre arbre informative mais déplaisante.
