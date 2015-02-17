fr: <h3 id="more-on-infinite-tree">Revenons sur les arbres infinis</h3>
en: <h3 id="more-on-infinite-tree">More on Infinite Tree</h3>

fr: Dans la section sur [les structures infinies](#infinite-structures) nous avons vu quelques 
en: In the section [Infinite Structures](#infinite-structures) we saw some simple
fr: constructions simples.
en: constructions.
fr: Malheureusement, nous avons enlevé deux propriétés de notre arbre:
en: Unfortunately we removed two properties from our tree:

fr: 1. Pas de valeurs identiques
en: 1. no duplicate node value
fr: 2. Arbre bien ordonné
en: 2. well ordered tree

fr: Dans cette section nous allons tenter de garder la première propriété.
en: In this section we will try to keep the first property.
fr: Concernant la seconde, nous ne devons pas nous en préoccuper ici mais nous discuterons
en: Concerning the second one, we must relax it but we'll discuss how to
fr: de comment la garder le plus possible.
en: keep it as much as possible.

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
>

</div>

fr: Notre première étape est de créer une liste de nombres pseudo-aléatoires:
en: Our first step is to create some pseudo-random number list:

> shuffle = map (\x -> (x*3123) `mod` 4331) [1..]

fr: Pour mémoire, voici la définition de `treeFromList`
en: Just as a reminder, here is the definition of `treeFromList`

> treeFromList :: (Ord a) => [a] -> BinTree a
> treeFromList []    = Empty
> treeFromList (x:xs) = Node x (treeFromList (filter (<x) xs))
>                              (treeFromList (filter (>x) xs))

fr: et 
en: and 
`treeTakeDepth`:

> treeTakeDepth _ Empty = Empty
> treeTakeDepth 0 _     = Empty
> treeTakeDepth n (Node x left right) = let
>           nl = treeTakeDepth (n-1) left
>           nr = treeTakeDepth (n-1) right
>           in
>               Node x nl nr

fr: Voyez le résultats de:
en: See the result of:

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

fr: Le code fonctionne!
en: Yay! It ends! 
fr: Attention cependant, cela marchere seulement si vous avez toujours quelque chose à mettre dans une branche.
en: Beware though, it will only work if you always have something to put into a branch.

fr: Par exemple
en: For example 

<code class="haskell">
treeTakeDepth 4 (treeFromList [1..]) 
</code>

fr: tournera en boucle pour toujours.
en: will loop forever. 
fr: Simplement parce que le code essayera d'accéder à première valeur de `filter (<1) [2..]`.
en: Simply because it will try to access the head of `filter (<1) [2..]`.
fr: Mais `filter` n'est pas assez intelligent pour comprendre que le résultat est une liste vide.
en: But `filter` is not smart enought to understand that the result is the empty list.

fr: Toutefois, cela reste un exemple sympa de ce qu'un programme non-stricit a à offrir.
en: Nonetheless, it is still a very cool example of what non strict programs have to offer.

fr: Laissé pour exercice au lecteur:
en: Left as an exercise to the reader:

fr: - Prouver l'existence d'un nombre `n` tel que `treeTakeDepth n (treeFromList shuffle)` provoquera une boucle infinie.
en: - Prove the existence of a number `n` so that `treeTakeDepth n (treeFromList shuffle)` will enter an infinite loop.
fr: - Trouver une borne supérieur `n`.
en: - Find an upper bound for `n`.
fr: - Prouver qu'il n(y a pas de liste `shuffle` qui termine le programme pour n'importe quelle profondeur.
en: - Prove there is no `shuffle` list so that, for any depth, the program ends.
