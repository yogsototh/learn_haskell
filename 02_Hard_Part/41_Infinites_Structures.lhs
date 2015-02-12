
<div style="display:none">

This code is mostly the same as the previous one.

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

</div>

en: Suppose we don't mind having an ordered binary tree.
en: Here is an infinite binary tree:
fr: Supposons que nous ne nous préoccupions pas d'avoir une arbre ordonné.
fr: Voici un arbre binaire infini:

> nullTree = Node 0 nullTree nullTree

en: A complete binary tree where each node is equal to 0.
en: Now I will prove you can manipulate this object using the following function:
fr: Un arbre complet où chaque noeud est égal à 0.
fr: Maintenant je vais vous prouver que nous pouvons manipuler cet arbre avec la fonction suivante:

> -- take all element of a BinTree 
> -- up to some depth
> treeTakeDepth _ Empty = Empty
> treeTakeDepth 0 _     = Empty
> treeTakeDepth n (Node x left right) = let
>           nl = treeTakeDepth (n-1) left
>           nr = treeTakeDepth (n-1) right
>           in
>               Node x nl nr

en: See what occurs for this program:
fr: Regardez ce qui se passe avec ce programme:

<code class="haskell">
main = print $ treeTakeDepth 4 nullTree
</code>

en: This code compiles, runs and stops giving the following result:
fr: Le code compile, se lance et s'arrête en donnant ce résultat:

~~~
<  0
: |-- 0
: |  |-- 0
: |  |  |-- 0
: |  |  `-- 0
: |  `-- 0
: |     |-- 0
: |     `-- 0
: `-- 0
:    |-- 0
:    |  |-- 0
:    |  `-- 0
:    `-- 0
:       |-- 0
:       `-- 0
~~~

en: Just to heat up your neurones a bit more,
en: let's make a slightly more interesting tree:
fr: Pour nous chauffer encore un peu les neurones,
fr: faisons un arbre plus intéressant:

> iTree = Node 0 (dec iTree) (inc iTree)
>         where
>            dec (Node x l r) = Node (x-1) (dec l) (dec r) 
>            inc (Node x l r) = Node (x+1) (inc l) (inc r) 

en: Another way to create this tree is to use a higher order function.
en: This function should be similar to `map`, but should work on `BinTree` instead of list.
en: Here is such a function:
fr: Un autre moyen de créer cet arbre est d'utiliser une fonction d'ordre supérieur.
fr: Cette fonction devrait être similaire à `map` n, mais devrais travailler sur un `BinTree` au lieu d'une liste. 
fr: Voici cette fonction:

> -- apply a function to each node of Tree
> treeMap :: (a -> b) -> BinTree a -> BinTree b
> treeMap f Empty = Empty
> treeMap f (Node x left right) = Node (f x) 
>                                      (treeMap f left) 
>                                      (treeMap f right)

en: _Hint_: I won't talk more about this here. 
en: If you are interested in the generalization of `map` to other data structures,
en: search for functor and `fmap`.
fr: _NB_: Je ne parlerais pas plus de cette fonction ici.
fr: Si vous vous intéressez à la généralisation de `map`à d'autre structures de données,
fr: cherchez des informations sur les foncteurs et `fmap`.

en: Our definition is now:
fr: Notre définition est maintenant:

> infTreeTwo :: BinTree Int
> infTreeTwo = Node 0 (treeMap (\x -> x-1) infTreeTwo) 
>                     (treeMap (\x -> x+1) infTreeTwo) 

en: Look at the result for 
fr: Regardez le résultat pour

<code class="haskell">
main = print $ treeTakeDepth 4 infTreeTwo
</code>

~~~
<  0
: |-- -1
: |  |-- -2
: |  |  |-- -3
: |  |  `-- -1
: |  `-- 0
: |     |-- -1
: |     `-- 1
: `-- 1
:    |-- 0
:    |  |-- -1
:    |  `-- 1
:    `-- 2
:       |-- 1
:       `-- 3
~~~


<div style="display:none">

> main = do
>   print $ treeTakeDepth 4 nullTree
>   print $ treeTakeDepth 4 infTreeTwo

</div>
