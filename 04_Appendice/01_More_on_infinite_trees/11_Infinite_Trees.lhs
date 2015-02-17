
<div style="display:none">

This code is mostly the same as the preceding one.

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

fr: Pour résoudre ces problèmes nous allons modifier légèrement nos
en: In order to resolve these problem we will modify slightly our
fr: fonctions `treeFromList` et `shuffle`.
en: `treeFromList` and `shuffle` function.

fr: Un premier problème est le manque de nombres différents dans notre immlémentation de `shuffle`.
en: A first problem, is the lack of infinite different number in our implementation of `shuffle`.
fr: Nous avons  généré seulement `4331` nombres différents.
en: We generated only `4331` different numbers.
fr: Pour résoudre cela nous allons faire un meilleure fonction `shuffle`.
en: To resolve this we make a slightly better `shuffle` function.

> shuffle = map rand [1..]
>           where 
>               rand x = ((p x) `mod` (x+c)) - ((x+c) `div` 2)
>               p x = m*x^2 + n*x + o -- some polynome
>               m = 3123    
>               n = 31
>               o = 7641
>               c = 1237

fr: Cette fonction à la propriété de ne pas avoir de bornes supérieure ou inférieure.
en: This shuffle function has the property (hopefully) not to have an upper nor lower bound.
fr: Mais avoir une meilleure list `shuffle` n'est pas assez pour entrer dans une boucle infinie.
en: But having a better shuffle list isn't enough not to enter an infinite loop.

fr: Généralement,  nous ne pouvons pas décider que `filter (<x) xs` est vide.
en: Generally, we cannot decide whether `filter (<x) xs` is empty.
fr: Donc pour résoudre le problème, je vais autoriser quelques erreurs dans la création de notre arbre binaire.
en: Then to resolve this problem, I'll authorize some error in the creation of our binary tree.
fr: Cette nouvelle version du code peut créer des arbres binaires qui n'ont pas à suivre les propriétés suivantes pour quelque uns de leurs noeuds:
en: This new version of code can create binary tree which don't have the following property for some of its nodes: 

fr:  > Tous les élements de la branche de gauche doit être strictement inférieur au la valeur racine.
en:  > Any element of the left (resp. right) branch must all be strictly inferior (resp. superior) to the label of the root.

fr: Remarquez que cela donnera _souvent_ un arbre ordonné.
en: Remark it will remains _mostly_ an ordered binary tree.
fr: En outre, avec cette construction, chaque noeud est unique dans l'arbre.
en: Furthermore, by construction, each node value is unique in the tree.

fr: Voici notre nouvelle version de `treeFromList`. Nous avons simplement remplacé `filter` par `safefilter`.
en: Here is our new version of `treeFromList`. We simply have replaced `filter` by `safefilter`.

> treeFromList :: (Ord a, Show a) => [a] -> BinTree a
> treeFromList []    = Empty
> treeFromList (x:xs) = Node x left right
>           where 
>               left = treeFromList $ safefilter (<x) xs
>               right = treeFromList $ safefilter (>x) xs

fr: Cette nouvelle fonction `safefilter` est presque équivalente à `filter` mais n'entre pas dans des boucles infinies si le résultat est une liste finie.
en: This new function `safefilter` is almost equivalent to `filter` but don't enter infinite loop if the result is a finite list.
fr: Si elle ne peut pas trouver un élément pour lequel le test est vrai après 10000 étapes consécutives, alors elle considère que la recherche est finie.
en: If it cannot find an element for which the test is true after 10000 consecutive steps, then it considers to be the end of the search.

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

fr: Maintenant faites tourner le programme et soyez heureux:
en: Now run the program and be happy:

> main = do
>       putStrLn "take 10 shuffle"
>       print $ take 10 shuffle
>       putStrLn "\ntreeTakeDepth 8 (treeFromList shuffle)"
>       print $ treeTakeDepth 8 (treeFromList $ shuffle)

fr: Vous devriez réaliser que le temps nécessaire pour afficher chaque valeur est différent.
en: You should realize the time to print each value is different.
fr: C'est parce que Haskell calcule chaque valeur lorsqu'il en a besoin.
en: This is because Haskell compute each value when it needs it.
fr: Et dans ce cas, il est demandé de l'afficher à l'écran.
en: And in this case, this is when asked to print it on the screen.

fr: Vous pouvez même essayer de remplacer la profondeur de `8` par `100`.
en: Impressively enough, try to replace the depth from `8` to `100`.
fr: Cela marchera sans tuer votre RAM!
en: It will work without killing your RAM! 
fr: La gestion de la mémoire est faite naturellement par Haskell.
en: The flow and the memory management is done naturally by Haskell.

fr: Laissé comme exercices au lecteur:
en: Left as an exercise to the reader:

fr: - Même avec une grande valeur constante pour `deep` et `nbTry`, cela semble marcher correctement. Mais dans le pire des cas, cela peut devenir exponentiel. 
en: - Even with large constant value for `deep` and `nbTry`, it seems to work nicely. But in the worst case, it can be exponential.
fr:   Créez la pire liste à donner comme paramètre à `treeFromList`.
en:   Create a worst case list to give as parameter to `treeFromList`.  
fr:   _indice_: pensez à (`[0,-1,-1,....,-1,1,-1,...,-1,1,...]`).
en:   _hint_: think about (`[0,-1,-1,....,-1,1,-1,...,-1,1,...]`).
fr: - J'ai commencé à implémenter `safefilter` comme ceci:
en: - I first tried to implement `safefilter` as follow:
  <pre>
  safefilter' f l = if filter f (take 10000 l) == []
                    then []
                    else filter f l
  </pre>
fr:   Expliquer pourquoi cela ne fonctionne pas et peut entrer dans une boucle infinie.
en:   Explain why it doesn't work and can enter into an infinite loop.
fr: - Supposez que `shuffle` est une liste de nombre réellement aléatoires avec de plus en plus de bornes.
en: - Suppose that `shuffle` is real random list with growing bounds.
fr:   Si vous étudiez un peu cette structure, vous découvrirez qu'elle a toutes les chances
en:   If you study a bit this structure, you'll discover that with probability 1,
fr:   d'être finie.
en:   this structure is finite.
fr:   En utilisant le code suivant
en:   Using the following code 
fr:   (supposez que nous pouvons utliser `safefilter'` directement comme si cela n'était pas dans le `where` de `safefilter`.
en:   (suppose we could use `safefilter'` directly as if was not in the where of safefilter)
fr:   trouvez une définition de `f` telle que, avec une probabilité de `1`,
en:   find a definition of `f` such that with probability `1`, 
fr:   `treeFromList' shuffle` est infinie?. Et prouvez-le.
en:   `treeFromList' shuffle` is infinite. And prove it.
fr:   Avertissement, ce n'est qu'une conjecture.
en:   Disclaimer, this is only a conjecture.

<code class="haskell">
treeFromList' []  n = Empty
treeFromList' (x:xs) n = Node x left right
    where
        left = treeFromList' (safefilter' (<x) xs (f n)
        right = treeFromList' (safefilter' (>x) xs (f n)
        f = ???
</code>
