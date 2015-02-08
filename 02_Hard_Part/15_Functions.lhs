en: <h4 id="higher-order-functions">Higher Order Functions</h4>
fr: <h4 id="higher-order-functions">Fonctions d'ordre supérieur</h4>

blogimage("escher_polygon.png","Escher")

en: To make things even better we should use higher order functions.
en: What are these beasts?
en: Higher order functions are functions taking functions as parameters.
fr: Pour rendre les choses plus faciles, nous devrions utiliser des fonctions d'ordre supérieur.
fr: Ce sont des fonctions qui prennent des fonctions en paramètres

en: Here are some examples:
fr: Voici quelques exemples:

<code class="haskell">
filter :: (a -> Bool) -> [a] -> [a]
map :: (a -> b) -> [a] -> [b]
foldl :: (a -> b -> a) -> a -> [b] -> a
</code>

en: Let's proceed by small steps.
fr: Procédons par étapes.

<code class="haskell">
-- Version 5
evenSum l = mysum 0 (filter even l)
    where
      mysum n [] = n
      mysum n (x:xs) = mysum (n+x) xs
</code>

en: where
fr: où

<code class="haskell">
filter even [1..10] ⇔  [2,4,6,8,10]
</code>

en: The function `filter` takes a function of type (`a -> Bool`) and a list of type `[a]`. 
en: It returns a list containing only elements for which the function returned `true`.
fr: La fonction `filter` prend une fonction du type (`a -> Bool`) et une liste de type `[a]`.
fr: Elle retourne une liste qui contient seulement les élements pour qui la fonction  a retourné `True`.

en: Our next step is to use another technique to accomplish the same thing as a loop.
en: We will use the `foldl` function to accumulate a value as we pass through the list.
en: The function `foldl` captures a general coding pattern:
fr: La prochaine étape est d'utiliser une autre technique pour accomplir la même chose qu'une boucle.
fr: Nous allons utiliser la fonction `foldl` pour accumuler une valeur au fur et à mesure que l'on parcoure la liste.
fr: La fonction `foldl` capture un modèle de code général:

<pre>
    myfunc list = foo <span class="blue">initialValue</span> <span class="green">list</span>
    foo accumulated []     = accumulated
    foo tmpValue    (x:xs) = foo (<span class="yellow">bar</span> tmpValue x) xs
</pre>

en: Which can be replaced by:
fr: Qui peut être remplacé par:

<pre>
myfunc list = foldl <span class="yellow">bar</span> <span class="blue">initialValue</span> <span class="green">list</span>
</pre>

en: If you really want to know how the magic works, here is the definition of `foldl`:
fr: Si vous souhaitez vraiment savoir comment la magie se produit, voici la définition de `foldl`:

<code class="haskell">
foldl f z [] = z
foldl f z (x:xs) = foldl f (f z x) xs
</code>

<code class="haskell">
foldl f z [x1,...xn]
⇔  f (... (f (f z x1) x2) ...) xn
</code>

en: But as Haskell is lazy, it doesn't evaluate `(f z x)` and  simply pushes it onto the stack.
en: This is why we generally use `foldl'` instead of `foldl`;
en: `foldl'` is a _strict_ version of `foldl`.
en: If you don't understand what lazy and strict means,
en: don't worry, just follow the code as if `foldl` and `foldl'` were identical.
fr: Mais comme Haskell est paresseux, il n'évalue pas `(f z x)` et le met simplement dans la pile.
fr: C'est pourquoi on utilise généralement `foldl'`, une version _stricte_ de `foldl`,
fr: Si vous ne comprenez pas encore ce que _paresseux_ ou _strict_ signifie,
fr: ne vous inquiétez pas, suivez le code comme si `foldl'` et `foldl` étaient identiques

en: Now our new version of `evenSum` becomes:
fr: Maintenant notre version de `evenSum` devient: 

<code class="haskell">
-- Version 6
en: -- foldl' isn't accessible by default
en: -- we need to import it from the module Data.List
fr: -- foldl' n'est pas accessible par défaut
fr: -- nous devons l'importer depuis le module Data.List
import Data.List
evenSum l = foldl' mysum 0 (filter even l)
  where mysum acc value = acc + value
</code>

en: We can also simplify this by using directly a lambda notation.
en: This way we don't have to create the temporary name `mysum`.
fr: Nous pouvons aussi simplifier cela en utilisant une _lambda-notation_.
fr: Ainsi nous n'avons pas besoin de créer le nom temporaire `mySum`.

> -- Version 7
> -- Generally it is considered a good practice
> -- to import only the necessary function(s)
> import Data.List (foldl')
> evenSum l = foldl' (\x y -> x+y) 0 (filter even l)

en: And of course, we note that
fr: Et bien sûr, nous remarquons que

<code class="haskell">
(\x y -> x+y) ⇔ (+)
</code>

<div style="display:none">

> main = print $ evenSum [1..10]

</div>
