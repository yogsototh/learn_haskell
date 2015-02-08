en: Finally
fr: Finalement

<code class="haskell">
-- Version 8
import Data.List (foldl')
evenSum :: Integral a => [a] -> a
evenSum l = foldl' (+) 0 (filter even l)
</code>

en: `foldl'` isn't the easiest function to grasp.
en: If you are not used to it, you should study it a bit.
fr: `foldl'` n'est pas la fonction la plus facile à prendre en main.
fr: Si vous n'y êtes pas habitué, vous devriez l'étudier un peu.

en: To help you understand what's going on here, let's look at a step by step evaluation:
fr: Pour mieux comprendre ce qui se passe ici, étudions une évaluation étape par étape:

<pre>
  <span class="yellow">evenSum [1,2,3,4]</span>
⇒ foldl' (+) 0 (<span class="yellow">filter even [1,2,3,4]</span>)
⇒ <span class="yellow">foldl' (+) 0 <span class="blue">[2,4]</span></span>
⇒ <span class="blue">foldl' (+) (<span class="yellow">0+2</span>) [4]</span> 
⇒ <span class="yellow">foldl' (+) <span class="blue">2</span> [4]</span>
⇒ <span class="blue">foldl' (+) (<span class="yellow">2+4</span>) []</span>
⇒ <span class="yellow">foldl' (+) <span class="blue">6</span> []</span>
⇒ <span class="blue">6</span>
</pre>


en: Another useful higher order function is `(.)`.
en: The `(.)` function corresponds to mathematical composition.
fr: Une autr fonction d'ordre supérieur utile est `(.)`.
fr: Elle correspond à une composition en mathématiques.

<code class="haskell">
(f . g . h) x ⇔  f ( g (h x))
</code>

en: We can take advantage of this operator to η-reduce our function:
fr: Nous pouvons profiter de cet opérateur pour η-réduire notre fonction:

<code class="haskell">
-- Version 9
import Data.List (foldl')
evenSum :: Integral a => [a] -> a
evenSum = (foldl' (+) 0) . (filter even)
</code>

en: Also, we could rename some parts to make it clearer:
fr: Nous pouvons maintenant renommer certaines parties pour rendre le tout plus clair:

> -- Version 10 
> import Data.List (foldl')
> sum' :: (Num a) => [a] -> a
> sum' = foldl' (+) 0
> evenSum :: Integral a => [a] -> a
> evenSum = sum' . (filter even)
>  

en: It is time to discuss the direction our code has moved as we introduced more functional idioms.
en: What did we gain by using higher order functions?
fr: Il est temps de discuter de la direction qu'a pris notre code depuis que nous avons introduit plus d'idiomes fonctionnels.
fr: Que gagnons-nous à utiliser des fonctions d'ordre supérieur?

en: At first, you might think the main difference is terseness. But in fact, it has
en: more to do with better thinking. Suppose we want to modify our function
en: slightly, for example, to get the sum of all even squares of elements of the list.
fr: D'abord, vous pourriez penser que la principale différence est la brièveté. Mais en réalité,
fr: il s'agit d'une meilleure façon de penser. Supposons que nous voulons modifier légèrement notre fonction,
fr: par exemple, pour qu'elle renvoie la somme de tous les carrés pairs des éléments de la liste.

~~~
[1,2,3,4] ▷ [1,4,9,16] ▷ [4,16] ▷ 20
~~~

en: Updating version 10 is extremely easy:
fr: Mettre la version 10 à jour est très facile:

> squareEvenSum = sum' . (filter even) . (map (^2))
> squareEvenSum' = evenSum . (map (^2))

en: We just had to add another "transformation function"[^0216].
fr: Nous avons juste eu à ajouter une autre "fonction de trabsformation"[^0216].

~~~
map (^2) [1,2,3,4] ⇔ [1,4,9,16]
~~~

en: The `map` function simply applies a function to all the elements of a list.
fr: La fonction `map` applique simplementune fonction à tous les élements d'une liste.

en: We didn't have to modify anything _inside_ the function definition.
en: This makes the code more modular.
en: But in addition you can think more mathematically about your function.
en: You can also use your function interchangably with others, as needed.
en: That is, you can compose, map, fold, filter using your new function.
fr: Nous n'avons rien modifié _à l'intérieur_ de notre définition de fonction.
fr: Cela rend le code plus modulaire.
fr: En plus de cela, vous pouvez penser à votre fonction plus mathématiquement.
fr: Vous pouvez aussi utilier votre fonction avec d'autres, au besoin:
fr: vous pouvez utiliser `compose`, `map`, `fold` ou `filter` sur notre nouvelle fonction.

en: Modifying version 1 is left as an exercise to the reader ☺.
fr: Modifier la version 1 est laissé comme un exercice pour le lecteur ☺.

en: If you believe we have reached the end of generalization, then know you are very wrong.
en: For example, there is a way to not only use this function on lists but on any recursive type.
en: If you want to know how, I suggest you to read this quite fun article: [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire by Meijer, Fokkinga and Paterson](http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf).
fr: Si vous croyez avoir atteint le bout de la généralisation, vous avez tout faux.
fr: Par example, il y a un moyen d'utiliser cette fonction non seulement sur les listes mais aussi sur n'importe quel type récursif.
fr: Si vous voulez savoir comment, je vous suggère de lire cet article: [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire by Meijer, Fokkinga and Paterson](http://eprints.eemcs.utwente.nl/7281/01/db-utwente-40501F46.pdf) (_NDT: en anglais, mais là vous vous en seriez douté je pense ☺_)

en: This example should show you how great pure functional programming is.
en: Unfortunately, using pure functional programming isn't well suited to all usages.
en: Or at least such a language hasn't been found yet.
fr: Cet exemple montre à quel point la programmation fonctionnelle pure est géniale.
fr: Malheureusement, utiliser cet outil n'est pas adapté à tous les besoins.
fr: Ou alors un langage qui le premettrait n'a pas encore été trouvé.

en: One of the great powers of Haskell is the ability to create DSLs
en: (Domain Specific Language)
en: making it easy to change the programming paradigm.
fr: Une des grands pouvoirs de Haskell est sa capacité à créer des DSLs
fr: (_Domain Specific Language_, en français : _langage spécifique à un domaine_)
fr: Il est ainsi facile de changer le pardigme de programmation

en: In fact, Haskell is also great when you want to write imperative style
en: programming. Understanding this was really hard for me to grasp when first
en: learning Haskell. A lot of effort tends to go into explaining the superiority
en: of the functional approach. Then when you start using an imperative style with
en: Haskell, it can be hard to understand when and how to use it.
fr: In fact, Haskell is also great when you want to write imperative style
fr: programming. Understanding this was really hard for me to grasp when first
fr: learning Haskell. A lot of effort tends to go into explaining the superiority
fr: of the functional approach. Then when you start using an imperative style with
fr: Haskell, it can be hard to understand when and how to use it.
fr: (_NDT: Je ne parviens pas à traduire correctement cette partie pour des raisons qui m'échappent..._)

en: But before talking about this Haskell super-power, we must talk about another
en: essential aspect of Haskell: _Types_.
fr: Mais avant de parler de ce super-pouvoir de Haskell, nous devons parler
fr: d'un autre aspet essentiel: les _Types_.

<div style="display:none">

> main = print $ evenSum [1..10]

</div>
