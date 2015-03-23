en: <h3 id="useful-notations-for-functions">Useful notations for functions</h3>
fr: <h3 id="useful-notations-for-functions">Notations utiles pour les fonctions</h3>

en: Just a reminder:
fr: Juste un mémo:

~~~
en: x :: Int            ⇔ x is of type Int
en: x :: a              ⇔ x can be of any type
en: x :: Num a => a     ⇔ x can be any type a
en:                       such that a belongs to Num type class 
en: f :: a -> b         ⇔ f is a function from a to b
en: f :: a -> b -> c    ⇔ f is a function from a to (b→c)
en: f :: (a -> b) -> c  ⇔ f is a function from (a→b) to c
fr: x :: Int            ⇔ x est de type Int
fr: x :: a              ⇔ x peut être de n'importe quel type
fr: x :: Num a => a     ⇔ x peut être de n'importe quel type a
fr:                       tant qu' a appartient à la classe de type Num 
fr: f :: a -> b         ⇔ f est une fonction qui prend un a et retourne un b
fr: f :: a -> b -> c    ⇔ f est une fonction qui prend un a et retourne un (b→c)
fr: f :: (a -> b) -> c  ⇔ f est une fonction qui prend un (a→b) et retourne un c
~~~

en: Remember that defining the type of a function before its declaration isn't mandatory.
en: Haskell infers the most general type for you.
en: But it is considered a good practice to do so.
fr: Rappelez-vous que définir le type d'une fonction avant sa déclaration n'est pas obligatoire.
fr: Haskell infère le type le plus général pour vous.
fr: Mais c'est considéré comme une bonne pratique.

en: _Infix notation_
fr: _Notation Infixée_

> square :: Num a => a -> a
> square x = x^2

en: Note `^` uses infix notation.
en: For each infix operator there its associated prefix notation.
en: You just have to put it inside parenthesis.
fr: Remarquez que `^` utilise une notation infixée.
fr: Pour chaque opérateur infixe il y a une notation préfixée associée.
fr: Vous devz juste l'écrire entre parenthèses.

> square' x = (^) x 2
> 
> square'' x = (^2) x

en: We can remove `x` in the left and right side!
en: It's called η-reduction.
fr: Nous pouvons enlever le `x` dans les parties de gauche et de droite!
fr: On appelle cela la η-réduction

> square''' = (^2)

en: Note we can declare functions with `'` in their name.
en: Here:
fr: Rmarquez qu nous pouvons déclarer des fonctions avec `'` dans leur nom.
fr: Exemples:

 > `square` ⇔  `square'` ⇔ `square''` ⇔ `square'''`

_Tests_

en: An implementation of the absolute function.
fr: Une implémentation de la fonction absolue.

> absolute :: (Ord a, Num a) => a -> a
> absolute x = if x >= 0 then x else -x

en: Note: the `if .. then .. else` Haskell notation is more like the
en: `¤?¤:¤` C operator. You cannot forget the `else`.
fr: Remarque: la notation de Haskell pour le `if .. then .. else` ressemble plus
fr: à l'opérateur `¤?¤:¤` en C. Le `else` est obligatoire.

en: Another equivalent version:
fr: Une version équivalente:

> absolute' x
>     | x >= 0 = x
>     | otherwise = -x

en:  > Notation warning: indentation is _important_ in Haskell.
en:  > Like in Python, bad indentation can break your code!
fr:  > Avertissement: l'indentation est _importante_ en Haskell.
fr:  > Comme en Python, une mauvaise indentation peut détruire votre code!

<div style="display:none">

> main = do
>       print $ square 10
>       print $ square' 10
>       print $ square'' 10
>       print $ square''' 10
>       print $ absolute 10
>       print $ absolute (-10)
>       print $ absolute' 10
>       print $ absolute' (-10)

</div>
