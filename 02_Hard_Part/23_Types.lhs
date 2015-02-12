en: <h4 id="recursive-type">Recursive type</h4>
fr: <h4 id="recursive-type">Type récursif</h4>

en: You already encountered a recursive type: lists.
en: You can re-create lists, but with a more verbose syntax:
fr: Nous avons déjà rencontré un type récursif : les listes.
fr: Nous pourrions re-créer les listes, avec une syntaxe plus bavarde:

<code class="haskell">
data List a = Empty | Cons a (List a)
</code>


en: If you really want to use an easier syntax you can use an infix name for constructors.
fr: Si vous voulez réellement utiliser une syntxe plus simple, utilisez un nom infixe pour les constructeurs.

<code class="haskell">
infixr 5 :::
data List a = Nil | a ::: (List a)
</code>

en: The number after `infixr` gives the precedence.
fr: Le nombre après `infixr` donne la priorité.

en: If you want to be able to print (`Show`), read (`Read`), test equality (`Eq`) and compare (`Ord`) your new data structure you can tell Haskell to derive the appropriate functions for you.
fr: Si vous voulez pouvoir écrire (`Show`), lire (`Read`), tester l'égalite (`Eq`) et comparer (`Ord`) votre nouvelle structure, vous pouvez demander à Haskell de dériver les fonctions appropriées pour vous.

> infixr 5 :::
> data List a = Nil | a ::: (List a) 
>               deriving (Show,Read,Eq,Ord)

en: When you add `deriving (Show)` to your data declaration, Haskell creates a `show` function for you.
en: We'll see soon how you can use your own `show` function.
fr: Quand vous ajoutez `deriving (Show)` à votre déclaration, Haskell crée une fonction `show` pour vous.
fr: Nous verrons bientôt comment utiliser sa propre fonction `show`.

> convertList [] = Nil
> convertList (x:xs) = x ::: convertList xs

> main = do
>       print (0 ::: 1 ::: Nil)
>       print (convertList [0,1])

en: This prints:
fr: Ceci écrit:

~~~
0 ::: (1 ::: Nil)
0 ::: (1 ::: Nil)
~~~
