fr: La prochaine étape sera de demader la liste de nombre à l'utilisateur encore et encore jusqu'à ce qu'il entre une réponse valide
en: Our next iteration will be to prompt the user again and again until she enters a valid answer.

fr: Nous gardons la première partie:
en: We keep the first part:

> import Data.Maybe
>
> maybeRead :: Read a => String -> Maybe a
> maybeRead s = case reads s of
>                   [(x,"")]    -> Just x
>                   _           -> Nothing
> getListFromString :: String -> Maybe [Integer]
> getListFromString str = maybeRead $ "[" ++ str ++ "]"

fr: Maintenant nous créons la fonction qui demandera une liste d'entiers à l'utilisateur
en: Now we create a function which will ask the user for an list of integers
fr: jusqu'à ce que l'entrée soit correcte
en: until the input is right.

> askUser :: IO [Integer]
> askUser = do
>   putStrLn "Enter a list of numbers (separated by comma):"
>   input <- getLine
>   let maybeList = getListFromString input in
>       case maybeList of
>           Just l  -> return l
>           Nothing -> askUser

fr: Cette fonction est de type `IO [Integer]`.
en: This function is of type `IO [Integer]`.
fr: cela signifie que la valeur trouvée est de type `[Integer`] et est le résultat d'actions d'E/S.
en: Such a type means that we retrieved a value of type `[Integer]` through some IO actions.
fr: Certaines personnes expliqueraient en agitant leurs mains:
en: Some people might explain while waving their hands:

fr:  > «C'est un `[Integer]` dans un `IO`»
en:  > «This is an `[Integer]` inside an `IO`»

fr: Si vous voulez comprendre les détails derrière tout cela, vous devrez lire la prochaine section.
en: If you want to understand the details behind all of this, you'll have to read the next section.
fr: MAis si vous voulez seulement _utiliser_ l'E/S, pratiquer juste une peu et rappelez-vous de penser aux types.
en: But really, if you just want to _use_ IO just practice a little and remember to think about the type.

fr: Finalement notre fonction `main`est plus simple:
en: Finally our main function is much simpler:

> main :: IO ()
> main = do
>   list <- askUser
>   print $ sum list

fr: Nous avons fini notre introduction à l'`IO`.
en: We have finished with our introduction to `IO`.
fr: C'était rapide. Voici les principales choses à se rappeler:
en: This was quite fast. Here are the main things to remember:

fr: - Dans le bloc `do`, chaque expression doit avoir le type `IO a`.
en: - in the `do` block, each expression must have the type `IO a`.
fr: Vous êtes donc limité dans le nombre d'expression disponibles.
en: You are then limited in the number of expressions available.
fr: Par exemple, `getLine`, `print`, `putStrLn`, etc...
en: For example, `getLine`, `print`, `putStrLn`, etc...
fr: - Essayez d'externaliser le plus possible les fonctions pures.
en: - Try to externalize the pure functions as much as possible.
fr: - le type `IO a` signifie: un _action_ d'E/S qui retourne un élément de type a.
en: - the `IO a` type means: an IO _action_ which returns an element of type `a`.
fr: L'`IO` représente des actions; `IO a` est le type d'une fonction.
en: `IO` represents actions; under the hood, `IO a` is the type of a function.
fr: Lisez la prochaine section si vous êtes curieux.
en: Read the next section if you are curious.

fr: Si vous pratiquez un peu, vous devriez être capable d'_utiliser_ l'`IO`.
en: If you practice a bit, you should be able to _use_ `IO`.

fr:  > -Exercices_:
en:  > _Exercises_:
 >
fr:  > - Faites un programme qui additionne tous ses arguments. Utilisez la fonction `getArgs`.
en:  > - Make a program that sums all of its arguments. Hint: use the function `getArgs`.
