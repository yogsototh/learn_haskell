en: Now let's see how this program behaves.
en: For example, what happens if the user enters something strange?
en: Let's try:
fr: Maintenant voyons comment ce programme se comporte.
fr: Par exemple, que ce passe-t-il si l'utilisateur entre une mauvaise valeur?
fr: Essayons :

~~~
    % runghc 02_progressive_io_example.lhs
    Enter a list of numbers (separated by comma):
    foo
    Prelude.read: no parse
~~~


en: Argh! An evil error message and a crash!
fr: Argh! Un message d'erreur effrayant et un crash !
en: Our first improvement will simply be to answer with a more friendly message.
fr: Notre première amélioration sera de répondre avec un message plus amical.

en: In order to do this, we must detect that something went wrong.
fr: Pour faire cela, nous devons détecter que quelque chose s'est mal passé.
en: Here is one way to do this: use the type `Maybe`.
fr: Voici un moyen de le faire : utiliser le type `Maybe`.
en: This is a very common type in Haskell.
fr: C'est un type très utilisé en Haskell.

> import Data.Maybe

en: What is this thing? `Maybe` is a type which takes one parameter.
fr: Mais qu'est-ce que c'est ? `Maybe` est un type qui prend un paramètre.
en: Its definition is:
fr: Sa définition est :

<code class="haskell">
data Maybe a = Nothing | Just a
</code>

fr: C'est un bon moyen de dire qu'il y a eu une erreur en essayant de créer/évaluer
en: This is a nice way to tell there was an error while trying to create/compute
fr: une valeur.
en: a value.
fr: La fonction `maybeRead` en est un bon exemple.
en: The `maybeRead` function is a great example of this.
fr: C'est une fonction similaire à `read`[^1],
en: This is a function similar to the function `read`[^1],
fr: mais s'il y a un problème, la valeur retournée est `Nothing`.
en: but if something goes wrong the returned value is `Nothing`.
fr: Si la valeur est bonne, la valeur retournée est `Just <la valeur>`.
en: If the value is right, it returns `Just <the value>`.
fr: Ne vous efforcez pas trop de comprendre cette fonction.
en: Don't try to understand too much of this function.
fr: J'utilise une fonction de plus bas niveau que `read` : `reads`.
en: I use a lower level function than `read`: `reads`.

fr: [^1]: Qui est elle-même très similaire à la fonction `eval` de javascript, appliquée sur une chaîne contenant du code au format JSON.
en: [^1]: Which is itself very similar to the javascript `eval` function, that is applied to a string containing JSON.

> maybeRead :: Read a => String -> Maybe a
> maybeRead s = case reads s of
>                   [(x,"")]    -> Just x
>                   _           -> Nothing

fr: Maintenant, pour être plus lisible, on définit une fonction comme ceci :
en: Now to be a bit more readable, we define a function which goes like this:
fr: Si la chaîne a un mauvais format, elle retournera `Nothing`.
en: If the string has the wrong format, it will return `Nothing`.
fr: Sinon, par exemple pour "1,2,3", cela retournera `Just [1,2,3]`.
en: Otherwise, for example for "1,2,3", it will return `Just [1,2,3]`.

> getListFromString :: String -> Maybe [Integer]
> getListFromString str = maybeRead $ "[" ++ str ++ "]"


en: We simply have to test the value in our main function.
fr: Nous avons juste à tester la valeur dans notre fonction principale.

> main :: IO ()
> main = do
>   putStrLn "Enter a list of numbers (separated by comma):"
>   input <- getLine
>   let maybeList = getListFromString input in
>       case maybeList of
>           Just l  -> print (sum l)
>           Nothing -> error "Bad format. Good Bye."

fr: En cas d'erreur, on affiche un joli message.
en: In case of error, we display a nice error message.

fr: Notez que le type de chaque expression dans le bloc `do` de `main` reste de la forme `IO a`.
en: Note that the type of each expression in the main's `do` block remains of the form `IO a`.
fr: La seule construction étrange est `error`.
en: The only strange construction is `error`.
fr: Disons juste que `error msg` prend le type nécessaire (ici, `IO ()`).
en: I'll just say here that `error msg` takes the needed type (here `IO ()`).

fr: Une chose très importante à noter est le type de toutes les fonctions définies jusqu'ici.
en: One very important thing to note is the type of all the functions defined so far.
fr: Il n'y a qu'une seule fonction qui contient `IO` dans son type : `main`.
en: There is only one function which contains `IO` in its type: `main`.
fr: Cela signifie que `main` est impure.
en: This means main is impure.
fr: Mais `main` utilise `getListFromString`, qui, elle, est pure.
en: But main uses `getListFromString` which is pure.
fr: Nous pouvons donc facilement repérer quelles fonctions sont pures
en: So it's clear just by looking at declared types which functions are pure and
fr: et lesquelles sont impures, seulement en regardant leur type.
en: which are impure.

fr: Pourquoi la pureté a-t-elle de l'importance?
en: Why does purity matter?
fr: Parmi ses nombreux avantages, en voici trois :
en: Among the many advantages, here are three:

fr: - Il est beaucoup plus facile de penser à du code pur qu'à du code impur.
en: - It is far easier to think about pure code than impure code.
fr: - La pureté vous protège de tous les bugs difficiles à reproduire dûs aux [effets de bord](https://fr.wikipedia.org/wiki/Effet_de_bord_(informatique)).
en: - Purity protects you from all the hard-to-reproduce bugs that are due to side effects.
fr: - Vous pouvez évaluer des fonctions pures dans n'importe quel ordre ou en parallèle, sans prendre de risques.
en: - You can evaluate pure functions in any order or in parallel without risk.

fr: C'est pourquoi vous devriez mettre le plus de code possible dans des fonctions pures.
en: This is why you should generally put as most code as possible inside pure functions.
