fr: <h3 id="io-trick-explained">Le truc des IO révélé</h3>
en: <h3 id="io-trick-explained">IO trick explained</h3>

blogimage("magritte_pipe.jpg","Magritte, ceci n'est pas une pipe")

fr:  > Voici un %tlal pour cette section.
en:  > Here is a %tldr for this section.
 >
fr:  > Pour séparer les parties pures et impures,
en:  > To separate pure and impure parts,
fr:  > `main` est définie comme une fonction.
en:  > `main` is defined as a function
fr:  > qui modifie l'état du monde.
en:  > which modifies the state of the world.
 >
 > ~~~
 > main :: World -> World
 > ~~~
 >
fr:  > Une fonction aura des effets de bord si elle a ce type.
en:  > A function is guaranteed to have side effects only if it has this type.
fr:  > Mais regardez cette fonction `main` typique:
en:  > But look at a typical main function:
 >
 > ~~~
 > 
 > main w0 =
 >     let (v1,w1) = action1 w0 in
 >     let (v2,w2) = action2 v1 w1 in
 >     let (v3,w3) = action3 v2 w2 in
 >     action4 v3 w3
 > ~~~
 >
fr:  > Nous avons beaucoup d'élements temporaires (ici, `w1`, `w2` et `w3`) 
en:  > We have a lot of temporary elements (here `w1`, `w2` and `w3`)
fr:  > qui doivent être passés à l'action suivante.
en:  > which must be passed on to the next action.
 >
fr:  > Nous créons une fonction `bind` ou `(>>=)`.
en:  > We create a function `bind` or `(>>=)`.
fr:  > Avec `bind` nous n'avons plus besoin de noms temporaires.
en:  > With `bind` we don't need temporary names anymore.
 >
 > ~~~
 > main =
 >   action1 >>= action2 >>= action3 >>= action4
 > ~~~
 >
fr:  > Bonus: Haskell a du sucre syntaxique :
en:  > Bonus: Haskell has syntactical sugar for us:
 >
 > ~~~
 > main = do
 >   v1 <- action1
 >   v2 <- action2 v1
 >   v3 <- action3 v2
 >   action4 v3
 > ~~~


fr: Pourquoi avons-nous utilisé cette syntaxe étrange, et quel est exactement le type `IO`?
en: Why did we use this strange syntax, and what exactly is this `IO` type?
fr: Cela peut sembler un peu magique.
en: It looks a bit like magic.

fr: Pour l'instant, oublions les parties pures de notre programme, et concentrons-nous
en: For now let's just forget all about the pure parts of our program, and focus
fr: sur les parties impures:
en: on the impure parts:

<code class="haskell">
askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers (separated by commas):"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> return l
          Nothing -> askUser

main :: IO ()
main = do
  list <- askUser
  print $ sum list
</code>

fr: Première remarque : on dirait de l'impératif.
en: First remark: this looks imperative.
fr: Haskell est assez puissant pour faire sembler impératif du code impur.
en: Haskell is powerful enough to make impure code look imperative.
fr: Par exemple, si vous le vouliez vous pourriez créer une boucle `while` en Haskell.
en: For example, if you wish you could create a `while` in Haskell.
fr: En fait, pour utiliser les `IO`, le style impératif est en général plus approprié.
en: In fact, for dealing with `IO`, an imperative style is generally more appropriate.

fr: Mais vous devriez avoir remarqué que la notation est inhabituelle.
en: But you should have noticed that the notation is a bit unusual.
fr: Voici pourquoi, en détail.
en: Here is why, in detail.

fr: Dans un langage impur, l'état du monde peut être vu comme une énorme variable globale cachée.
en: In an impure language, the state of the world can be seen as a huge hidden global variable.
fr: Cette variable cachée est accessible par toutes les fonctions du langage.
en: This hidden variable is accessible by all functions of your language.
fr: Par exemple, vous pouvez lire et écrire dans un fichier avec n'importe quelle fonction.
en: For example, you can read and write a file in any function.
fr: Le fait que le fichier putatif existe ou non est une éventualité qui relève des états possibles que le monde courant peut prendre.
en: Whether a file exists or not is a difference in the possible states that the world can take.

fr: En Haskell l'état courant du monde n'est pas caché.
en: In Haskell the current state of the world is not hidden.
fr: Au contraire, il est dit _explicitement_ que `main` est une fonction qui change _potentiellement_ l'état du monde.
en: Rather, it is _explicitly_ said that `main` is a function that _potentially_ changes the state of the world.
fr: Son type est donc quelque chose comme :
en: Its type is then something like:

<code class="haskell">
main :: World -> World
</code>

fr: Les fonctions ne sont pas toutes susceptibles de modifier cette variable.
en: Not all functions may access this variable.
fr: Celle qui peuvent la modifier sont impures.
en: Those which have access to this variable are impure.
fr: Les fonctions qui ne peuvent pas agir sur la variable sont pures[^032001].
en: Functions to which the world variable isn't provided are pure[^032001].

fr: [^032001]: Il y a quelques exceptions _peu sûres_ à cette règle. Mais vous ne devriez pas en voir en application réelle, sauf pour le _debugging_.
en: [^032001]: There are some _unsafe_ exceptions to this rule. But you shouldn't see such use in a real application except maybe for debugging purposes.

fr: Haskell considère l'état du monde comme une variable à passer à `main`.
en: Haskell considers the state of the world as an input variable to `main`.
fr: Mais son type réel est plus proche de celui ci[^032002] :
en: But the real type of main is closer to this one[^032002]:

fr: [^032002]: Pour les curieux, le vrai type est `data IO a = IO {unIO :: State# RealWorld -> (# State# RealWorld, a #)}`. Tous les `#` ont rapport avec l'optimisation et j'ai échangé quelques champs dans mon exemple. Mais c'est l'idée de base.
en: [^032002]: For the curious ones, the real type is `data IO a = IO {unIO :: State# RealWorld -> (# State# RealWorld, a #)}`. All the `#` has to do with optimisation and I swapped the fields in my example. But this is the basic idea.

<code class="haskell">
main :: World -> ((),World)
</code>

fr: Le type `()` est le type "unit".
en: The `()` type is the unit type.
fr: Rien à voir ici.
en: Nothing to see here.

fr: Maintenant réécrivons notre fonction `main` avec cela à l'esprit :
en: Now let's rewrite our main function with this in mind:

<code class="haskell">
main w0 =
    let (list,w1) = askUser w0 in
    let (x,w2) = print (sum list,w1) in
    x
</code>

fr: D'abord, on remarque que toutes les fonctions avec des effets de bord doivent avoir le type :
en: First, we note that all functions which have side effects must have the type:

<code class="haskell">
World -> (a,World)
</code>

fr: où `a` est le type du résultat.
en: where `a` is the type of the result.
fr: Par exemple, une fonction `getChar` aura le type `World -> (Char, World).
en: For example, a `getChar` function should have the type `World -> (Char, World)`.

fr: Une autre chose à noter est l'astuce pour corriger l'ordre d'évaluation.
en: Another thing to note is the trick to fix the order of evaluation.
fr: En Haskell, pour évaluer `f a b`, vous avez l'embarras du choix :
en: In Haskell, in order to evaluate `f a b`, you have many choices:

fr: - évaluer d'abord `a` puis `b` puis `f a b`
en: - first eval `a` then `b` then `f a b`
fr: - évaluer d'abord `b` puis `a` puis `f a b`
en: - first eval `b` then `a` then `f a b`.
fr: - évaluer `a` et `b` parallèlement, puis `f a b`
en: - eval `a` and `b` in parallel then `f a b`

fr: Cela vient du fait que nous avons recours à une partie pure du langage.
en: This is true because we're working in a pure part of the language.

fr: Maintenant, si vous regardez la fonction `main`, vous voyez tout de suite qu'il faut évaluer la première
en: Now, if you look at the main function, it is clear you must eval the first
fr: ligne avant la seconde, car pour évaluer la seconde ligne vous devez
en: line before the second one since to evaluate the second line you have
fr: utliser un paramètre donné suite à l'évaluation de la première ligne.
en: to get a parameter given by the evaluation of the first line.

fr: Cette astuce fonctionne très bien.
en: This trick works like a charm.
fr: Le compilateur donnera à chaque étape un pointeur sur l'id du nouveau monde courant.
en: The compiler will at each step provide a pointer to a new real world id.
fr: En réalité, `print` sera évaluée comme suit :
en: Under the hood, `print` will evaluate as:

fr: - Écrit quelque chose sur l'écran
en: - print something on the screen
fr: - Modifie l'id du monde
en: - modify the id of the world
fr: - renvoyer `((), id du nouveau monde)`.
en: - evaluate as `((),new world id)`.

fr: Maintenant, si jetez un oeil au style de la fonction `main`, vous remarquerez qu'il est clairement peu commode.
en: Now, if you look at the style of the main function, it is clearly awkward.
fr: Essayons de faire la même chose avec la fonction `askUser` :
en: Let's try to do the same to the `askUser` function:

<code class="haskell">
askUser :: World -> ([Integer],World)
</code>

fr: Avant :
en: Before:

<code class="haskell">
askUser :: IO [Integer]
askUser = do
  putStrLn "Enter a list of numbers:"
  input <- getLine
  let maybeList = getListFromString input in
      case maybeList of
          Just l  -> return l
          Nothing -> askUser
</code>

fr: Après :
en: After:

<code class="haskell">
askUser w0 =
    let (_,w1)     = putStrLn "Enter a list of numbers:" in
    let (input,w2) = getLine w1 in
    let (l,w3)     = case getListFromString input of
                      Just l   -> (l,w2)
                      Nothing  -> askUser w2
    in
        (l,w3)
</code>

fr: C'est similaire, mais peu commode.
en: This is similar, but awkward.
fr: Voyez-vous toutes ces variables temporaires `w?`.
en: Look at all these temporary `w?` names.

fr: Voici la leçon : une implémentation naïve des IO dans les langages fonctionnels purs serait maladroite !
en: The lesson is: naive IO implementation in Pure functional languages is awkward!

fr: Heureusement, il y a un meilleur moyen de résoudre ce problème.
en: Fortunately, there is a better way to handle this problem.
fr: Nous voyons un motif.
en: We see a pattern.
fr: Chaque ligne est de la forme :
en: Each line is of the form:

<code class="haskell">
let (y,w') = action x w in
</code>

fr: Même si pour certaines lignes l'argument `x` n'est pas nécessaire.
en: Even if for some lines the first `x` argument isn't needed.
fr: La sortie est un couple, `(answer, newWorldValue)`.
en: The output type is a couple, `(answer, newWorldValue)`.
fr: Chaque fonction `f` doit avoir un type similaire à :
en: Each function `f` must have a type similar to:

<code class="haskell">
f :: World -> (a,World)
</code>

fr: Et ce n'est pas fini, nous pouvons aussi remarquer que nous suivons toujours le même motif :
en: Not only this, but we can also note that we always follow the same usage pattern:

<code class="haskell">
let (y,w1) = action1 w0 in
let (z,w2) = action2 w1 in
let (t,w3) = action3 w2 in
...
</code>

fr: Chaque action peut prendre de 0 à n paramètres.
en: Each action can take from 0 to n parameters.
fr: Et en particulier, chaque action prend comme paramètre le résultat de la ligne précédente.
en: And in particular, each action can take a parameter from the result of a line above.

fr: Par exemple, nous pourrions aussi avoir :
en: For example, we could also have:

<code class="haskell">
let (_,w1) = action1 x w0   in
let (z,w2) = action2 w1     in
let (_,w3) = action3 z w2 in
...
</code>

fr: Avec, bien entendu, `actionN w :: (World) -> (a,World)`.
en: With, of course: `actionN w :: (World) -> (a,World)`.

fr:  > IMPORTANT: Il y a seulement 2 schémas importants à considérer :
en:  > IMPORTANT: there are only two important patterns to consider:
 >
 > ~~~
 > let (x,w1) = action1 w0 in
 > let (y,w2) = action2 x w1 in
 > ~~~
 >
fr:  > et 
en:  > and
 >
 > ~~~
 > let (_,w1) = action1 w0 in
 > let (y,w2) = action2 w1 in
 > ~~~

leftblogimage("jocker_pencil_trick.jpg","Jocker pencil trick")

fr: Maintenant, préparez-vous pour un petit tour de magie !
en: Now, we will do a magic trick.
fr: Faisons disparaître les variables temporaires de monde courant.
en: We will make the temporary world symbols "disappear".
fr: Nous allons `attacher` (_NDT: `bind` en anglais_) les deux lignes.
en: We will `bind` the two lines.
fr: Définissons la fonction `bind`.
en: Let's define the `bind` function.
fr: Son type est assez intimidant au début :
en: Its type is quite intimidating at first:

<code class="haskell">
bind :: (World -> (a,World))
        -> (a -> (World -> (b,World)))
        -> (World -> (b,World))
</code>

fr: Mais gardez en tête que `(World -> (a,World))` est le type d'une action d'IO.
en: But remember that `(World -> (a,World))` is the type for an IO action.
fr: Renommons-le pour plus de clarté :
en: Now let's rename it for clarity:

<code class="haskell">
type IO a = World -> (a, World)
</code>

fr: Quelques exemples de fonctions :
en: Some examples of functions:

<code class="haskell">
getLine :: IO String
print :: Show a => a -> IO ()
</code>

fr: `getLine` est une action d'E/S qui prend le monde en paramètre et retourne un couple `(String, World)`.
en: `getLine` is an IO action which takes world as a parameter and returns a couple `(String, World)`.
fr: Cela peut être résumé par : `getLine` est de type `IO String`, que nous pouvons voir comme une action d'E/S qui retournera une chaîne de caractères "dans une E/S".
en: This can be summarized as: `getLine` is of type `IO String`, which we also see as an IO action which will return a String "embeded inside an IO".

fr: La fonction `print` est elle aussi intéressante.
en: The function `print` is also interesting.
fr: Elle prend un argument qui peut être montré avec `show`.
en: It takes one argument which can be shown.
fr: En fait, elle prend deux arguments.
en: In fact it takes two arguments.
fr: Le premier est la valeur et le deuxième est l'état du monde.
en: The first is the value to print and the other is the state of world.
fr: Elle retourne un couple de type `((), World)`.
en: It then returns a couple of type `((), World)`.
fr: Cela signifie qu'elle change l'état du monde, mais ne produit pas d'autre donnée.
en: This means that it changes the state of the world, but doesn't yield any more data.

fr: Ce nouveau type `IO a` nous aide à simplifier le type de `bind` :
en: This new `IO a` type helps us simplify the type of `bind`:

<code class="haskell">
bind :: IO a
        -> (a -> IO b)
        -> IO b
</code>

fr: Cela dit que `bind` prend deux actions d'E/S en paramètres et retourne une autre action d'E/S.
en: It says that `bind` takes two IO actions as parameters and returns another IO action.

fr: Maintenant, rappelez-vous des motifs _importants_. Le premier était :
en: Now, remember the _important_ patterns. The first was:

<code class="haskell">
pattern1 w0 = 
 let (x,w1) = action1 w0 in
 let (y,w2) = action2 x w1 in
 (y,w2)
</code>

fr: Voyez les types :
en: Look at the types:

<code class="haskell">
action1  :: IO a
action2  :: a -> IO b
pattern1 :: IO b
</code>

fr: Cela ne vous semble-t-il pas familier ?
en: Doesn't it seem familiar?

<code class="haskell">
(bind action1 action2) w0 =
    let (x, w1) = action1 w0
        (y, w2) = action2 x w1
    in  (y, w2)
</code>

fr: L'idée est de cacher l'argument `World` avec cette fonction. Allons-y !
en: The idea is to hide the World argument with this function. Let's go:
fr: Par exemple si nous avions voulu simuler :
en: As an example imagine if we wanted to simulate:

<code class="haskell">
let (line1, w1) = getLine w0 in
let ((), w2) = print line1 in
((), w2)
</code>

fr: Maintenant, en utilisant la fonction `bind` :
en: Now, using the `bind` function:

<code class="haskell">
(res, w2) = (bind getLine print) w0
</code>

fr: Comme `print` est de type `Show a => a -> (World -> ((), World))`, nous savons que `res = ()` (type `unit`)
en: As print is of type `Show a => a -> (World -> ((), World))`, we know `res = ()` (`unit` type).
fr: Si vous ne voyez pas ce qui est magique ici, essayons avec trois lignes cette fois.
en: If you didn't see what was magic here, let's try with three lines this time.


<code class="haskell">
let (line1,w1) = getLine w0 in
let (line2,w2) = getLine w1 in
let ((),w3) = print (line1 ++ line2) in
((),w3)
</code>

fr: Qui est équivalent à :
en: Which is equivalent to:

<code class="haskell">
(res,w3) = (bind getLine (\line1 ->
             (bind getLine (\line2 ->
               print (line1 ++ line2))))) w0
</code>

fr: Avez-vous remarqué quelque chose ?
en: Didn't you notice something?
fr: Oui, aucune variable `World` temporaire n'est utilisée !
en: Yes, no temporary World variables are used anywhere!
fr: C'est _MA_._GIQUE_.
en: This is _MA_. _GIC_.

fr: Nous pouvons utiliser une meilleure notation.
en: We can use a better notation.
fr: Utilisons `(>>=)` au lieu de `bind`.
en: Let's use `(>>=)` instead of `bind`.
fr: `(>>=)` est une fonction infixe, comme
en: `(>>=)` is an infix function like
fr: `(+)`; pour mémoire : `3 + 4 ⇔ (+) 3 4`
en: `(+)`; reminder `3 + 4 ⇔ (+) 3 4`

<code class="haskell">
(res,w3) = (getLine >>=
           (\line1 -> getLine >>=
           (\line2 -> print (line1 ++ line2)))) w0
</code>

fr: Ho Ho Ho! Joyeux Noël !
fr; Haskell a confectionné du sucre syntaxique pour vous :
en: Ho Ho Ho! Merry Christmas Everyone!
en: Haskell has made syntactical sugar for us:

<code class="haskell">
do
  x <- action1
  y <- action2
  z <- action3
  ...
</code>

fr: Est remplacé par :
en: Is replaced by:

<code class="haskell">
action1 >>= (\x ->
action2 >>= (\y ->
action3 >>= (\z ->
...
)))
</code>

fr: Remarquez que vous pouvez utliser `x` dans `action2` et `x` et `y` dans `action3`.
en: Note that you can use `x` in `action2` and `x` and `y` in `action3`.

fr: Mais que se passe-t-il pour les lignes qui n'utilisent pas le `<-` ?
en: But what about the lines not using the `<-`?
fr: Facile, une autre fonction `blindBind` :
en: Easy, another function `blindBind`:

<code class="haskell">
blindBind :: IO a -> IO b -> IO b
blindBind action1 action2 w0 =
    bind action (\_ -> action2) w0
</code>

fr: Je n'ai pas simplifié cette définition pour plus de clarté.
en: I didn't simplify this definition for the purposes of clarity.
fr: Bien sûr, nous pouvons utiliser une meilleure notation avec l'opérateur `(>>)`.
en: Of course, we can use a better notation: we'll use the `(>>)` operator.

fr: Et
en: And

<code class="haskell">
do
    action1
    action2
    action3
</code>

fr: Devient
en: Is transformed into

<code class="haskell">
action1 >>
action2 >>
action3
</code>

fr: Enfin, une autre fonction est plutôt utile.
en: Also, another function is quite useful.

<code class="haskell">
putInIO :: a -> IO a
putInIO x = IO (\w -> (x,w))
</code>

fr: D'une manière générale, c'est une façon de mettre des valeurs pures dans le "contexte d'E/S".
en: This is the general way to put pure values inside the "IO context".
fr: Le nom général pour `putInIO` est `return`.
en: The general name for `putInIO` is `return`.
fr: C'est un plutôt un mauvais nom lorsque vous commencez à programmer en Haskell. `return` est très différent de ce à quoi vous pourriez être habitué.
en: This is quite a bad name when you learn Haskell. `return` is very different from what you might be used to.
