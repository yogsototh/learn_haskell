en: <h3 id="deal-with-io">Deal With IO</h3>
fr: <h3 id="deal-with-io">S'occuper de l'E/S (IO)</h3>

blogimage("magritte_carte_blanche.jpg","Magritte, Carte blanche")

 > %tldr
 >
en:  > A typical function doing `IO` looks a lot like an imperative program:
fr:  > Une fonction typique qui fait de l'`IO` ressemble à un programme impératif:
 > 
 > ~~~
 > f :: IO a
 > f = do
 >   x <- action1
 >   action2 x
 >   y <- action3
 >   action4 x y
 > ~~~
 >
en:  > - To set a value to an object we use `<-` .
en:  > - The type of each line is `IO *`;
en:  >   in this example:
fr:  > - Pour définir la valeur d'un objet on utilise `<-` .
fr:  > - Le type de chaque ligne est `IO *`;
fr:  >   dans cet exemple:
 >   - `action1     :: IO b`
 >   - `action2 x   :: IO ()`
 >   - `action3     :: IO c`
 >   - `action4 x y :: IO a`
 >   - `x :: b`, `y :: c`
en:  > - Few objects have the type `IO a`, this should help you choose.
en:  >   In particular you cannot use pure functions directly here.
en:  >   To use pure functions you could do `action2 (purefunction x)` for example.
fr:  > - Quelques objets ont le type `IO a`, cela devrait vous aider à choisir.
fr:  >   En particulier vous ne pouvez pas utiliser de fonctions pures directement ici.
fr:  >   Pour utiliser des fonctions pures vous pourriez faire `action2 (pureFunction x)` par exemple.

en: In this section, I will explain how to use IO, not how it works.
en: You'll see how Haskell separates the pure from the impure parts of the program.
fr: Dans cette section, je vais expliquer comment utiliser l'IO, pas comment ça marche.
fr: Vous verrez comment Haskell sépare les parties pures et impures du programme.

en: Don't stop because you're trying to understand the details of the syntax.
en: Answers will come in the next section.
fr: Ne vous arrêtez pas sur les détails de la syntaxe
fr: Les réponses viendront dans la section suivante.

en: What to achieve?
fr: Que cherchons-nous à faire?

en:  > Ask a user to enter a list of numbers.
en:  > Print the sum of the numbers
fr:  > Demander une liste de nombres à l'utilisateur.
fr:  > Afficher la somme de ces nombres.

> toList :: String -> [Integer]
> toList input = read ("[" ++ input ++ "]")
>
> main = do
>   putStrLn "Enter a list of numbers (separated by comma):"
>   input <- getLine
>   print $ sum (toList input)

en: It should be straightforward to understand the behavior of this program.
en: Let's analyze the types in more detail.
fr: Il devrait être simple de comprendre le comportement de ce programme.
fr: Analysons les types en détails.

~~~
putStrLn :: String -> IO ()
getLine  :: IO String
print    :: Show a => a -> IO ()
~~~

en: Or more interestingly, we note that each expression in the `do` block has a type of `IO a`.
fr: Ou, plus intéressant, on remarque que chaque expression dans le bloc `do` est de type `IO a`.

<pre>
main = do
  putStrLn "Enter ... " :: <span class="high">IO ()</span>
  getLine               :: <span class="high">IO String</span>
  print Something       :: <span class="high">IO ()</span>
</pre>

en: We should also pay attention to the effect of the `<-` symbol.
fr: Nous devrions aussi prêter attention à l'effet du symbole `<-`.

~~~
do
 x <- something
~~~

en: If `something :: IO a` then `x :: a`.
fr: Si `something :: IO a` alors `x :: a`.

en: Another important note about using `IO`:
en: All lines in a do block must be of one of the two forms:
fr: Une autre remarque important sur l'`IO`:
fr: Toutes les lignes d'un bloc `do` doivent être d'une des deux formes:

~~~
action1             :: IO a
                    -- in this case, generally a = ()
~~~

ou

~~~
value <- action2    -- where
                    -- action2 :: IO b
                    -- value   :: b
~~~

en: These two kinds of line will correspond to two different ways of sequencing actions.
en: The meaning of this sentence should be clearer by the end of the next section.
fr: Ces deux types de ligne correspondent à deux différents types de séquençage d'action.
fr: La signification de cette phrase devrait être plus claire à la fin de la prochaine section.
