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
en:  > which modifies the state of the world
 >
 > ~~~
 > main :: World -> World
 > ~~~
 >
fr:  > Une fonction aura des effets collatéraux si elle a ce type.
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
fr:  > qui doivent être passés à la prochauine action.
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
fr:  > Bonus: Haskell a un sucre syntaxique:
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

fr: Première remarque: on dirait de l'impératif.
en: First remark: this looks imperative.
fr: Haskell est assez puissant pour faire sembler impératif du code impur.
en: Haskell is powerful enough to make impure code look imperative.
fr: Par exemple, si vous le vouliez vous pourriez créer `while` en Haskell.
en: For example, if you wish you could create a `while` in Haskell.
fr: En fait, pour utiliser les `IO`, le style impératif est généralement plus approprié.
en: In fact, for dealing with `IO`, an imperative style is generally more appropriate.

fr: Mais vous devriez avoir remarqué que la notation est inhabituelle.
en: But you should have noticed that the notation is a bit unusual.
fr: Voici pourquoi, en détail.
en: Here is why, in detail.

fr: Dans un langage impur, l'état du monde peut être vu comme une énorme variable globale cachée.
en: In an impure language, the state of the world can be seen as a huge hidden global variable.
fr: Cette variable cachée est accessible par toutes les fonctions du langage/
en: This hidden variable is accessible by all functions of your language.
fr: Par exemple, vous pouvez lire et écrire un fichier dans n'importe quelle fonction.
en: For example, you can read and write a file in any function.
fr: L'existence hypothétique du fichier est une différence dans les états possibles que le monde peut prendre.
en: Whether a file exists or not is a difference in the possible states that the world can take.

fr: En Haskell cet état n'est pas caché.
en: In Haskell this state is not hidden.
fr: Au contraire, il est dit _explicitement_ que `main` est une fonction qui change _potentiellement_ l'état du monde.
en: Rather, it is _explicitly_ said that `main` is a function that _potentially_ changes the state of the world.
fr: Son type est donc quelque chose comme:
en: Its type is then something like:

<code class="haskell">
main :: World -> World
</code>

fr: 
en: Not all functions may have access to this variable.
fr: 
en: Those which have access to this variable are impure.
fr: 
en: Functions to which the world variable isn't provided are pure[^032001].

fr: [^032001]: 
en: [^032001]: There are some _unsafe_ exceptions to this rule. But you shouldn't see such use in a real application except maybe for debugging purposes.

Haskell considers the state of the world as an input variable to `main`.
But the real type of main is closer to this one[^032002]:

[^032002]: For the curious the real type is `data IO a = IO {unIO :: State# RealWorld -> (# State# RealWorld, a #)}`. All the `#` has to do with optimisation and I swapped the fields in my example. But this is the basic idea.

<code class="haskell">
main :: World -> ((),World)
</code>

The `()` type is the unit type.
Nothing to see here.

Now let's rewrite our main function with this in mind:

<code class="haskell">
main w0 =
    let (list,w1) = askUser w0 in
    let (x,w2) = print (sum list,w1) in
    x
</code>

First, we note that all functions which have side effects must have the type:

<code class="haskell">
World -> (a,World)
</code>

where `a` is the type of the result.
For example, a `getChar` function should have the type `World -> (Char,World)`.

Another thing to note is the trick to fix the order of evaluation.
In Haskell, in order to evaluate `f a b`, you have many choices:

- first eval `a` then `b` then `f a b`
- first eval `b` then `a` then `f a b`.
- eval `a` and `b` in parallel then `f a b`

This is true because we're working in a pure part of the language.

Now, if you look at the main function, it is clear you must eval the first
line before the second one since to evaluate the second line you have
to get a parameter given by the evaluation of the first line.

This trick works nicely.
The compiler will at each step provide a pointer to a new real world id.
Under the hood, `print` will evaluate as:

- print something on the screen
- modify the id of the world
- evaluate as `((),new world id)`.

Now, if you look at the style of the main function, it is clearly awkward.
Let's try to do the same to the askUser function:

<code class="haskell">
askUser :: World -> ([Integer],World)
</code>

Before:

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

After:

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

This is similar, but awkward.
Look at all these temporary `w?` names.

The lesson is: naive IO implementation in Pure functional languages is awkward!

Fortunately, there is a better way to handle this problem.
We see a pattern.
Each line is of the form:

<code class="haskell">
let (y,w') = action x w in
</code>

Even if for some line the first `x` argument isn't needed.
The output type is a couple, `(answer, newWorldValue)`.
Each function `f` must have a type similar to:

<code class="haskell">
f :: World -> (a,World)
</code>

Not only this, but we can also note that we always follow the same usage pattern:

<code class="haskell">
let (y,w1) = action1 w0 in
let (z,w2) = action2 w1 in
let (t,w3) = action3 w2 in
...
</code>

Each action can take from 0 to n parameters.
And in particular, each action can take a parameter from the result of a line above.

For example, we could also have:

<code class="haskell">
let (_,w1) = action1 x w0   in
let (z,w2) = action2 w1     in
let (_,w3) = action3 x z w2 in
...
</code>

And of course `actionN w :: (World) -> (a,World)`.

 > IMPORTANT: there are only two important patterns to consider:
 >
 > ~~~
 > let (x,w1) = action1 w0 in
 > let (y,w2) = action2 x w1 in
 > ~~~
 >
 > and
 >
 > ~~~
 > let (_,w1) = action1 w0 in
 > let (y,w2) = action2 w1 in
 > ~~~

leftblogimage("jocker_pencil_trick.jpg","Jocker pencil trick")

Now, we will do a magic trick.
We will make the temporary world symbol "disappear".
We will `bind` the two lines.
Let's define the `bind` function.
Its type is quite intimidating at first:

<code class="haskell">
bind :: (World -> (a,World))
        -> (a -> (World -> (b,World)))
        -> (World -> (b,World))
</code>

But remember that `(World -> (a,World))` is the type for an IO action.
Now let's rename it for clarity:

<code class="haskell">
type IO a = World -> (a, World)
</code>

Some examples of functions:

<code class="haskell">
getLine :: IO String
print :: Show a => a -> IO ()
</code>

`getLine` is an IO action which takes world as a parameter and returns a couple `(String,World)`.
This can be summarized as: `getLine` is of type `IO String`, which we also see as an IO action which will return a String "embeded inside an IO".

The function `print` is also interesting.
It takes one argument which can be shown.
In fact it takes two arguments.
The first is the value to print and the other is the state of world.
It then returns a couple of type `((),World)`.
This means that it changes the state of the world, but doesn't yield any more data.

This type helps us simplify the type of `bind`:

<code class="haskell">
bind :: IO a
        -> (a -> IO b)
        -> IO b
</code>

It says that `bind` takes two IO actions as parameters and returns another IO action.

Now, remember the _important_ patterns. The first was:

<code class="haskell">
let (x,w1) = action1 w0 in
let (y,w2) = action2 x w1 in
(y,w2)
</code>

Look at the types:

<code class="haskell">
action1  :: IO a
action2  :: a -> IO b
(y,w2)   :: IO b
</code>

Doesn't it seem familiar?

<code class="haskell">
(bind action1 action2) w0 =
    let (x, w1) = action1 w0
        (y, w2) = action2 x w1
    in  (y, w2)
</code>

The idea is to hide the World argument with this function. Let's go:
As an example imagine if we wanted to simulate:

<code class="haskell">
let (line1,w1) = getLine w0 in
let ((),w2) = print line1 in
((),w2)
</code>

Now, using the bind function:

<code class="haskell">
(res,w2) = (bind getLine (\l -> print l)) w0
</code>

As print is of type `(World -> ((),World))`, we know `res = ()` (null type).
If you didn't see what was magic here, let's try with three lines this time.


<code class="haskell">
let (line1,w1) = getLine w0 in
let (line2,w2) = getLine w1 in
let ((),w3) = print (line1 ++ line2) in
((),w3)
</code>

Which is equivalent to:

<code class="haskell">
(res,w3) = (bind getLine (\line1 ->
             (bind getLine (\line2 ->
               print (line1 ++ line2))))) w0
</code>

Didn't you notice something?
Yes, no temporary World variables are used anywhere!
This is _MA_. _GIC_.

We can use a better notation.
Let's use `(>>=)` instead of `bind`.
`(>>=)` is an infix function like
`(+)`; reminder `3 + 4 ⇔ (+) 3 4`

<code class="haskell">
(res,w3) = (getLine >>=
           (\line1 -> getLine >>=
           (\line2 -> print (line1 ++ line2)))) w0
</code>

Ho Ho Ho! Merry Christmas Everyone!
Haskell has made syntactical sugar for us:

<code class="haskell">
do
  x <- action1
  y <- action2
  z <- action3
  ...
</code>

Is replaced by:

<code class="haskell">
action1 >>= (\x ->
action2 >>= (\y ->
action3 >>= (\z ->
...
)))
</code>

Note that you can use `x` in `action2` and `x` and `y` in `action3`.

But what about the lines not using the `<-`?
Easy, another function `blindBind`:

<code class="haskell">
blindBind :: IO a -> IO b -> IO b
blindBind action1 action2 w0 =
    bind action (\_ -> action2) w0
</code>

I didn't simplify this definition for the purposes of clarity.
Of course we can use a better notation, we'll use the `(>>)` operator.

And

<code class="haskell">
do
    action1
    action2
    action3
</code>

Is transformed into

<code class="haskell">
action1 >>
action2 >>
action3
</code>

Also, another function is quite useful.

<code class="haskell">
putInIO :: a -> IO a
putInIO x = IO (\w -> (x,w))
</code>

This is the general way to put pure values inside the "IO context".
The general name for `putInIO` is `return`.
This is quite a bad name when you learn Haskell. `return` is very different from what you might be used to.
