en: The solution: don't declare a type for `f` for the moment and let Haskell infer the most general type for us:
fr: La soulution: ne déclarez pas de type pour `f` pour le moment et laissez Haskell inférer le type le plus général pour nous:

> f x y = x*x + y*y
>
> main = print (f 2.3 4.2)

en: It works! 
en: Luckily, we don't have to declare a new function for every single type.
en: For example, in `C`, you'll have to declare a function for `int`, for `float`, for `long`, for `double`, etc...
fr: Maintenant, ça marche!
fr: Heureursement, nous n'avons pas à déclarer un nouvelle fonction pour chaque type différent.
fr: Par exemple, en `C`, vous auriez dû déclarer un fonction pour `int`, pour `float`, pour `long`, pour `double`, etc...

en: But, what type should we declare?
en: To discover the type Haskell has found for us, just launch ghci:
fr: Mais quel type devons nous déclarer?
fr: Pour découvrir le type que Haskell a trouvé pour nous, lançons ghci:

<pre><span class="low">
%</span> ghci<span class="low"><code>
GHCi, version 7.0.4: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Loading package ffi-1.0 ... linking ... done.
Prelude></code></span> let f x y = x*x + y*y
<span class="low"><code>Prelude></code></span> :type f
<code>f :: Num a => a -> a -> a</code>
</pre>

en: Uh? What is this strange type?
fr: Hein? Quel ce type étrange?

~~~
Num a => a -> a -> a
~~~

en: First, let's focus on the right part `a -> a -> a`.
en: To understand it, just look at a list of progressive examples: 
fr: Preumièrement, concentrons-nous sur la partie de droite: `a -> a -> a`.
fr: Pour le comprendre, regardez cette liste d'exemples progressifs:

en: --------------------------------------------------------------------------------------------------
en: The&nbsp;written&nbsp;type Its meaning
en: -------------------------- -----------------------------------------------------------------------
en: `Int`                      the type `Int`
en: 
en: `Int -> Int`               the type function from `Int` to `Int`
en: 
en: `Float -> Int`             the type function from `Float` to `Int`
en: 
en: `a -> Int`                 the type function from any type to `Int`
en: 
en: `a -> a`                   the type function from any type `a` to the same type `a`
en: 
en: `a -> a -> a`              the type function of two arguments of any type `a` to the same type `a`
en: --------------------------------------------------------------------------------------------------
fr: -------------------------------------------------------------------------------------------------------------------------------------- 
fr: Le&nbsp;type&nbsp;écrit    Son sens
fr: -------------------------- -----------------------------------------------------------------------------------------------------------
fr: `Int`                      Le type `Int`
fr: 
fr: `Int -> Int`               Le type de la fonction qui prend un `Int` et retourne un `Int`
fr: 
fr: `Float -> Int`             Le type de la fonction qui prend un `Float` et retourne un `Int`
fr: 
fr: `a -> Int`                 Le type de la fonction qui prend n'importe quel type de variable et retourne un `Int`
fr: 
fr: `a -> a`                   Le type de la fonction qui prend n'importe quel type `a` et retourne une variable du même type `a`
fr: 
fr: `a -> a -> a`              Le type de la fonction qui prend de arguments de n'importe quel type`a` et retourne une variable de type `a`
fr: --------------------------------------------------------------------------------------------------------------------------------------

en: In the type `a -> a -> a`, the letter `a` is a _type variable_. 
en: It means `f` is a function with two arguments and both arguments and the result have the same type.
en: The type variable `a` could take many different type values.
en: For example `Int`, `Integer`, `Float`...
fr: Dans le type `a -> a -> a`, la lettre `a` est une _variable de type_.
fr: Cela signifie que `f` est une fonction avec deux arguments et que les deux arguments et le résultat ont le même type.
fr: La variable de type `a` peut prendre de nombreuses valeurs différentes
fr: Par exemple `Int`, `Integer`, `Float`...

en: So instead of having a forced type like in `C` and having to declare a function
en: for `int`, `long`, `float`, `double`, etc., we declare only one function like
en: in a dynamically typed language.
fr: Donc à la place d'avoir un type forcé comme en `C` et de devoir déclarer une fonction
fr: pour `int`, `long`, `float`, `double`, etc., nous déclarons une seule fonction comme 
fr: dans un langage typé de façon dynamique.

en: This is sometimes called parametric polymorphism. It's also called having your
en: cake and eating it too.
fr: C'est parfois appelé le polymorphisme paramétrique. C'est aussi appelé avoir un
fr: gâteau et le manger.

en: Generally `a` can be any type, for example a `String` or an `Int`, but also
en: more complex types, like `Trees`, other functions, etc. But here our type is
en: prefixed with `Num a => `.
fr: Généralement `a` peut être de n'importe quel type, par exemple un `String` ou un `Int`, mais aussi
fr: des types plus complexes comme `Trees`, d'autres fonctions, etc. Mais ici notre type est
fr: préfixé par `Num a => `.

en: `Num` is a _type class_.
en: A type class can be understood as a set of types.
en: `Num` contains only types which behave like numbers.
en: More precisely, `Num` is class containing types which implement a specific list of functions, and in particular `(+)` and `(*)`.
fr: `Num` est une _classe de type_.
fr: Une classe de type peut être comprise comme un ensemble de types
fr: `Num` contient seulement les types qui se comportent comme des nombres.
fr: Plus précisement, `Num` est une classe qui contient des types qui implémentent une liste spécifique de fonctions,
fr: en particulier `(+)` et `(*)`.

en: Type classes are a very powerful language construct.
en: We can do some incredibly powerful stuff with this.
en: More on this later.
fr: Les classes de types sont une structure de langage très puissante.
fr: Nous pouvons faire des trucs incroyablement puissants avec.
fr: Nous verrons cela plus tard.

en: Finally, `Num a => a -> a -> a` means:
fr: Finalement, `Num a => a -> a -> a` signifie:

en: Let `a` be a type belonging to the `Num` type class.
en: This is a function from type `a` to (`a -> a`).
fr: soit `a` un type qui appartient à la classe `Num`.
fr: C'est une fonction qui prend une variable de type `a` et retourne une fonction de type `(a -> a)`

en: Yes, strange. 
en: In fact, in Haskell no function really has two arguments.
en: Instead all functions have only one argument.
en: But we will note that taking two arguments is equivalent to taking one argument and returning a function taking the second argument as a parameter.
fr: Oui, c'est étrange.
fr: En fait, en Haskell aucune fonction ne prend réellement deux arguments.
fr: Au lieu de cela toutes les fonctions n'ont qu'un argument unique.
fr: Mais nous retiendrons que prendre deux arguments est équivalent à n'en prendre qu'un et à retourner une fonction qui prend le second argument en paramètre.

en: More precisely `f 3 4` is equivalent to `(f 3) 4`. 
en: Note `f 3` is a function:
fr: Plus précisement `f 3 4` est équivalent à `(f 3) 4 `.
fr: Remarque: `f 3` est une fonction:

~~~
f :: Num a => a -> a -> a

g :: Num a => a -> a
g = f 3

g y ⇔ 3*3 + y*y
~~~

en: Another notation exists for functions. 
en: The lambda notation allows us to create functions without assigning them a name.
en: We call them anonymous functions.
en: We could also have written:
fr: Une autre notation existe pour les fonctions.
fr: La notation lambda nous autorise à créer des fonctions sans leur assigner un nom.
fr: On les appelle des fonctions anonymes.
fr: nous aurions donc pu écrire:

~~~
g = \y -> 3*3 + y*y
~~~

en: The `\` is used because it looks like `λ` and is ASCII.
fr: Le `\` esst utilisé car il ressemble à un `λ` et est un caractère ASCII.

en: If you are not used to functional programming your brain should be starting to heat up.
en: It is time to make a real application.
fr: Si vous n'êtes pas habitué à la programmation fonctionnelle, votre cerveau devrait commencer à chauffer
fr: Il est temps de faire une vraie application.
