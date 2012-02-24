Finaly

> -- Version 8
> import Data.List
> evenSum :: Integral a => [a] -> a
> evenSum l = foldl' (+) 0 (filter even l)

`foldl'` isn't the easiest function to intuit.
If you are not used to it, you should exercise a bit.

I would like to introduce another higher order function: `(.)`.
The `(.)` function correspond to the mathematical composition.

> (f . g . h) x ⇔  f ( g (h x))

We can take advantage of this operator to curry a bit more our function:

> -- Version 9
> import Data.List
> evenSum :: Integral a => [a] -> a
> evenSum = (foldl' (+) 0) . (filter even)

Also, there already exists a `sum` function.

> -- Version 10 
> import Data.List
> evenSum :: Integral a => [a] -> a
> evenSum = sum . (filter even)

Why is this last definition of evenSum superior to the first one?
Imagine we want not only to make the sum of even numbers of a list.
But now, we want to get the sum of all even square of element of the list.

~~~
[1,2,3,4] ~> [1,4,9,16] ~> [4,16] ~> 20
~~~

Update the version 10 is extremely easy:

> squareEvenSum = sum . (filter even) . (map (^2))

We simply had to add another "transformation function".

~~~
map (^2) [1,2,3,4] ⇔ [1,4,9,16]
~~~

To modify version 1 is left as an exercise to the reader.

If you believe we reached the end of generalization, then know you are very wrong. For example, there is a way to not only use this function on list but on any recursive type. If you want to know how, I suggest you to read this quite fun article: [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire by Meijer, Fokkinga and Paterson](http://eprints.eemcs.utwente.nl/7281/0
1/db-utwente-40501F46.pdf).

This example should show you how pure functional programming is
great. Unfortunately, using pure functional programming isn't well
suited for all usages. Or at least it isn't found yet.

One of the great power of Haskell, is the ability to create DSL 
(Domain Specific Language)
making it easy to change the programming paradigm.

In fact, Haskell is also great when you want to write imperative style
programming. Understand this was really hard for me when learning Haskell.
Because a lot of effort is provided to explain you how much functional
approach is superior. Than when you attack the imperative style of Haskell, it
is hard to understand why and how.

But before talking about this Haskell super-power, we must talk about another
essential aspect of Haskell; the _types_.
