Finaly

> -- Version 8
> import Data.List
> evenSum :: Integral a => [a] -> a
> evenSum l = foldl' (+) 0 (filter even l)

`foldl'` isn't the easiest function to intuit.
If you are not used to it, you should exercise a bit.

To help you understand what's going on here, a step by step evaluation:

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


Another useful higher order function is `(.)`.
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

What power did we gain by using `foldl'`?

At first, you can say it is terseness.
But in fact, it has more to do with better thinking.
Suppose we want to modify slightly our function.
We want to get the sum of all even square of element of the list.

~~~
[1,2,3,4] ▷ [1,4,9,16] ▷ [4,16] ▷ 20
~~~

Update the version 10 is extremely easy:

> squareEvenSum = sum . (filter even) . (map (^2))
> squareEvenSum' = evenSum . (map (^2))
> squareEvenSum'' = sum . (map (^2)) . (filter even)

We just had to add another "transformation function".

~~~
map (^2) [1,2,3,4] ⇔ [1,4,9,16]
~~~

The `map` function simply apply a function to all element of a list.

We didn't had to modify _inside_ the function definition.
It feels more modular.
But there is also you can think more mathematically about your function.
You could then use your function as any other one.
You could compose, map, fold, filter using your new function.

To modify version 1 is left as an exercise to the reader ☺.

If you believe we reached the end of generalization, then know you are very wrong.
For example, there is a way to not only use this function on list but on any recursive type.
If you want to know how, I suggest you to read this quite fun article: [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire by Meijer, Fokkinga and Paterson](http://eprints.eemcs.utwente.nl/7281/0
1/db-utwente-40501F46.pdf).

This example should show you how pure functional programming is
great. Unfortunately, using pure functional programming isn't well
suited for all usages.
Or at least it isn't found yet.

One of the great power of Haskell, is the ability to create DSL 
(Domain Specific Language)
making it easy to change the programming paradigm.

In fact, Haskell is also great when you want to write imperative style programming.
Understanding this was really hard for me when learning Haskell.
A lot of effort is provided to explain you how much functional approach is superior. 
Then when you attack the imperative style of Haskell, it is hard to understand why and how.

But before talking about this Haskell super-power, we must talk about another
essential aspect of Haskell: _Types_.
