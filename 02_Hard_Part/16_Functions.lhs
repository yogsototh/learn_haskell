Finally

<code class="haskell">
-- Version 8
import Data.List (foldl')
evenSum :: Integral a => [a] -> a
evenSum l = foldl' (+) 0 (filter even l)
</code>

`foldl'` isn't the easiest function to grasp.
If you are not used to it, you should study it a bit.

To help you understand what's going on here, let's look at a step by step evaluation:

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
The `(.)` function corresponds to mathematical composition.

<code class="haskell">
(f . g . h) x ⇔  f ( g (h x))
</code>

We can take advantage of this operator to η-reduce our function:

<code class="haskell">
-- Version 9
import Data.List (foldl')
evenSum :: Integral a => [a] -> a
evenSum = (foldl' (+) 0) . (filter even)
</code>

Also, we could rename some parts to make it clearer:

> -- Version 10 
> import Data.List (foldl')
> sum' :: (Num a) => [a] -> a
> sum' = foldl' (+) 0
> evenSum :: Integral a => [a] -> a
> evenSum = sum' . (filter even)
>  

It is time to discuss the direction our code has moved as we introduced more functional idioms.
What did we gain by using higher order functions?

At first, you might think the main difference is terseness. But in fact, it has
more to do with better thinking. Suppose we want to modify our function
slightly, for example, to get the sum of all even squares of elements of the
list.

~~~
[1,2,3,4] ▷ [1,4,9,16] ▷ [4,16] ▷ 20
~~~

Updating version 10 is extremely easy:

> squareEvenSum = sum' . (filter even) . (map (^2))
> squareEvenSum' = evenSum . (map (^2))
> squareEvenSum'' = sum' . (map (^2)) . (filter even)

We just had to add another "transformation function"[^0216].

[^0216]: Notice that `squareEvenSum''` is more efficient that the two other versions. The order of `(.)` is important.

~~~
map (^2) [1,2,3,4] ⇔ [1,4,9,16]
~~~

The `map` function simply applies a function to all the elements of a list.

We didn't have to modify anything _inside_ the function definition.
This makes the code more modular.
But in addition you can think more mathematically about your function.
You can also use your function interchangably with others, as needed.
That is, you can compose, map, fold, filter using your new function.

Modifying version 1 is left as an exercise to the reader ☺.

If you believe we have reached the end of generalization, then know you are very wrong.
For example, there is a way to not only use this function on lists but on any recursive type.
If you want to know how, I suggest you to read this quite fun article: [Functional Programming with Bananas, Lenses, Envelopes and Barbed Wire by Meijer, Fokkinga and Paterson](http://eprints.eemcs.utwente.nl/7281/0
1/db-utwente-40501F46.pdf).

This example should show you how great pure functional programming is.
Unfortunately, using pure functional programming isn't well suited to all usages.
Or at least such a language hasn't been found yet.

One of the great powers of Haskell is the ability to create DSLs
(Domain Specific Language)
making it easy to change the programming paradigm.

In fact, Haskell is also great when you want to write imperative style
programming. Understanding this was really hard for me to grasp when first
learning Haskell. A lot of effort tends to go into explaining the superiority
of the functional approach. Then when you start using an imperative style with
Haskell, it can be hard to understand when and how to use it.

But before talking about this Haskell super-power, we must talk about another
essential aspect of Haskell: _Types_.

<div style="display:none">

> main = print $ evenSum [1..10]

</div>
