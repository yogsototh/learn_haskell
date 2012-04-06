<h3 id="infinite-structures">Infinite Structures</h3>

<%= blogimage("escher_infinite_lizards.jpg","Escher") %>

It is often stated that Haskell is _lazy_.

In fact, if you are a bit pedantic, you should state that [Haskell is _non-strict_](http://www.haskell.org/haskellwiki/Lazy_vs._non-strict).
Laziness is just a common implementation for non-strict languages.

Then what does not-strict means? From the Haskell wiki:

 > Reduction (the mathematical term for evaluation) proceeds from the outside in.
 >
 > so if you have `(a+(b*c))` then you first reduce `+` first, then you reduce the inner `(b*c)`

For example in Haskell you can do:

> -- numbers = [1,2,..]
> numbers :: [Integer]
> numbers = 0:map (1+) numbers
> 
> take' n [] = []
> take' 0 l = []
> take' n (x:xs) = x:take' (n-1) xs
> 
> main = print $ take' 10 numbers

And it stops.

How?

Instead of trying to evaluate `numbers` entirely, 
it evaluates elements only when needed.

Also, note in Haskell there is a notation for infinite lists

~~~
[1..]   ⇔ [1,2,3,4...]
[1,3..] ⇔ [1,3,5,7,9,11...]
~~~

And most function will work with them. 
Also there exists the function `take` equivalent to our `take'`.
