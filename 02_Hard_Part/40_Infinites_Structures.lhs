<h4>Infinite Structures</h4>

It is often stated that Haskell is _lazy_.

In fact, If you are a bit pedantic, you should state that [Haskell is not lazy but _non-strict_](http://www.haskell.org/haskellwiki/Lazy_vs._non-strict).
Lazyness is just a common implementation for non-strict languages.

Then what does not-strict means (from the haskell wiki):

 > Reduction (the mathematical term for evaluation) proceeds from the outside in.
 >
 > so if you have `(a+(b*c))` then you first reduce `+` first, then you reduce the inner `(b*c)`

For example in Haskell you can do:

> numbers :: [Integer]
> numbers = 0:map (1+) numbers
> 
> take' n [] = []
> take' 0 l = []
> take' n (x:xs) = x:take (n-1) xs
> 
> main = print $ take' 10 numbers

And it stops.

How?

Instead of trying to evaluate `numbers` entirely, 
it evaluates only needed elements.
