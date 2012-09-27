<h4 id="the-list-monad">The list monad</h4>

blogimage("golconde.jpg","Golconde de Magritte") 

The list monad helps us to simulate non deterministic computations.
Here we go:

> import Control.Monad (guard)
>
> allCases = [1..10]
>
> resolve :: [(Int,Int,Int)]
> resolve = do
>               x <- allCases
>               y <- allCases
>               z <- allCases
>               guard $ 4*x + 2*y < z
>               return (x,y,z)
> 
> main = do
>   print resolve


MA. GIC. :

~~~
[(1,1,7),(1,1,8),(1,1,9),(1,1,10),(1,2,9),(1,2,10)]
~~~

For the list monad, there is also a syntactical sugar:

>   print $ [ (x,y,z) | x <- allCases, 
>                       y <- allCases, 
>                       z <- allCases, 
>                       4*x + 2*y < z ]

I won't list all the monads, but there are many monads.
Using monads simplifies the manipulation of several notions in pure languages.
In particular, monad are very useful for: 

- IO,
- non deterministic computation,
- generating pseudo random numbers, 
- keeping configuration state, 
- writing state,
- ...

If you have followed me until here, then you've done it! 
You know monads[^03021301]!

[^03021301]: Well, you'll certainly need to practice a bit to get used to them
and to understand when you can use them and create your own. But you already
made a big step in this direction.
