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

Update version 10 is extremely easy:

> squareEvenSum = sum . (filter even) . (map (^2))

We simply had to add another "transformation function".

~~~
map (^2) [1,2,3,4] ⇔ [1,4,9,16]
~~~

To modify version 1 is left as an exercise to the reader.
