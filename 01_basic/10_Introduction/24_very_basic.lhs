If we force our function to work with different type, it will fail:

> f :: Num a => a -> a -> a
> f x y = x*x + y*y
>
> x :: Int
> x = 3
> y :: Float
> y = 2.4
> main = print (f x y) -- won't work because type x ‡ type y

The comiler complains. 
The two parameter must have the same type.

If you believe it is a bad idea, and the compiler should make the transformation 
from a type to another for you, you should really watch this great (and funny) video:
[WAT](https://www.destroyallsoftware.com/talks/wat)
