But if we force our function to work with different type, it will fail:

> f :: Num a => a -> a -> a
> f x y = x*x + y*y
>
> x :: Int
> x = 3
> y :: Float
> y = 2.4
> main = print (f x y)

Yes the comiler complain. Because, `a` must be or `Int` or `Float`.
If you believe it is a bad idea, and the compiler should make the transformation from a type to another for you, you should really watch this great video:

[http://www.destroyallsoftware.com/talks/wat](https://www.destroyallsoftware.com/talks/wat)
