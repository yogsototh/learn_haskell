Now try

> f :: Int -> Int -> Int
> f x y = x*x + y*y
>
> main = print (f 2.3 4.2)

You get this error:

~~~
21_very_basic.lhs:6:23:
    No instance for (Fractional Int)
      arising from the literal `4.2'
    Possible fix: add an instance declaration for (Fractional Int)
    In the second argument of `f', namely `4.2'
    In the first argument of `print', namely `(f 2.3 4.2)'
    In the expression: print (f 2.3 4.2)
~~~

The problem: `2.3` isn't an Int.
