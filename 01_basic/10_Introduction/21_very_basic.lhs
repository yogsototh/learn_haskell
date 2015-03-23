en: Now try
fr: Maintenant essayez

> f :: Int -> Int -> Int
> f x y = x*x + y*y
>
> main = print (f 2.3 4.2)

en: You should get this error:
fr: Vous devriez avoir cette erreur:

~~~
21_very_basic.lhs:6:23:
    No instance for (Fractional Int)
      arising from the literal `4.2'
    Possible fix: add an instance declaration for (Fractional Int)
    In the second argument of `f', namely `4.2'
    In the first argument of `print', namely `(f 2.3 4.2)'
    In the expression: print (f 2.3 4.2)
~~~

en: The problem: `4.2` isn't an Int.
fr: Le problème est que `4.2` n'est pas de type `Int` (_NDT: Il n'est pas un entier_)
