en: If we force our function to work with different types, it will fail:
fr: Si nous forçons notre fonction à travailler avec des types différents, le test échouera:

> f :: Num a => a -> a -> a
> f x y = x*x + y*y
>
> x :: Int
> x = 3
> y :: Float
> y = 2.4
> -- won't work because type x ≠ type y
> main = print (f x y)

en: The compiler complains. 
en: The two parameters must have the same type.
fr: Le compilateur se plaint.
fr: Les deux paramètres doivent avoir le même type.

en: If you believe that this is a bad idea, and that the compiler should make the transformation 
en: from one type to another for you, you should really watch this great (and funny) video:
en: [WAT](https://www.destroyallsoftware.com/talks/wat)
fr: Si vous pensez que c'est une mauvaise idée et que le compilateur devrait faire la transformation
fr: depuis un type à un autr pour vous, vous devriez vraiment regarder cette vidéo géniale (et amusante):
fr: [WAT](https://www.destroyallsoftware.com/talks/wat) (_NDT: En Anglais_)
