en: But just before that, we should verify the type system works as expected:
fr: Mais juste avant cela, nous devrions vérifier que le système de type marche comme nous le supposons:

> f :: Num a => a -> a -> a
> f x y = x*x + y*y
>
> main = print (f 3 2.4)

en: It works, because, `3` is a valid representation both for Fractional numbers like Float and for Integer.
en: As `2.4` is a Fractional number, `3` is then interpreted as being also a Fractional number.
fr: Cela fonctionne, car `3` est une représentation valide autant pour les nombres fractionnaires comme Float que pour les entiers.
fr: Comme `2.4` est un nombre fractionnaire, `3` est interprété comme une autre nombre fractionnaire
