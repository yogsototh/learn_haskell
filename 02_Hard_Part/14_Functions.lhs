en: In Haskell you can simplify function definitions by η-reducing them.
en: For example, instead of writing:
fr: Avec Haskell, nous pouvons simplifier les défitions des fonctions en les _η-réduisant_ .
fr: Par exemple, au lieu d'écrire:

<code class="haskell">
en: f x = (some expresion) x
fr: f x = (expression) x
</code>

en: you can simply write
fr: Nous pouvons écrire

<code class="haskell">
en: f = some expression
fr: f = expression
</code>

en: We use this method to remove the `l`:
fr: Utilisons cette méthode pour retirer le `l`:

> -- Version 4
> evenSum :: Integral a => [a] -> a
>
> evenSum = accumSum 0
>     where
>         accumSum n [] = n
>         accumSum n (x:xs) =
>              if even x
>                 then accumSum (n+x) xs
>                 else accumSum n xs

<div style="display:none">

> main = print $ evenSum [1..10]

</div>
