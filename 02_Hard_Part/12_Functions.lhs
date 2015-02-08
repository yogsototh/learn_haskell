en: Next, we can use sub functions using `where` or `let`.
fr: Ensuite, nous pouvons utiliser des sous-fonctions grâce à `where` et `let`.
en: This way our `accumSum` function won't pollute the namespace of our module.
fr: Ansi, notre fonction `accumSum` ne polluera pas le _namespace_ de notre module

> -- Version 2
> evenSum :: Integral a => [a] -> a
> 
> evenSum l = accumSum 0 l
>     where accumSum n l = 
>             if l == []
>                 then n
>                 else let x = head l 
>                          xs = tail l 
>                      in if even x
>                             then accumSum (n+x) xs
>                             else accumSum n xs

<div style="display:none">

> main = print $ evenSum [1..10]

</div>
