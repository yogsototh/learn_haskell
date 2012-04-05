We also can currify a bit our definition by removing the `l`:

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
