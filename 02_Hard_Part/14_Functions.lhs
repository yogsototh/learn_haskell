In Haskell you can simplify function definition by η-reducing them.
For example, instead of writing:

<code class="haskell">
f x = (some expresion) x
</code>

you can simply write

<code class="haskell">
f = some expression
</code>

We use this method to remove the `l`:

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
