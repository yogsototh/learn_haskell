Next, we can use sub functions using `where` or `let`.
This way our `accumSum` function won't polute the global name space.

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

