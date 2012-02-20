Next, we can use pattern matching.

> -- Version 3
> evenSum l = accumSum 0 l
>     where 
>         accumSum n [] = n
>         accumSum n (x:xs) = 
>              if even x
>                 then accumSum (n+x) xs
>                 else accumSum n xs

What is pattern matching? 
Use value instead of general parameter name.

Instead of saying: `foo l = if l == [] then <x> else <y>`
You simply state:  

> foo [] =  <x>
> foo l  =  <y>

But pattern matching go even further. 
It is also able to inspect inside datas. 
We can replace

>  foo l =  let x  = head l 
>               xs = tail l
>           in if even x 
>               then foo (n+x) xs
>               else foo n xs

by

>  foo (x:xs) = if even x 
>                   then foo (n+x) xs
>                   else foo n xs

This is a very useful feature.
It makes our code both terse and easier to read.

