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
Use value instead of general parameter name[^021301].

[^021301]: For the brave, a more complete explanation of pattern matching can be found [here](http://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/patterns.html).

Instead of saying: `foo l = if l == [] then <x> else <y>`
You simply state:  

<code class="haskell">
foo [] =  <x>
foo l  =  <y>
</code>

But pattern matching go even further. 
It is also able to inspect inside data. 
We can replace

<code class="haskell">
foo l =  let x  = head l 
             xs = tail l
         in if even x 
             then foo (n+x) xs
             else foo n xs
</code>

by

<code class="haskell">
foo (x:xs) = if even x 
                 then foo (n+x) xs
                 else foo n xs
</code>

This is a very useful feature.
It makes our code both terser and easier to read.

<div style="display:none">

> main = print $ evenSum [1..10]

</div>
