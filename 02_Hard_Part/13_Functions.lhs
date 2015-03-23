en: Next, we can use pattern matching.
fr: Puis on utilise le _pattern matching_

> -- Version 3
> evenSum l = accumSum 0 l
>     where
>         accumSum n [] = n
>         accumSum n (x:xs) =
>              if even x
>                 then accumSum (n+x) xs
>                 else accumSum n xs

en: What is pattern matching?
en: Use values instead of general parameter names[^021301].
fr: Qu'est ce que le _pattern matching_ ?
fr: Il s'agit d'utiliser des valeurs au lieu de noms de paramètres généraux.

en: [^021301]: For the brave, a more complete explanation of pattern matching can be found [here](http://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/patterns.html).
fr: [^021301]: Pour les plus courageux, une explication plus complète du _pattern matching_ peut être trouvée [ici](http://www.cs.auckland.ac.nz/references/haskell/haskell-intro-html/patterns.html) (_NdT: En anglais_)

en: Instead of saying: `foo l = if l == [] then <x> else <y>`
en: You simply state:
fr: Au lieu d'écrire: `foo l = if l == [] then <x> else <y>`
fr: Vous écrivez tout simplement :

<code class="haskell">
foo [] =  <x>
foo l  =  <y>
</code>

en: But pattern matching goes even further.
en: It is also able to inspect the inner data of a complex value.
en: We can replace
fr: Mais le _pattern matching_ peut aller encore plus loin.
fr: Il est également capable d'inspect les données internes d'un valeur complexe.
fr: Nous pouvons ainsi remplacer

<code class="haskell">
foo l =  let x  = head l
             xs = tail l
         in if even x
             then foo (n+x) xs
             else foo n xs
</code>

en: with
fr: par

<code class="haskell">
foo (x:xs) = if even x
                 then foo (n+x) xs
                 else foo n xs
</code>

en: This is a very useful feature.
en: It makes our code both terser and easier to read.
fr: C'est une caractéristique très utile.
fr: Notre code est ainsi plus concis et plus facile à lire.

<div style="display:none">

> main = print $ evenSum [1..10]

</div>
