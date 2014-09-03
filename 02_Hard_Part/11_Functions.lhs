en: The first Haskell solution.
en: The function `evenSum` returns the sum of all even numbers in a list:
fr: La première solution en Haskell.
fr: La fonction `evenSum` retourne la somme de tous les nombres pairs d'une liste:

> -- Version 1
> evenSum :: [Integer] -> Integer
> 
> evenSum l = accumSum 0 l
> 
> accumSum n l = if l == []
>                   then n
>                   else let x = head l 
>                            xs = tail l 
>                        in if even x
>                               then accumSum (n+x) xs
>                               else accumSum n xs

en: To test a function you can use `ghci`:
fr: Pour tester une fonction nous pouvons utiliser `ghci`:

<pre>
% ghci
<span class="low">GHCi, version 7.0.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude&gt;</span> :load 11_Functions.lhs 
<span class="low">[1 of 1] Compiling Main             ( 11_Functions.lhs, interpreted )
Ok, modules loaded: Main.
*Main&gt;</span> evenSum [1..5]
6
</pre>

en: Here is an example of execution[^2]: 
fr: Voici un exemple d'exécution[^2]:

en: [^2]: I know I'm cheating. But I will talk about non-strictness later.
fr: [^2]: Je sais que je triche. Mais je parlerais de la non-rigueur plus tard. <!-- IL FAUDRA TROUVER UNE AUTRE TRADUCTION POUR NON-STRICTNESS -->

<pre>
*Main> evenSum [1..5]
accumSum 0 [1,2,3,4,5]
en: <span class="yellow">1 is odd</span>
fr: <span class="yellow">1 est impair</span>
accumSum 0 [2,3,4,5]
en: <span class="yellow">2 is even</span>
fr: <span class="yellow">2 est pair</span>
accumSum (0+2) [3,4,5]
en: <span class="yellow">3 is odd</span>
fr: <span class="yellow">3 est impair</span>
accumSum (0+2) [4,5]
en: <span class="yellow">2 is even</span>
fr: <span class="yellow">4 est pair</span>
accumSum (0+2+4) [5]
en: <span class="yellow">5 is odd</span>
fr: <span class="yellow">5 est impair</span>
accumSum (0+2+4) []
<span class="yellow">l == []</span>
0+2+4
0+6
6
</pre>

en: Coming from an imperative language all should seem right.
en: In fact, many things can be improved here.
en: First, we can generalize the type.
fr: En venant d'un langage impératif, tout devrait vous sembler juste.
fr: En fait, beaucoup de choses peuvent être améliorées ici.
fr: Tout d'abord, nous pouvons généraliser le type.

<code class="haskell">
evenSum :: Integral a => [a] -> a
</code>

<div style="display:none">

> main = do print $ evenSum [1..10]

</div>
