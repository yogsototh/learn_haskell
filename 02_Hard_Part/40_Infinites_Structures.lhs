en: <h3 id="infinite-structures">Infinite Structures</h3>
fr: <h3 id="infinite-structures">Structures infinies</h3>

blogimage("escher_infinite_lizards.jpg","Escher")

en: It is often said that Haskell is _lazy_.
fr: On dit souvent que Haskell est _paresseux_.

en: In fact, if you are a bit pedantic, you should say that [Haskell is _non-strict_](http://www.haskell.org/haskellwiki/Lazy_vs._non-strict).
en: Laziness is just a common implementation for non-strict languages.
fr: En fait, si vous êtes un petit peu pédant, vous devriez dire que [Haskell est _non-strict_](http://www.haskell.org/haskellwiki/Lazy_vs._non-strict) (_NDT: En anglais, pour changer_).
fr: La paresse est juste une implémentation commune aux langages non-stricts.

en: Then what does "not-strict" mean? From the Haskell wiki:
fr: Alors que signifie "non-strict"? D'après le wiki de Haskell :

en:  > Reduction (the mathematical term for evaluation) proceeds from the outside in.
en:  >
en:  > so if you have `(a+(b*c))` then you first reduce `+` first, then you reduce the inner `(b*c)`
fr:  > La réduction (terme mathématique pour "évaluation") procède depuis l'extérieur.
fr:  >
fr:  > Donc si vous avez `(a+(b*c))`, alors vous réduisez `+` d'abord, puis vous réduisez `(b*c)`

en: For example in Haskell you can do:
fr: Par exemple en Haskell vous pouvez faire :

> -- numbers = [1,2,..]
> numbers :: [Integer]
> numbers = 0:map (1+) numbers
>
> take' n [] = []
> take' 0 l = []
> take' n (x:xs) = x:take' (n-1) xs
>
> main = print $ take' 10 numbers

en: And it stops.
fr: Et ça s'arrête.

en: How?
fr: Comment ?

en: Instead of trying to evaluate `numbers` entirely,
en: it evaluates elements only when needed.
fr: Au lieu d'essayer d'évaluer `numbers` entièrement,
fr: Haskell évalue les éléments seulement lorsque c'est nécessaire.

en: Also, note in Haskell there is a notation for infinite lists
fr: Remarquez aussi qu'en Haskell, il y a une notation pour les listes infinies

~~~
[1..]   ⇔ [1,2,3,4...]
[1,3..] ⇔ [1,3,5,7,9,11...]
~~~

en: and most functions will work with them.
en: Also, there is a built-in function `take` which is equivalent to our `take'`.
fr: et que la majorité des fonctions fonctionnera avec ces listes.
fr: Il y a aussi une fonction `take` équivalente à notre `take'`.
