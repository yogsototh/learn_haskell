fr: <h4 id="the-list-monad">La monade List</h4>
en: <h4 id="the-list-monad">The list monad</h4>

blogimage("golconde.jpg","Golconde de Magritte")

fr: La monade List nouis aide à simuler des calculs non-détertiministe.
en: The list monad helps us to simulate non-deterministic computations.
fr: C'est parti:
en: Here we go:

> import Control.Monad (guard)
>
> allCases = [1..10]
>
> resolve :: [(Int,Int,Int)]
> resolve = do
>               x <- allCases
>               y <- allCases
>               z <- allCases
>               guard $ 4*x + 2*y < z
>               return (x,y,z)
>
> main = do
>   print resolve


fr: Ma. GIQUE. :
en: MA. GIC. :

~~~
[(1,1,7),(1,1,8),(1,1,9),(1,1,10),(1,2,9),(1,2,10)]
~~~

fr: Pour la monade List, il y a aussi un sucre syntaxique:
en: For the list monad, there is also this syntactic sugar:

>   print $ [ (x,y,z) | x <- allCases,
>                       y <- allCases,
>                       z <- allCases,
>                       4*x + 2*y < z ]

fr: Je ne listerais pas toutes les monades, mais il y en a beaucoup.
en: I won't list all the monads, but there are many of them.
fr: Utiliser les monades simplifie la manipulations de plusieurs notions dans les langages purs.
en: Using monads simplifies the manipulation of several notions in pure languages.
fr: Les monades sont très utiles, en particulier pour:
en: In particular, monads are very useful for:

fr: - L'E/S;
en: - IO,
fr: - calculs non-déterministes,
en: - non-deterministic computation,
fr: - générer des nombres pseudo-aléatoires,
en: - generating pseudo random numbers,
fr: - garder un état de configuration,
en: - keeping configuration state,
fr: - écrire un état,
en: - writing state,
- ...

fr: Si vous m'avez suivi jusqu'ici, alors vous avez terminé!
en: If you have followed me until here, then you've done it!
fr: Vous connaissez les monades[^03021301]!
en: You know monads[^03021301]!

fr: [^03021301]: Vous aurez quand même besoin de pratiquer un peu pour vous habituer à elles et pour comprendre quand les utiliser ou créer les vôtres. Mais vous avez déjà fait un grand pas dans cette direction.
en: [^03021301]: Well, you'll certainly need to practice a bit to get used to them and to understand when you can use them and create your own. But you already made a big step in this direction.
