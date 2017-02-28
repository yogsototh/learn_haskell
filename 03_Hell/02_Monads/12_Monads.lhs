fr: Pas mauvais, mais nous pouvons faire encore mieux :
en: Not bad, but we can make it even better:

> deposit :: (Num a) => a -> a -> Maybe a
> deposit value account = Just (account + value)
> 
> withdraw :: (Num a,Ord a) => a -> a -> Maybe a
> withdraw value account = if (account < value) 
>                          then Nothing 
>                          else Just (account - value)
> 
> eligible :: (Num a, Ord a) => a -> Maybe Bool
> eligible account =
>   deposit 100 account >>=
>   withdraw 200 >>=
>   deposit 100  >>=
>   withdraw 300 >>=
>   deposit 1000 >>
>   return True
> 
> main = do
>   print $ eligible 300 -- Just True
>   print $ eligible 299 -- Nothing

fr: Nous avons prouvé que les monades sont un bon moyen de rendre notre code plus élégant.
en: We have proven that Monads are a good way to make our code more elegant.
fr: Remarquez que cette idée d'organisation de code, en particulier pour `Maybe`, peut être utilisée
en: Note this idea of code organization, in particular for `Maybe` can be used
fr: dans la plupart des langages impératifs.
en: in most imperative languages.
fr: En fait, c'est le type de construction que nous faisons naturellement.
en: In fact, this is the kind of construction we make naturally.

fr:  > Une remarque importante :
en:  > An important remark:
 > 
fr:  > Le premier élement de la séquence qui sera évalué comme `Nothing` stoppera
en:  > The first element in the sequence being evaluated to `Nothing` will stop
fr:  > l'évaluation.
en:  > the complete evaluation. 
fr:  > Cela signifie que vous n'exécutez pas toutes les lignes.
en:  > This means you don't execute all lines.
fr:  > Cela découle du caractère paresseux de Haskell.
en:  > You get this for free, thanks to laziness.

fr: Vous pourriez aussi revoir ces exemples avec la définition de `(>>=)` pour `Maybe`
en: You could also replay these example with the definition of `(>>=)` for `Maybe`
fr: en tête :
en: in mind:

<code class="haskell">
instance Monad Maybe where
    (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
    Nothing  >>= _  = Nothing
    (Just x) >>= f  = f x

    return x = Just x
</code>


fr: La monade `Maybe` a prouvé par un simple exemple qu'elle est utile.
en: The `Maybe` monad proved to be useful while being a very simple example.
fr: Nous avons vu l'utilité de la monade `IO`.
en: We saw the utility of the `IO` monad.
fr: Mais maintenant, voici un exemple encore plus cool : les listes.
en: But now for a cooler example, lists.
