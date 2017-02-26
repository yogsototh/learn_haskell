fr: <h3 id="monads">Les monades</h3>
en: <h3 id="monads">Monads</h3>

blogimage("dali_reve.jpg","Dali, reve. It represents a weapon out of the mouth of a tiger, itself out of the mouth of another tiger, itself out of the mouth of a fish itself out of a grenade. I could have choosen a picture of the Human centipede as it is a very good representation of what a monad really is. But just to think about it, I find this disgusting and that wasn't the purpose of this document.")

fr: Maintenant le secret peut être dévoilé : `IO` est une _monade_.
en: Now the secret can be revealed: `IO` is a _monad_.
fr: Être une monade signifie que vous avez accès à du sucre syntaxique avec la notation `do`.
en: Being a monad means you have access to some syntactical sugar with the `do` notation.
fr: Mais principalement, vous avez accès à un motif de codage qui tempérera le flux de votre code.
en: But mainly, you have access to a coding pattern which will ease the flow of your code.

fr:  > **Remarques importantes** :
en:  > **Important remarks**:
 >
fr:  > - Le monades n'ont pas forcément quoi que ce soit à voir avec les effets de bord !
en:  > - Monad are not necessarily about effects!
fr:  >   Il y a beaucoup de monades _pures_.
en:  >   There are a lot of _pure_ monads.
fr:  > - Les monades concernent plus le séquençage.
en:  > - Monad are more about sequencing

fr: En Haskell, `Monad` est une classe de type.
en: In Haskell, `Monad` is a type class.
fr: Pour être une instance d'une classe de type, vous devez fournir les fonctions `(>>=)` et `return`.
en: To be an instance of this type class, you must provide the functions `(>>=)` and `return`.
fr: La fonction `(>>)` est dérivée de `(>>=)`.
en: The function `(>>)` is derived from `(>>=)`.
fr: Voici commment la classe de type `Monad` est déclarée (grosso modo) :
en: Here is how the type class `Monad` is declared (basically):

<code class="haskell">
class Monad m  where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

  (>>) :: m a -> m b -> m b
  f >> g = f >>= \_ -> g

fr:   -- Vous pouvez ignorer cette fonction généralement,
en:   -- You should generally safely ignore this function
fr:   -- je crois qu'elle existe pour des raisons historiques
en:   -- which I believe exists for historical reasons
  fail :: String -> m a
  fail = error
</code>


fr:  > Remarques :
en:  > Remarks:
 >
fr:  > - le mot-clé `class` n'est pas votre ami.
en:  > - the keyword `class` is not your friend.
fr:  >   Une classe en Haskell _n'est pas_ du même genre que celle des langages orientés-objet.
en:  >   A Haskell class is _not_ a class of the kind you will find in object-oriented programming.
fr:  >   Elles ont beaucoup de similarités avec les interfaces de Java.
en:  >   A Haskell class has a lot of similarities with Java interfaces.
fr:  >   Un meilleur mot aurait été `typeClass`, ce qui signifierait un ensemble de types. 
en:  >   A better word would have been `typeclass`, since that means a set of types.
fr:  >   Pour qu'un type appartienne à une classe, toutes les fonctions de cette classe doivent être fournies pour ce type.
en:  >   For a type to belong to a class, all functions of the class must be provided for this type.
fr:  > - Dans cet exemple particulier de classe de type, le type `m` doit être un type qui prend un argument.
en:  > - In this particular example of type class, the type `m` must be a type that takes an argument.
fr:  >   par exemple `IO a`, mais aussi `Maybe a`, `[a]`, etc...
en:  >   for example `IO a`, but also `Maybe a`, `[a]`, etc...
fr:  > - Pour être une monade utile, votre fonction doit obéir à quelques règles.
en:  > - To be a useful monad, your function must obey some rules.
fr:  >   Si votre construction n'obéit pas à ces règles, des choses étranges pourraient se produire :
en:  >   If your construction does not obey these rules strange things might happens:
 
 >   ~~~
 >   return a >>= k  ==  k a
 >   m >>= return  ==  m
 >   m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
 >   ~~~

fr: <h4 id="maybe-monad">Maybe est une monade</h4>
en: <h4 id="maybe-monad">Maybe is a monad</h4>

fr: Il y a beaucoup de types différents qui sont des instances de `Monad`.
en: There are a lot of different types that are instances of `Monad`.
fr: L'un des plus faciles à décrire est `Maybe`.
en: One of the easiest to describe is `Maybe`.
fr: Si vous avez une séquence de valeurs `Maybe`, vous pouvez utiliser les monades pour les manipuler.
en: If you have a sequence of `Maybe` values, you can use monads to manipulate them.
fr: C'est particulièrement utile pour enlever des constructions `if..then..else..` trop nombreuses.
en: It is particularly useful to remove very deep `if..then..else..` constructions.

fr: Imaginez une opération bancaire complexe. Vous êtes éligible pour gagner 700€ seulement si
en: Imagine a complex bank operation. You are eligible to gain about 700€ only
fr: vous pouvez effectuer une liste d'opérations sans tomber en dessous de zéro.
en: if you can afford to follow a list of operations without your balance dipping below zero.

> deposit  value account = account + value
> withdraw value account = account - value
>
> eligible :: (Num a,Ord a) => a -> Bool
> eligible account =
>   let account1 = deposit 100 account in
>     if (account1 < 0)
>     then False
>     else
>       let account2 = withdraw 200 account1 in
>       if (account2 < 0)
>       then False
>       else
>         let account3 = deposit 100 account2 in
>         if (account3 < 0)
>         then False
>         else
>           let account4 = withdraw 300 account3 in
>           if (account4 < 0)
>           then False
>           else
>             let account5 = deposit 1000 account4 in
>             if (account5 < 0)
>             then False
>             else
>               True
>
> main = do
>   print $ eligible 300 -- True
>   print $ eligible 299 -- False

