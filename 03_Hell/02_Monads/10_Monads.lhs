<h3 id="monads">Monads</h3>

blogimage("dali_reve.jpg","Dali, reve. It represents a weapon out of the mouth of a tiger, itself out of the mouth of another tiger, itself out of the mouth of a fish itself out of a grenade. I could have choosen a picture of the Human centipede as it is a very good representation of what a monad really is. But just to thing about it, I find this disgusting and that wasn't the purpose of this document.")

Now the secret can be revealed: `IO` is a _monad_.
Being a monad means you have access to some syntactical sugar with the `do` notation.
But mainly, you have access to a coding pattern which will ease the flow of your code.

 > **Important remarks**:
 >
 > - Monad are not necessarily about effects!
 >   There are a lot of _pure_ monads.
 > - Monad are more about sequencing

For the Haskell language `Monad` is a type class.
To be an instance of this type class, you must provide the functions `(>>=)` and `return`.
The function `(>>)` will be derived from `(>>=)`.
Here is how the type class `Monad` is declared (mostly):

<code class="haskell">
class Monad m  where
  (>>=) :: m a -> (a -> m b) -> m b
  return :: a -> m a

  (>>) :: m a -> m b -> m b
  f >> g = f >>= \_ -> g

  -- You should generally safely ignore this function
  -- which I believe exists for historical reason
  fail :: String -> m a
  fail = error
</code>


 > Remarks:
 >
 > - the keyword `class` is not your friend.
 >   A Haskell class is _not_ a class like in object model.
 >   A Haskell class has a lot of similarities with Java interfaces.
 >   A better word should have been `typeclass`.
 >   That means a set of types.
 >   For a type to belong to a class, all functions of the class must be provided for this type.
 > - In this particular example of type class, the type `m` must be a type that takes an argument.
 >   for example `IO a`, but also `Maybe a`, `[a]`, etc...
 > - To be a useful monad, your function must obey some rules.
 >   If your construction does not obey these rules strange things might happens:
 >
 >   ~~~
 >   return a >>= k  ==  k a
 >   m >>= return  ==  m
 >   m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
 >   ~~~

<h4 id="maybe-monad">Maybe is a monad</h4>

There are a lot of different types that are instance of `Monad`.
One of the easiest to describe is `Maybe`.
If you have a sequence of `Maybe` values, you can use monads to manipulate them.
It is particularly useful to remove very deep `if..then..else..` constructions.

Imagine a complex bank operation. You are eligible to gain about 700€ only
if you can afford to follow a list of operations without being negative.

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

