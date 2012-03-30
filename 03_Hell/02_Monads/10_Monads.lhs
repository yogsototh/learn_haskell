<h3 id="monads">Monads</h3>

Then now the word is spoken. `IO` is a _monad_.
Being a monad means you have access to some syntactical sugar with the `do` notation.
But mainly, you have access to some coding pattern which will ease the flow of your code.

 > **Important remarks**:
 > 
 > - Monad are not about effects!
 > - Monad are more about sequencing

For the Haskell language `Monad` is a typeclass.
To be an instance of this typeclass, you must provide the functions `(>>=)` and `return`.
The function `(>>)` will be derived from `(>>=)`.
Here is how the typeclass `Monad` is declared (mostly):

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
 > - the keyword `class` is also a false friend. 
 >   An Haskell class is far more closer to an interface in Java.
 > - the type `m` must be a type that take an argument. 
 >   for example `IO a`, but also `Maybe a`, `[a]`, etc...
 > - To be a useful monad, your function must obey some rule.
 >   If your construction does not obey these rules strange thing might happens:
 >   <pre><code>
 >   return a >>= k  ==  k a
 >   m >>= return  ==  m
 >   m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
 >   </code></pre>

<h4 id="maybe-monad">Maybe is a monad</h4>

There exists a lot of different type that are instance of `Monad`.
The easiest to describe is `Maybe`.
Know if you have a sequence of `Maybe` value, you could use monad to manipulate them.
It is particularly useful to remove very deep `if..then..else..` constructions.

Imagine a complex bank operation. You are elligible to gain about 700€ only
if you can afford to follow a list of operation without being negative.

> deposit  value account = account + value
> withdraw value account = account - value
>
> elligible :: (Num a,Ord a) => a -> Bool
> elligible account = 
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
>   print $ elligible 300
>   print $ elligible 299


To be a usage monad, your function must obey some rule.
As these rule are undecidable to verify, you have to prove them before creating a new monad.
Here are the rules:

<code class="haskell">
return a >>= k  ==  k a
m >>= return  ==  m
m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
</code>
