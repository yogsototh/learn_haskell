<h3 id="monads">Monads</h3>

Then now the word is spoken. `IO` is a _monad_.
More precisely, `IO` is an instance of the typeclass `Monad`.

To be an instance of the typeclass `Monad` you must provide the functions `(>>)`, `(>>=)`, `return` and `fail`.
You can safely ignore `fail` and as we saw earlier, `(>>)` can be generally derived from `(>>=)`.
Then to create new Monad, you'll generally need to implement only `return` and `(>>=)`.
Here is how the typeclass `Monad` is declared (mostly):

> class Monad m  where
>   (>>=) :: m a -> (a -> m b) -> m b
>   return :: a -> m a
>
>   (>>) :: m a -> m b -> m b
>   action1 >> action2 = action1 >>= \_ -> action2
>
>   fail :: String -> m a
>   fail = error

 > Remarks:
 > 
 > - the keyword `class` is also a false friend. 
 >   An Haskell class is far more closer to an interface in Java.
 > - the type `m` must be a type that take an argument. 
 >   for example `IO a`, but also `Maybe a`, `[a]`, etc...


 > **Important remarks**:
 > 
 > - Monad are not about effects!
 > - Monad are more about sequencing

There exists a lot of different type that are instance of `Monad`.
The easiest to describe is `Maybe`.
We saw earlier how to use `Maybe`.
Know if you have a sequence of `Maybe` value, you could use monad to manipulate them.
It is particularly useful to remove very deep `if..then..else..` constructions.

To be a usage monad, your function must obey some rule.
As these rule are undecidable to verify, you have to prove them before creating a new monad.
Here are the rules:

<code class="haskell">
return a >>= k  ==  k a
m >>= return  ==  m
m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
</code>
