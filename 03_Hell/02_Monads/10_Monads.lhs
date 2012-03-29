<h4 id="monads">Monads</h4>

Then now the word is spoken. `IO` is a _monad_.
More precisely, `IO` is an instance of the typeclass `Monad`.

To be an instance of the typeclass `Monad` you must provide the functions `(>>)`, `(>>=)`, `return` and `fail`.
You can safely ignore `fail` and as we saw earlier, `(>>)` can be derived from `(>>=)`.
Then to create new Monad, you only have to implement `return` and `(>>=)`.
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
 > - the type `m` must be of the form `* -> *`, which means a type taking another type as parameter.

Thus:

- Monad are not about effects!
- Monad are more about sequencing

There exists a lot of different type that are instance of `Monad`.
The easiest to describe is `Maybe`.
We saw earlier how to use `Maybe`.
Know if you have a sequence of `Maybe` value, you could use monad to manipulate them.
It is particularly useful to remove very deep `if..then..else..` constructions.

To be a usage monad, your function must obey some rule.
As these rule are undecidable to verify, you have to prove them before creating a new monad.
