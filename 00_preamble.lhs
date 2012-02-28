begindiv(intro)

Bend his mind to Haskell can be hard.
It was for me.
In this article I will try to provide you what I lacked to learn Haskell.

Why should you care about learning Haskell?
You will learn far more than just a new language.

By learning Haskell you'll learn a lot of new concepts.
You will certainly learn how to make your code much better in most languages.

Haskell has an excelent balance between speed and high level.
Haskell is very fast compared to interpreted languages such as Pyton and Ruby.
But it is not its main advantage ; `C` and `C++` remains faster... _yet_.
For me, the best part of this language, is its ability to organize your code.

When you read Haskell, it is like reading a scientific paper.
Some part are very clear, but time to time, there are formulas and proofs.
During these parts, you'll have to focus a bit more and to read _slower_.

The part to learn are:

- the syntax
- functional approach
- purity and IO
- lazyness (use infinite structures!)
- Monads, Arrows, Monoid, Applicatives, etc...

This actual article contains three parts.

- Introduction: a fast dig inside some real Haskell example
- Basic Haskell: Haskell syntax, and some essential notions
- The real start: Explaining the hard parts; IO and Monads
    - Functional style; an example from imperative to functional
    - Purity and IO; how to use and how it is incredible
    - Monads; incredible how we can generalize

 > Note: Each time you'll see a separator with a filename ending in `.lhs`, you could click the filename to get this file. If you save the file as `filename.lhs`, you can run it with 
 > <pre>
 > runhaskell filename.lhs
 > </pre>
 > 
 > You should see a link just below the line.

enddiv
