## Introduction

### Install

- [Haskell Platform](http://www.haskell.org/platform) is the standard way to install Haskell.

Tools:

- `ghc`: Compiler similar to gcc for `C`.
- `ghci`: Interactive Haskell (REPL)
- `runhaskell`: Execute a program without compiling it. Convenient but very slow compared to compiled program.

### That was easy!

At first I won't show you any Haskell super power.
I will show you similarities between Haskell and other programming languages.
We'll just start with the obligatory "Hello World".

> main = print "Hello World!"

To run it, save this code in a `hello.hs` and:

<code class="zsh">
~ runhaskell ./hello.hs
Hello World!
</code>

You could also download the literate haskell source.
You should see a link just above the introduction title.
Download this file as `00_hello_world.lhs` and:

<code class="zsh">
~ runhaskell 00_hello_world.lhs
Hello World!
</code>
