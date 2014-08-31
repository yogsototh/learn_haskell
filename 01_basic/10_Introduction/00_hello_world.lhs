en: <h2 id="introduction">Introduction</h2>
en: 
en: <h3 id="install">Install</h3>
en: 
en: blogimage("Haskell-logo.png", "Haskell logo")
en: 
en: - [Haskell Platform](http://www.haskell.org/platform) is the standard way to install Haskell.
en: 
en: Tools:
en: 
en: - `ghc`: Compiler similar to gcc for `C`.
en: - `ghci`: Interactive Haskell (REPL)
en: - `runhaskell`: Execute a program without compiling it. Convenient but very slow compared to compiled programs.
en: 
en: <h3 id="don-t-be-afraid">Don't be afraid</h3>
en: 
en: blogimage("munch_TheScream.jpg","The Scream")
en: 
en: Many books/articles about Haskell start by introducing some esoteric formula (quick sort, Fibonacci, etc...).
en: I will do the exact opposite.
en: At first I won't show you any Haskell super power.
en: I will start with similarities between Haskell and other programming languages.
en: Let's jump to the mandatory "Hello World".
en: 
en: > main = putStrLn "Hello World!"
en: 
en: To run it, you can save this code in a `hello.hs` and:
en: 
en: <code class="zsh">
en: ~ runhaskell ./hello.hs
en: Hello World!
en: </code>
en: 
en: You could also download the literate Haskell source.
en: You should see a link just above the introduction title.
en: Download this file as `00_hello_world.lhs` and:
en: 
en: <code class="zsh">
en: ~ runhaskell 00_hello_world.lhs
en: Hello World!
en: </code>
 
fr: <h2 id="introduction">Introduction</h2>
fr: 
fr: <h3 id="install">Installation</h3>
fr: 
fr: blogimage("Haskell-logo.png", "Haskell logo")
fr: 
fr: - [Haskell Platform](http://www.haskell.org/platform) est la principale façon d'installer Haskell.
fr: 
fr: Outils:
fr: 
fr: - `ghc`: Compilateur similaire à gcc pour le langage `C`.
fr: - `ghci`: Ligne de commande interactive pour Haskell
fr: - `runhaskell`: Exécute un programme sans le compiler. Pratique mais très lent comparé au programmmes compilés.
fr: 
fr: <h3 id="don-t-be-afraid">Ne soyez pas effrayés !</h3>
fr: 
fr: blogimage("munch_TheScream.jpg","The Scream")
fr: 
fr: Beaucoup de livres/articles sur Haskell commencent en introduisant des formules esotériques (Telles que le tri rapides, la suite de Fibonacci, etc...).
fr: Je ferais l'exact opposé.
fr: En premier lieu je ne vous montrerais pas les super-pouvoirs de Haskell.
fr: Je vais commencer par les similarités entre Haskell et les autres langages de programmations.
fr: Débutons par l'indispensable "Hello World!".
fr: 
fr: > main = putStrLn "Hello World!"
fr: 
fr: Pour exécuter ce code, vous pouvez le sauvegarder dans un fichier `hello.hs` et tapez:
fr: 
fr: <code class="zsh">
fr: ~ runhaskell ./hello.hs
fr: Hello World!
fr: </code>
fr: Dans la ligne de commande.
fr:  
fr: Vous pouvez également télécharger la source Haskell litéralle.
fr: Vous devriez voir un lien juste au dessus du titre de l'introduction.
fr: Télécharger ce fichier sous le nom `00_hello_world.lhs` et tapez:
fr: 
fr: <code class="zsh">
fr: ~ runhaskell 00_hello_world.lhs
fr: Hello World!
fr: </code>