begindiv(intro)

fr: Adapter son esprit à Haskell peut être difficile.
fr: Ce le fût pour moi.
fr: Dans cet article, j'essaye de fournir les informations qui m'ont manquées quand j'essayais d'apprendre Haskell.

en: Bend his mind to Haskell can be hard.
en: It was for me.
en: In this article I try to provide what I lacked when I started to learn Haskell.

fr: Apprendre Haskell ce n'est pas simplement apprendre un nouveau langage de programmation.
fr: Il faut y associer un tas de notions que je n'avais jamais vues avant.
fr: Beaucoup de concepts servent aussi dans la programmation de langages plus communs.

en: As Boromir would say: "One does not simply learn Haskell".
en: To learn Haskell you'll need to learn far more than just a new language.
en: Haskell use a lot of concepts I've never heard about before.
en: But many will be useful for programming even in other languages.

fr: Cet article sera certainement difficile à suivre.
fr: Mais c'est voulu.
fr: Il n'y a pas de raccourci pour apprendre Haskell.
fr: C'est difficile.
fr: Mais je pense que c'est une bonne chose.

en: The article will certainly be hard to follow.
en: This is done on purpose.
en: There is no shortcut to learn Haskell.
en: It is hard and challenging. 
en: But I believe it is a good thing.

en: This article can be seen as a very dense introduction to Haskell.
en: The conventional way to learn Haskell is to read two books. 
en: First ["Learn You a Haskell"](http://learnyouahaskell.com) and just after ["Real World Haskell"](http://www.realworldhaskell.org).
en: I also believe this is the right way to go.
en: But Haskell is very hard to learn by skimming these books.
en: You'll have to read them in detail.
en: This is why I believe such an article while difficult to read can be a very good introduction.
en: Furthermore, I believe I missed such an article while learning Haskell. 

fr: Cet article peut être vu comme une introduction très dense d'Haskell.
fr: La manière conventionnelle d'apprendre Haskell est de lire deux livres.
fr: En premier ["Learn You a Haskell"](http://learnyouahaskell.com) 
fr: et ensuite ["Real World Haskell"](http://www.realworldhaskell.org).
fr: Je pense aussi que c'est la bonne manière de s'y prendre.
fr: Mais apprendre même un tout petit peu d'Haskell est presque impossible sans se plonger réellement dans ces livres.
fr: C'est pourquoi je pense qu'un article plus court comme celui-ci peut avoir son utilité.
fr: De plus, un tel article m'a manqué au début de mon apprentissage.

fr: Pour les francophones ; je suis désolé. 
fr: Je n'ai pas eu le courage de tout retraduire en français.
fr: Sachez cependant que si vous êtes plusieurs à insister, je ferai certainement l'effort.

en: This actual (long) article contains five parts:
fr: Cet article contient cinq parties :

en: - Introduction: a fast short example to show Haskell can be friendly.
en: - Basic Haskell: Haskell syntax, and some essential notions.
en: - Hard Difficulty Part:
en:     - Functional style; an example from imperative to functional style
en:     - Types; types and a standard binary tree example
en:     - Infinite Structure; manipulate an infinite binary tree!
en: - Hell Difficulty Part:
en:     - Deal with IO; A very minimal example
en:     - IO trick explained; the hidden detail I lacked to understand IO
en:     - Monads; incredible how we can generalize
en: - Appendix:
en:     - More on infinite tree; a more math oriented discussion about infinite trees

fr: - Introduction : un exemple rapide pour montrer qu'Haskell peut être facile.
fr: - Les bases d'Haskell : La syntaxe et des notions essentielles
fr: - Partie difficile : 
fr:     - Style fonctionnel : un exemple progressif, du style impératif au style fonctionnel ;
fr:     - Types : la syntaxe et un exemple d'arbre binaire ;
fr:     - Structure infinie : manipulons un arbre infini !
fr: - Partie de difficulté infernale :
fr:     - Utiliser les IO : un exemple très minimal ;
fr:     - Le truc des IO révélé : les détails cachés d'IO qui m'ont manqués
fr:     - Les monades : incroyable à quel point on peut généraliser
fr: - Appendice :
fr:     - Revenons sur les arbres infinis : une discussion plus mathématique sur la manipulation d'arbres infinis.

en:  > Note: Each time you'll see a separator with a filename ending in `.lhs`, you could click the filename to get this file. If you save the file as `filename.lhs`, you can run it with 
en:  > <pre>
en:  > runhaskell filename.lhs
en:  > </pre>
en:  >
en:  > Some might not work, but most will.
en:  > You should see a link just below.

fr:  > Note: Chaque fois que vous voyez un séparateur avec un nom de fichier se terminant par `lhs`, vous pouvez cliquer sur le nom de fichier et télécharger le fichier. 
fr:  > Si vous sauvegarez le fichier sour le nom `filename.lhs`, vous pouvez l'exécuter avec :
fr:  > <pre>
fr:  > runhaskell filename.lhs
fr:  > </pre>
fr:  >
fr:  > Certain ne marcheront pas, mais la majorité vous donneront un résultat.
fr:  > Vous devriez voir un lien juste en dessous.

enddiv
