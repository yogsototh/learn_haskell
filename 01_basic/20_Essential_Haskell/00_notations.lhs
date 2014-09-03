en: <h2 id="essential-haskell">Essential Haskell</h2>
fr: <h2 id="essential-haskell">Notions essentielles</h2>

blogimage("kandinsky_gugg.jpg","Kandinsky Gugg")

en: I suggest that you skim this part.
en: Think of it as a reference.
en: Haskell has a lot of features.
en: A lot of information is missing here.
en: Come back here if the notation feels strange.
fr: Je vous suggère de seulement survoler cette partie
fr: Pensez-y seulement comme à une référence.
fr: Haskell a beaucoup de caractèristiques
fr: Il manque beaucoup d'informations ici.
fr: Revenz ici si la notation vous semble étrange.

en: I use the `⇔` symbol to state that two expression are equivalent.
en: It is a meta notation, `⇔` does not exists in Haskell.
en: I will also use `⇒` to show what the return value of an expression is.
fr: J'utilise le symbole `⇔` pour signifier que deux expressions sont équivalentes.
fr: C'est une notation extérieure, `⇔` n'existe pas en Haskell.
fr: Je vais aussi utiliser le symoble `⇒` quelle est la valeur que retourne une fonction.

<h3 id="notations">Notations</h3>

en: <h5 id="arithmetic">Arithmetic</h5>
fr: <h5 id="arithmetic">Arithmétique</h5>

~~~
3 + 2 * 6 / 3 ⇔ 3 + ((2*6)/3)
~~~

en: <h5 id="logic">Logic</h5>
fr: <h5 id="logic">Logique</h5>

~~~
True || False ⇒ True
True && False ⇒ False
True == False ⇒ False
en: True /= False ⇒ True  (/=) is the operator for different
fr: True /= False ⇒ True  (/=) est l'opérateur pour "différent de"
~~~

en: <h5 id="powers">Powers</h5>
fr: <h5 id="powers">Puissances</h5>

~~~
en: x^n     for n an integral (understand Int or Integer)
en: x**y    for y any kind of number (Float for example)
fr: x^n     pour n un entier (comprenez Int ou Integer)
fr: x**y    pour y tout type de nombre (Float par exemple)
~~~

en: `Integer` has no limit except the capacity of your machine:
fr: `Integer` n'a aucune limite à part la capacité de votre machine:

~~~
4^103
102844034832575377634685573909834406561420991602098741459288064
~~~

Yeah!
en: And also rational numbers FTW!
en: But you need to import the module `Data.Ratio`:
fr: Et aussi les nombres rationnels!
fr: Mais vous avez besoin d'importer le module `Data.Ratio`

~~~
$ ghci
....
Prelude> :m Data.Ratio
Data.Ratio> (11 % 15) * (5 % 3)
11 % 9
~~~

en: <h5 id="lists">Lists</h5>
fr: <h5 id="lists">Listes</h5>

~~~
en: []                      ⇔ empty list
en: [1,2,3]                 ⇔ List of integral
en: ["foo","bar","baz"]     ⇔ List of String
en: 1:[2,3]                 ⇔ [1,2,3], (:) prepend one element
en: 1:2:[]                  ⇔ [1,2]
en: [1,2] ++ [3,4]          ⇔ [1,2,3,4], (++) concatenate
en: [1,2,3] ++ ["foo"]      ⇔ ERROR String ≠ Integral
en: [1..4]                  ⇔ [1,2,3,4]
en: [1,3..10]               ⇔ [1,3,5,7,9]
en: [2,3,5,7,11..100]       ⇔ ERROR! I am not so smart!
en: [10,9..1]               ⇔ [10,9,8,7,6,5,4,3,2,1]
fr: []                      ⇔ liste vide
fr: [1,2,3]                 ⇔ Liste d'entiers
fr: ["foo","bar","baz"]     ⇔ Liste de chaînes de caractères
fr: 1:[2,3]                 ⇔ [1,2,3], (:) ajoute un élément au début
fr: 1:2:[]                  ⇔ [1,2]
fr: [1,2] ++ [3,4]          ⇔ [1,2,3,4], (++) concaténation de deux listes
fr: [1,2,3] ++ ["foo"]      ⇔ ERREUR String ≠ Integral
fr: [1..4]                  ⇔ [1,2,3,4]
fr: [1,3..10]               ⇔ [1,3,5,7,9]
fr: [2,3,5,7,11..100]       ⇔ ERREUR! Je ne suis pas si intelligent!
fr: [10,9..1]               ⇔ [10,9,8,7,6,5,4,3,2,1]
~~~

en: <h5 id="strings">Strings</h5>
fr: <h5 id="strings">Chaînes de caractères</h5>

en: In Haskell strings are list of `Char`.
fr: En Haskell les chaînes de caractères sont des listes de `Char`.

~~~
'a' :: Char
"a" :: [Char]
""  ⇔ []
"ab" ⇔ ['a','b'] ⇔  'a':"b" ⇔ 'a':['b'] ⇔ 'a':'b':[]
"abc" ⇔ "ab"++"c"
~~~

en:  > _Remark_:
en:  > In real code you shouldn't use list of char to represent text.
en:  > You should mostly use `Data.Text` instead.
en:  > If you want to represent a stream of ASCII char, you should use `Data.ByteString`.
fr:  > _Remarque_:
fr:  > Dans un vrai code vous n'utiliserez pas des listes de char pour représenter du texte.
fr:  > Vous utiliserez plus souvent `Data.Text` à la place.
fr:  > Si vous voulez représenter un chapelet de caractères ASCII, vous utiliserez `Data.ByteString`.

<h5 id="tuples">Tuples</h5>

en: The type of couple is `(a,b)`.
en: Elements in a tuple can have different types.
fr: Le type d'un couple est `(a,b)`.
fr: Les éléments d'un tuple peuvent avoir des types différents.

~~~
en: -- All these tuples are valid
fr: -- tous ces tuples sont valides
(2,"foo")
(3,'a',[2,3])
((2,"a"),"c",3)

fst (x,y)       ⇒  x
snd (x,y)       ⇒  y

fst (x,y,z)     ⇒  ERROR: fst :: (a,b) -> a
snd (x,y,z)     ⇒  ERROR: snd :: (a,b) -> b
~~~

en: <h5 id="deal-with-parentheses">Deal with parentheses</h5>
fr: <h5 id="deal-with-parentheses">Traiter avec les parenthèses</h5>

To remove some parentheses you can use two functions: `($)` and `(.)`.

~~~
en: -- By default:
fr: -- Par défaut:
f g h x         ⇔  (((f g) h) x)

en: -- the $ replace parenthesis from the $
en: -- to the end of the expression
fr: -- le $ remplace les parenthèses depuis le $
fr: -- jusqu'à la fin de l'expression.
f g $ h x       ⇔  f g (h x) ⇔ (f g) (h x)
f $ g h x       ⇔  f (g h x) ⇔ f ((g h) x)
f $ g $ h x     ⇔  f (g (h x))

en: -- (.) the composition function
fr: -- (.) premet de faire des compositions de fonctions
(f . g) x       ⇔  f (g x)
(f . g . h) x   ⇔  f (g (h x))
~~~
