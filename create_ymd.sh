#!/usr/bin/env zsh

cat <<END
-----
isHidden:       false
menupriority:   1
kind:           article
created_at:     2012-02-08T15:17:53+02:00
en: title: Haskell the Hard Way
en: subtitle: Haskell will blow your mind
fr: title: Haskell comme un vrai!
fr: subtitle: Comment se faire griller les neurones
author_name: Yann Esposito
author_uri: yannesposito.com
tags:
  - Haskell
  - programming
  - functional
  - tutorial
-----
<%= blogimage("main.png","Title image") %>

begindiv(intro)

en: <%= tldr %> A very short and dense tutorial for learning Haskell.

fr: <%= tlal %> Un tutoriel très court mais très dense pour apprendre Haskell.

> <center><sc><b>Table of Content</b></sc></center>
> begindiv(toc)
>
END

# Create the TOC

# get a list of 
# depth anchor name
grep -e '<h.' **/*.lhs | perl -pe 's#.*<h([2-6]) id="#\1 #;s#"[^>]*># "#; s#<.*#"#' |
while read num anchor title; do
    echo -n '> '
    while ((num-->2)); do echo -n "  "; done
    echo '* <a href="#'$anchor'">'${title[2,-2]}'</a>'
done


cat <<END
>
> enddiv

enddiv
END


for fic in **/*.lhs; do
    echo "\n<hr/><a href=\"code/$fic\" class=\"cut\">${fic:h}/<strong>${fic:t}</strong></a>\n"
    cat $fic
done | perl -pe 'BEGIN{$/="";} s#((^>.*\n)+)#<div class="codehighlight">\n<code class="haskell">\n$1</code>\n</div>#mg' | perl -pe 's#^> ?##'
