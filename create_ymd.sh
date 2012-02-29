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
-----
<%= blogimage("main.png","Title image") %>

begindiv(intro)

en: <%= tldr %>

fr: <%= tlal %>

> <center><sc><b>Table of Content</b></sc></center>
> 
> * Table of Content (generated)
> {:toc}

enddiv

END

for fic in **/*.lhs; do
    echo "\n<hr/><a href=\"code/$fic\" class=\"cut\">${fic:h}/<strong>${fic:t}</strong></a>\n"
    cat $fic
done | perl -pe 'BEGIN{$/="";} s#((^>.*\n)+)#<div class="codehighlight">\n<code class="haskell">\n$1</code>\n</div>#mg' | perl -pe 's#^> ?##'
