#!/usr/bin/env zsh

yblogdir=../yblog

err() { print -- $* >&2; exit 1}

[[ -e $yblogdir ]] || err "Can't find $yblogdir"

print -- "Generating ymd file"
./create_ymd.sh > $yblogdir/multi/blog/Haskell-the-Hard-Way.md
for lang in fr en; do
    print -- "Copying LHS files ($lang)"
    rsync -amv --include '*/' --include='*.lhs' --exclude='*' . $yblogdir/content/Scratch/${lang}/blog/Haskell-the-Hard-Way/code/
done

print -- "Don't forget to publish the change."
