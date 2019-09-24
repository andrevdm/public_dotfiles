#!/bin/sh

cm=$(cmus-remote -Q)


artist=$(echo "$cm" | grep tag | grep artist | head -n 1 | cut -d ' ' -f 3-)
title=$(echo "$cm" | grep tag | grep title | head -n 1 | cut -d ' ' -f 3-)
echo "$artist: $title"
