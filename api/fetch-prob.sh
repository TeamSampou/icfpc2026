#! /bin/sh

slug="$1"

if [ x"$slug" = x ]; then
    echo "slug equired! , like, atoi, max-element, triangle, ..." 1>&2
    exit 1
fi

set -x
curl https://icfpcontest2026.com/api/v1/public/problems/"$slug"
