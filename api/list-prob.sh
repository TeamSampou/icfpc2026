#! /bin/sh

[ x"$DEBUG_API" != x1 ] || set -x

curl https://icfpcontest2026.com/api/v1/public/problems
echo ''
