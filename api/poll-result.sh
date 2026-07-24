#! /bin/sh

sid="$1"

if [ x"$sid" = x ]; then
    echo "submission_id required!"
    exit 1
fi

[ ! -r ./.env ] || . ./.env

curl https://icfpcontest2026.com/api/v1/submissions/"$sid" \
  -H "Authorization: Bearer $API_KEY"
