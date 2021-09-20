#!/bin/sh

set -e

if [ -n "$GITHUB_WORKSPACE" ]; then
  cd "$GITHUB_WORKSPACE" || exit
fi

_USER=$(echo "$GITHUB_REPOSITORY" | cut -d / -f 1 )
_PROJECT=$(echo "$GITHUB_REPOSITORY" | cut -d / -f 2- )
_OUTPUT="$INPUT_FILENAME"
if [ "$INPUT_AUTHOR" = "yes" ]; then _AUTHOR="--author"; else _AUTHOR="--no-author"; fi
if [ "$INPUT_ISSUES" = "yes" ]; then _ISSUES="--issues"; else _ISSUES="--no-issues"; fi

/usr/local/bundle/bin/github_changelog_generator \
    --user $_USER \
    --project $_PROJECT \
    --token $INPUT_TOKEN \
    --since-tag $INPUT_SINCETAG \
    $_ISSUES \
    $_AUTHOR \
    --no-verbose \
    --output $_OUTPUT

sed -i '$ d' $_OUTPUT

echo ::set-output name=changelog::"$(cat $_OUTPUT)"
echo ::set-output name=filename::"$_OUTPUT"
