#!/bin/sh

set -ex

if [ -n "$GITHUB_WORKSPACE" ]; then
  cd "$GITHUB_WORKSPACE" || exit
fi

_USER=$(echo "$GITHUB_REPOSITORY" | cut -d / -f 1 )
_PROJECT=$(echo "$GITHUB_REPOSITORY" | cut -d / -f 2- )
_OUTPUT=".generated-changelog.md"
if [ "$INPUT_AUTHOR" = "yes" ]; then _AUTHOR="--author"; else _AUTHOR="--no-author"; fi
if [ "$INPUT_ISSUES" = "yes" ]; then _ISSUES="--issues"; else _ISSUES="--no-issues"; fi

# NOTE
# This effectively evaluates to _the closest tag (resembling a version)
# preceding previous commit on primary branch_. If the latest commit is newly
# tagged then `_SINCE_TAG` should contain previous tag, otherwise latest one.
_PREV_COMMIT=$(git rev-list --max-count=1 --skip=1 "$GITHUB_SHA")
_SINCE_TAG=$(git describe --abbrev=0 --tags --match='[0-9].*' "$_PREV_COMMIT")

/usr/local/bundle/bin/github_changelog_generator \
    --user $_USER \
    --project $_PROJECT \
    --token $INPUT_TOKEN \
    --since-tag $_SINCE_TAG \
    $_ISSUES \
    $_AUTHOR \
    --no-verbose \
    --output $_OUTPUT

sed -i '$ d' $_OUTPUT

echo ::set-output name=changelog::"$(cat $_OUTPUT)"
