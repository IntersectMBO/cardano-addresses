#!/bin/bash
curl -sSL https://raw.github.com/jaspervdj/stylish-haskell/master/scripts/latest.sh | sh -s $(find . -type f -name "*.hs" ! -path "*.stack-work*") -i
if [ -z "$(git status --porcelain)" ]; then
    echo "No style errors detected."
else
    echo "Style errors detected:"
    git diff
    exit 1
fi
