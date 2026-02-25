#!/bin/bash
# Generate README.md from docs/index.md

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
ROOT_DIR="$(dirname "$SCRIPT_DIR")"
DOCS_DIR="$ROOT_DIR/docs"

echo "Generating README.md from docs/index.md..."

# Read the docs/index.md content
content=$(cat "$DOCS_DIR/index.md")

# Remove the frontmatter (lines between ---)
content=$(sed '1,/^---$/d' <<< "$content")

# Fix the image path (docs uses ../.github, README uses .github)
content="${content//..\/.github\//.github/}"

# Add Haddock section before Documentation (using awk for more robust handling)
haddock_section='## Haddock documentation

Haddock API documentation is available [here](https://IntersectMBO.github.io/cardano-addresses/haddock).

## Docusaurus-powered documentation

Proudly powered by [Docusaurus](https://docusaurus.io/) ![Docusaurus logo](https://images.icon-icons.com/2699/PNG/512/docusaurus_logo_icon_171229.png)

CLI documentation is available [here](https://IntersectMBO.github.io/cardano-addresses)

'

# Use awk to replace the ## Documentation section entirely
content=$(awk '
/^## Documentation$/ {
    print "## Documentation"
    print ""
    print "## Haddock documentation"
    print ""
    print "Haddock API documentation is available [here](https://IntersectMBO.github.io/cardano-addresses/haddock)."
    print ""
    print "## Docusaurus-powered documentation"
    print ""
    print "Proudly powered by [Docusaurus](https://docusaurus.io/) ![Docusaurus logo](https://images.icon-icons.com/2699/PNG/512/docusaurus_logo_icon_171229.png)"
    print ""
    print "CLI documentation is available [here](https://IntersectMBO.github.io/cardano-addresses)"
    print ""
    while (getline line > 0) {
        if (line ~ /^## /) {
            print line
            break
        }
    }
    next
}
{print}
' <<< "$content")

# Add license footer
license_footer='
<hr />

<p align="center">
  <a href="https://github.com/IntersectMBO/cardano-addresses/blob/master/LICENSE"><img src="https://img.shields.io/github/license/IntersectMBO/cardano-addresses.svg?style=for-the-badge" /></a>
</p>
'

# Add a blank line before the license footer
content="${content}${license_footer}"

# Write to README.md
echo "$content" > "$ROOT_DIR/README.md"

echo "README.md generated successfully!"
