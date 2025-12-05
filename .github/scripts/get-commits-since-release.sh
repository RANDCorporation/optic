#!/bin/bash
# Get commits since last release for drafting release notes
# Usage: ./get-commits-since-release.sh [format]
#   format: simple (default), detailed, json, categorized

set -e

FORMAT="${1:-simple}"

# Get the last release tag
LAST_TAG=$(git describe --tags --abbrev=0 2>/dev/null || echo "")

if [ -z "$LAST_TAG" ]; then
    echo "No previous release tag found. Showing all commits." >&2
    RANGE="HEAD"
else
    echo "Commits since $LAST_TAG:" >&2
    RANGE="${LAST_TAG}..HEAD"
fi

case "$FORMAT" in
    simple)
        # Simple list of commit messages
        git log $RANGE --pretty=format:"* %s" --no-merges
        ;;

    detailed)
        # Detailed format with hash, author, date
        git log $RANGE --pretty=format:"- %s (%h by %an, %ar)" --no-merges
        ;;

    json)
        # JSON format for easy parsing
        git log $RANGE --pretty=format:'{"hash":"%h","message":"%s","author":"%an","date":"%ad"}' --no-merges --date=iso | \
        awk 'BEGIN {print "["} {if (NR>1) print ","; print} END {print "]"}'
        ;;

    categorized)
        # Categorize commits by type (for conventional commits)
        echo ""
        echo "=== NEW FEATURES ==="
        git log $RANGE --pretty=format:"%s" --no-merges | grep -iE "^(feat|feature|add|new):" || echo "None"

        echo ""
        echo "=== BUG FIXES ==="
        git log $RANGE --pretty=format:"%s" --no-merges | grep -iE "^(fix|bug):" || echo "None"

        echo ""
        echo "=== BREAKING CHANGES ==="
        git log $RANGE --pretty=format:"%s" --no-merges | grep -iE "^(break|breaking|!):" || echo "None"

        echo ""
        echo "=== IMPROVEMENTS ==="
        git log $RANGE --pretty=format:"%s" --no-merges | grep -iE "^(improve|enhance|perf|refactor):" || echo "None"

        echo ""
        echo "=== DOCUMENTATION ==="
        git log $RANGE --pretty=format:"%s" --no-merges | grep -iE "^(docs|doc|documentation):" || echo "None"

        echo ""
        echo "=== OTHER CHANGES ==="
        git log $RANGE --pretty=format:"%s" --no-merges | grep -viE "^(feat|feature|add|new|fix|bug|break|breaking|improve|enhance|perf|refactor|docs|doc|documentation):" || echo "None"
        ;;

    stats)
        # Statistics about the changes
        echo "=== COMMIT STATISTICS ==="
        echo "Total commits: $(git rev-list $RANGE --no-merges --count)"
        echo "Contributors: $(git log $RANGE --pretty=format:"%an" --no-merges | sort -u | wc -l)"
        echo "Files changed: $(git diff --name-only $LAST_TAG HEAD 2>/dev/null | wc -l)"
        echo ""
        echo "=== TOP CONTRIBUTORS ==="
        git log $RANGE --pretty=format:"%an" --no-merges | sort | uniq -c | sort -rn | head -5
        echo ""
        echo "=== FILES CHANGED MOST ==="
        git log $RANGE --name-only --pretty=format: --no-merges | sort | uniq -c | sort -rn | head -10
        ;;

    *)
        echo "Unknown format: $FORMAT" >&2
        echo "Usage: $0 [simple|detailed|json|categorized|stats]" >&2
        exit 1
        ;;
esac
