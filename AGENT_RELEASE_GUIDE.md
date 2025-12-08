# AI Agent Release Guide for optic

This guide is for AI agents (Claude Code, GitHub Copilot, etc.) to
autonomously manage the release process for the optic R package.

## Overview

You are an AI release manager. Your job is to: 1. Analyze recent changes
from git history 2. Draft release notes and NEWS entries 3. Create a
release branch using gitflow 4. Run all necessary tests 5. Prepare
everything for the user to approve and publish

**DO NOT**: Push tags, merge branches, or submit to CRAN. The user will
do these final steps.

## Prerequisites Check

Before starting, verify:

``` bash
# Check current branch (should be 'develop' typically)
git branch --show-current

# Check git status (should be clean)
git status

# Check if gitflow is initialized
git flow version

# Check R and required packages
Rscript -e "packageVersion('devtools'); packageVersion('usethis'); packageVersion('desc')"
```

If gitflow is not initialized:

``` bash
git flow init -d  # Use defaults
```

## Step-by-Step Process

### Step 1: Gather Information

Ask the user: 1. **Release type**: major, minor, or patch? 2. **Special
notes**: Any breaking changes or important highlights? 3. **CRAN
submission**: Confirm if this is a CRAN release (should be yes for
major/minor, no for patch)

Read current version:

``` bash
Rscript -e "desc::desc_get_version()"
```

### Step 2: Analyze Git History

Get all commits since the last release:

``` bash
# Get the last release tag
LAST_TAG=$(git describe --tags --abbrev=0 2>/dev/null || echo "")

# If no tag exists, get all commits from initial commit
if [ -z "$LAST_TAG" ]; then
  git log --pretty=format:"%h - %s (%an, %ar)" --no-merges
else
  git log ${LAST_TAG}..HEAD --pretty=format:"%h - %s (%an, %ar)" --no-merges
fi
```

Better formatted for analysis:

``` bash
# Get commits grouped by type (if using conventional commits)
git log ${LAST_TAG}..HEAD --pretty=format:"%s" --no-merges | sort

# Or get detailed view
git log ${LAST_TAG}..HEAD --pretty=format:"- %s (%h)" --no-merges
```

**Analyze the commits and categorize them:** - **New features**: New
functionality added - **Bug fixes**: Fixes to existing functionality -
**Breaking changes**: Changes that break backward compatibility -
**Documentation**: Docs, vignettes, examples - **Internal**:
Refactoring, tests, CI/CD changes (usually not in release notes) -
**Dependencies**: Package dependency changes

### Step 3: Draft Release Notes

Using the analyzed commits, create release notes following this template
structure (see `.github/templates/RELEASE_NOTES_TEMPLATE.md`):

**For NEWS.md format:**

``` markdown
# optic X.Y.Z

## New features

* Feature 1 description (based on commits)
* Feature 2 description

## Bug fixes

* Bug fix 1 (based on commits)
* Bug fix 2

## Breaking changes

* Breaking change description (if any)

## Internal changes

* Internal improvements (optional, only if significant)
```

**For GitHub release notes format:**

``` markdown
## Summary

Brief 2-3 sentence summary of this release.

## What's New

* Feature 1
* Feature 2

## Bug Fixes

* Fix 1
* Fix 2

## Breaking Changes

* Change 1 (if any)

## Installation

```r
# From CRAN (after acceptance)
install.packages("optic")

# From GitHub
remotes::install_github("randcorporation/optic@vX.Y.Z")
```

## Full Changelog

See [NEWS.md](https://randcorporation.github.io/optic/NEWS.md) for
complete details.

    **Key Principles for Drafting:**
    - Be concise but informative
    - Focus on user impact, not technical details
    - Group related changes together
    - Highlight breaking changes prominently
    - Use past tense ("Added", "Fixed", not "Add", "Fix")
    - Reference issue numbers if available (e.g., "Fixed #123")

    ### Step 4: Create Release Branch

    Use gitflow to create a release branch:

    ```bash
    # For version 1.1.0
    git flow release start 1.1.0

This creates a `release/1.1.0` branch from `develop`.

### Step 5: Update Version and Documentation

**Update DESCRIPTION:**

``` r
desc::desc_set_version("1.1.0")
```

**Update NEWS.md:**

Read the current NEWS.md:

``` bash
cat NEWS.md
```

Use the Edit tool to update it: - Replace the “development version”
section with the new version - Add your drafted release notes - Keep the
development section at the top for the next cycle

Example structure:

``` markdown
# optic (development version)

* No changes yet.

# optic 1.1.0

[Your drafted notes here]

# optic 1.0.2
[Previous version notes...]
```

**Update cran-comments.md (for CRAN releases only):**

Read current cran-comments.md and update it with: - Description of
changes in this version - R CMD check results (you’ll add this after
running checks) - Notes about any warnings or notes from checks - Tested
platforms

Template:

``` markdown
## Release Summary

This is a [major/minor] release of optic. This version includes:

* [Key change 1]
* [Key change 2]

## Test environments

* Local: macOS Sonoma, R 4.4.0
* GitHub Actions:
  - macOS-latest (R release)
  - windows-latest (R release)
  - ubuntu-latest (R devel, release, oldrel-1)
* win-builder: R devel

## R CMD check results

There were no ERRORs or WARNINGs.

There was 1 NOTE:

* [Note description and explanation if applicable]
```

### Step 6: Run Comprehensive Tests

**Run local R CMD check:**

``` bash
Rscript -e "devtools::check()"
```

Parse the results and report: - Errors (must be 0) - Warnings (must be
0) - Notes (document any)

If there are errors or warnings, STOP and report them to the user.

**Run additional checks (optional but recommended):**

``` bash
# Check examples
Rscript -e "devtools::run_examples()"

# Build vignettes
Rscript -e "devtools::build_vignettes()"

# Run tests
Rscript -e "devtools::test()"
```

### Step 7: Commit Changes

Commit all changes to the release branch:

``` bash
git add DESCRIPTION NEWS.md cran-comments.md
git commit -m "Prepare release X.Y.Z"
```

### Step 8: Generate Release Summary

Create a comprehensive summary for the user including:

1.  **Release Information:**

    - Version: X.Y.Z
    - Type: Major/Minor/Patch
    - CRAN submission: Yes/No
    - Branch: release/X.Y.Z

2.  **Changes Summary:**

    - Number of commits since last release
    - Key features added
    - Bugs fixed
    - Breaking changes (if any)

3.  **Drafted Content:**

    - NEWS.md entries (show excerpt)
    - cran-comments.md (show excerpt)
    - Suggested GitHub release notes (provide full text)

4.  **Test Results:**

    - R CMD check: ✅ PASSED (0 errors, 0 warnings, N notes)
    - Notes explanation if any

5.  **Next Steps for User:**

    ``` bash
    # Review the changes
    git diff develop release/X.Y.Z

    # Review NEWS.md
    cat NEWS.md

    # If everything looks good, finish the release
    git flow release finish X.Y.Z
    # (This will merge to main and develop, create a tag)

    # Push everything
    git push origin main develop --tags

    # GitHub Actions will automatically:
    # - Run checks
    # - Create GitHub release
    # - Update pkgdown site

    # For CRAN releases, then submit:
    Rscript -e "devtools::submit_cran()"
    ```

6.  **Warnings/Caveats:**

    - Any issues found during checks
    - Any manual review needed
    - Any breaking changes to communicate

## Templates and Helpers

Use these files for reference: -
`.github/templates/RELEASE_NOTES_TEMPLATE.md` - Release notes
structure - `.github/templates/NEWS_TEMPLATE.md` - NEWS.md entry
format - `.github/RELEASE_CHECKLIST.md` - Full CRAN checklist -
`.github/RELEASE_GUIDE.md` - Human-readable guide

## Common Patterns for Commit Analysis

When analyzing commits, look for these patterns:

**Feature additions:** - “add”, “implement”, “introduce”, “new” - Files:
New R files in R/, new exports in NAMESPACE

**Bug fixes:** - “fix”, “resolve”, “correct”, “patch” - Files: Changes
to existing R files

**Breaking changes:** - “break”, “remove”, “deprecate”, “change API” -
Look for: Removed functions, changed function signatures

**Documentation:** - “docs”, “documentation”, “vignette”, “readme” -
Files: .Rd files, vignettes/, README.md

**Tests:** - “test”, “spec”, “coverage” - Files: tests/

**Internal/CI:** - “refactor”, “cleanup”, “ci”, “workflow” - Files:
.github/, internal functions

## Error Handling

If you encounter errors:

**Git errors:** - Uncommitted changes: Ask user to commit or stash -
Detached HEAD: Ask user to checkout a branch - Merge conflicts: Report
and ask user to resolve

**R CMD check errors:** - ERRORS: Report in detail, don’t proceed -
WARNINGS: Report in detail, ask user if they want to proceed - NOTES:
Document in cran-comments.md, can proceed

**Missing files:** - NEWS.md: Create from template - cran-comments.md:
Create from template

## Example Interaction

    User: /release

    Agent: I'll help you prepare a release for optic. Let me check the current state...

    ✅ Current branch: develop
    ✅ Working directory: clean
    ✅ Gitflow: initialized
    ✅ Current version: 1.0.2

    Analyzing commits since last release (v1.0.2)...

    Found 15 commits:
    - 8 bug fixes
    - 3 new features
    - 2 documentation updates
    - 2 internal changes

    Based on the changes, I recommend a MINOR release (1.1.0) because there are new features.

    Questions:
    1. Should this be version 1.1.0? (Y/n)
    2. Are there any breaking changes I should highlight? (y/N)
    3. This will be submitted to CRAN (minor release). Confirm? (Y/n)

    [User responds]

    Agent: Great! Starting release process...

    🔄 Creating release branch (release/1.1.0)...
    ✅ Created release/1.1.0

    🔄 Drafting release notes from commits...
    ✅ Drafted release notes (see below)

    🔄 Updating NEWS.md...
    ✅ Updated NEWS.md

    🔄 Updating DESCRIPTION version to 1.1.0...
    ✅ Updated DESCRIPTION

    🔄 Updating cran-comments.md...
    ✅ Updated cran-comments.md

    🔄 Running R CMD check...
    ✅ Check passed (0 errors, 0 warnings, 1 note)

    Note found: [explanation]

    🔄 Committing changes...
    ✅ Committed to release/1.1.0

    ---

    ## Release Summary

    **Version:** 1.1.0 (Minor release)
    **CRAN submission:** Yes
    **Branch:** release/1.1.0

    ### Drafted Release Notes

    [Shows drafted content]

    ### Test Results

    R CMD check: ✅ PASSED
    - 0 errors
    - 0 warnings
    - 1 note (documented in cran-comments.md)

    ### Next Steps

    Review the changes and if everything looks good:

    ```bash
    git flow release finish 1.1.0
    git push origin main develop --tags

Then wait for GitHub Actions to complete and submit to CRAN.

Would you like me to make any changes to the release notes? \`\`\`

## Checklist for Agent

Before finishing, ensure:

Release type confirmed with user

Release branch created

Version updated in DESCRIPTION

NEWS.md updated with comprehensive notes

cran-comments.md updated (CRAN releases)

R CMD check run and passed

All changes committed

Summary provided to user

Next steps clearly documented

## Tips for Success

1.  **Read commits carefully**: Don’t just list commits, synthesize them
    into user-friendly notes
2.  **Ask questions**: If unclear, ask the user for clarification
3.  **Be thorough**: Run all checks, don’t skip steps
4.  **Be clear**: Provide structured output with clear sections
5.  **Stop at handoff**: Don’t merge or tag - let user review and
    approve
6.  **Document everything**: If you find issues, document them clearly
