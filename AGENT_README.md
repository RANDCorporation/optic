# AI Agent-Assisted Release Process

This document explains how to use AI agents (Claude Code, GitHub
Copilot, etc.) to automate the release process for the optic package.

## Overview

The optic package has comprehensive scaffolding for AI-assisted
releases. An AI agent can:

✅ Analyze git commits since the last release ✅ Draft release notes and
NEWS.md entries ✅ Create release branches using gitflow ✅ Update
version numbers ✅ Run comprehensive tests (R CMD check) ✅ Update
documentation (cran-comments.md) ✅ Prepare everything for final
approval

**What the agent WILL NOT do:** - ❌ Merge release branches (you
approve) - ❌ Push git tags (you control) - ❌ Submit to CRAN (you
decide)

## Quick Start

### Using Claude Code

The easiest way is to use the `/release` slash command:

    /release

The agent will: 1. Ask what type of release (major/minor/patch) 2.
Analyze commits since last release 3. Draft release notes 4. Create
release branch 5. Run all checks 6. Provide summary for your approval

### Using VS Code Tasks

Press `Cmd+Shift+P` (Mac) or `Ctrl+Shift+P` (Windows/Linux), then:

1.  Type “Run Task”
2.  Select one of:
    - **“Draft Release Notes”** - Just draft notes, don’t make changes
    - **“Prepare Release (Interactive)”** - Full release prep process
    - **“Get Commits Since Release”** - View commits for manual review
    - **“R CMD check”** - Run package checks
    - **“Run Tests”** - Run test suite

### Manual Agent Invocation

You can also manually instruct any AI agent:

    I want to start a release process. Please read .github/AGENT_RELEASE_GUIDE.md
    and follow the instructions to prepare a [major/minor/patch] release.

## Typical Workflow

### 1. Finish Your Work

Complete your feature branch and merge it to `develop`:

``` bash
# Finish feature
git flow feature finish my-feature

# Ensure develop is clean
git checkout develop
git status
```

### 2. Invoke the Agent

**In Claude Code:**

    /release

**In VS Code:** - Run Task: “Prepare Release (Interactive)”

**Manual prompt:**

    I've finished my feature work. Please help me prepare a minor release.
    Read .github/AGENT_RELEASE_GUIDE.md and draft release notes by analyzing
    commits since the last release.

### 3. Agent Works

The agent will:

1.  **Verify prerequisites**

    - Check git status
    - Verify you’re on `develop`
    - Check gitflow is initialized

2.  **Ask clarifying questions**

    - Release type (major/minor/patch)
    - Any special notes or breaking changes
    - Confirm CRAN submission (major/minor only)

3.  **Analyze commits**

    - Get all commits since last release
    - Categorize by type (features, fixes, breaking, etc.)
    - Extract key changes

4.  **Draft content**

    - NEWS.md entry
    - GitHub release notes
    - Update cran-comments.md (if CRAN release)

5.  **Create release branch**

    ``` bash
    git flow release start X.Y.Z
    ```

6.  **Update files**

    - DESCRIPTION (version)
    - NEWS.md (add entry)
    - cran-comments.md (update)

7.  **Run tests**

    ``` bash
    Rscript -e "devtools::check()"
    ```

8.  **Commit changes**

    ``` bash
    git add DESCRIPTION NEWS.md cran-comments.md
    git commit -m "Prepare release X.Y.Z"
    ```

9.  **Provide summary**

    - Show what changed
    - Show drafted content
    - Show test results
    - Provide next steps

### 4. Review and Approve

Review the agent’s work:

``` bash
# Check the release branch
git diff develop release/1.1.0

# Review NEWS.md
cat NEWS.md | head -n 30

# Review cran-comments.md
cat cran-comments.md

# Check DESCRIPTION version
grep "^Version:" DESCRIPTION
```

If you need changes:

    Please update the release notes to mention [specific change]

### 5. Complete the Release

When satisfied:

``` bash
# Finish the release (merges to main and develop, creates tag)
git flow release finish 1.1.0

# You'll be prompted for:
# - Merge message (default is fine)
# - Tag message (describe the release)
# - Merge back to develop message (default is fine)

# Push everything
git push origin main develop --tags
```

### 6. GitHub Actions Takes Over

Once the tag is pushed: - GitHub Actions runs comprehensive checks -
Creates GitHub release with notes - Updates pkgdown website

### 7. CRAN Submission (Major/Minor Only)

For major or minor releases:

``` r
# Submit to CRAN
devtools::submit_cran()
```

See
[RELEASE_CHECKLIST.md](https://randcorporation.github.io/optic/RELEASE_CHECKLIST.md)
for detailed CRAN steps.

## Files and Structure

### Agent Configuration

    .claude/
    └── commands/
        └── release.md              # /release slash command

    .github/
    ├── AGENT_RELEASE_GUIDE.md      # Detailed agent instructions
    ├── AGENT_README.md             # This file
    ├── templates/
    │   ├── RELEASE_NOTES_TEMPLATE.md  # GitHub release notes template
    │   └── NEWS_TEMPLATE.md           # NEWS.md entry template
    └── scripts/
        ├── draft-release-notes.R      # Auto-draft from commits
        ├── get-commits-since-release.sh  # Get commit list
        └── prepare-release.R          # Interactive prep (human use)

    .vscode/
    └── tasks.json                  # VS Code tasks

### Workflow Files

    .github/
    └── workflows/
        └── release.yml             # Automated release on tag push

### Documentation

    .github/
    ├── RELEASE_GUIDE.md           # Human-readable guide
    ├── RELEASE_CHECKLIST.md       # CRAN submission checklist
    └── README.md                  # Quick reference

## Helper Scripts

### Draft Release Notes from Commits

``` bash
# Auto-generate draft notes
Rscript .github/scripts/draft-release-notes.R 1.1.0

# This creates:
# - NEWS_DRAFT.md (for NEWS.md)
# - RELEASE_NOTES_DRAFT.md (for GitHub)
```

### Get Commit List

``` bash
# Simple list
.github/scripts/get-commits-since-release.sh simple

# Categorized by type
.github/scripts/get-commits-since-release.sh categorized

# Detailed with authors
.github/scripts/get-commits-since-release.sh detailed

# Statistics
.github/scripts/get-commits-since-release.sh stats
```

### Interactive Release Prep (Human)

``` bash
# Interactive script for humans
Rscript .github/scripts/prepare-release.R minor
```

## Customizing Agent Behavior

### Commit Message Conventions

The agent recognizes these commit patterns:

| Pattern                         | Category         |
|---------------------------------|------------------|
| `feat:`, `feature:`, `add:`     | New features     |
| `fix:`, `bug:`                  | Bug fixes        |
| `break:`, `breaking:`, `!:`     | Breaking changes |
| `improve:`, `enhance:`, `perf:` | Improvements     |
| `docs:`, `doc:`                 | Documentation    |
| `test:`, `tests:`               | Tests            |
| `refactor:`, `chore:`, `ci:`    | Internal         |

**Tip:** Use conventional commits for better automated drafting!

### Modifying Templates

Edit these files to change how release notes are drafted:

- `.github/templates/RELEASE_NOTES_TEMPLATE.md` - GitHub release format
- `.github/templates/NEWS_TEMPLATE.md` - NEWS.md format

### Customizing Agent Instructions

Edit `.github/AGENT_RELEASE_GUIDE.md` to change agent behavior.

## Troubleshooting

### Agent Doesn’t Find Commits

**Problem:** Agent reports no commits since last release

**Solution:**

``` bash
# Check if tag exists
git tag -l

# If no tags, agent will use all commits
# If tag exists but wrong, delete and recreate:
git tag -d v1.0.0
git tag v1.0.0 <correct-commit-hash>
```

### Gitflow Not Initialized

**Problem:** Agent reports gitflow not initialized

**Solution:**

``` bash
git flow init -d  # Use defaults
```

### Agent Makes Mistakes

**Problem:** Drafted release notes are incorrect

**Solution:** - Ask agent to revise: “Please update the release notes
to…” - Edit manually: `nano NEWS.md` - Use draft scripts and review:
`Rscript .github/scripts/draft-release-notes.R 1.1.0`

### Release Branch Already Exists

**Problem:** `git flow release start` fails - branch exists

**Solution:**

``` bash
# Delete old release branch
git branch -D release/1.1.0

# Or, if it was abandoned:
git flow release delete 1.1.0
```

## Best Practices

### For Better Agent Results

1.  **Use conventional commits**: Helps agent categorize changes
2.  **Write descriptive commit messages**: Agent will use these
3.  **Keep commits focused**: One logical change per commit
4.  **Reference issues**: Use `#123` in commits for tracking
5.  **Merge features to develop**: Don’t release from feature branches

### Review Checklist

Before approving agent’s work:

Version number is correct

Release type matches changes (major/minor/patch)

NEWS.md entry is comprehensive and accurate

Breaking changes are clearly marked

cran-comments.md is updated (CRAN releases)

All tests pass (0 errors, 0 warnings)

Documentation is updated

Commit is made to release branch

### Communication with Agent

**Good prompts:**

    /release

    Please prepare a minor release. The main changes are improved performance
    and a new spillover detection method.

    I've finished feature work. Draft release notes from commits and create
    a release branch for version 1.1.0.

**Less effective prompts:**

    Release                    # Too vague
    Make version 2.0           # Missing context
    Update NEWS                # Unclear what to do

## Advanced Usage

### Release After Hotfix

If you made a hotfix:

``` bash
# Hotfix creates a patch release
git flow hotfix finish 1.0.3

# Tag is already created by git flow
# Just push:
git push origin main develop --tags
```

### Testing Release Process

Create a test release branch without agent:

``` bash
git flow release start 1.1.0-rc1
# Make changes
# Test
git flow release finish 1.1.0-rc1  # Creates tag v1.1.0-rc1
```

### Partial Agent Assistance

You can ask agent for specific tasks:

    Please analyze commits since last release and draft NEWS.md entries,
    but don't create a release branch yet.

    I've already created a release branch. Please run R CMD check and
    report the results.

## FAQ

**Q: Can the agent submit to CRAN for me?** A: No. CRAN submission
requires human oversight. The agent prepares everything, but you submit.

**Q: What if I disagree with the agent’s categorization?** A: Just ask
it to revise: “Please move the X item from improvements to new features”

**Q: Can I use this without gitflow?** A: Yes, but you’ll need to adapt
the process. The agent assumes gitflow by default.

**Q: Does this work with main/master instead of develop?** A: Yes, but
update `.github/AGENT_RELEASE_GUIDE.md` to reflect your branch names.

**Q: How do I know if release will go to CRAN?** A: Major/minor releases
(X.Y.0) go to CRAN. Patches (X.Y.Z where Z \> 0) don’t.

**Q: Can I run this in CI/CD?** A: The agent-assisted process is
interactive. For full automation, use the workflow in `release.yml`.

## Support

For issues with: - **Agent behavior**: Edit
`.github/AGENT_RELEASE_GUIDE.md` - **Workflows**: See
`.github/workflows/release.yml` - **CRAN submission**: See
`.github/RELEASE_CHECKLIST.md` - **General process**: See
`.github/RELEASE_GUIDE.md`

## Examples

### Example 1: Minor Release

    You: /release

    Agent: I'll help you prepare a release. Let me check the current state...

    ✅ Current branch: develop
    ✅ Working directory: clean
    ✅ Current version: 1.0.2

    Analyzing commits since v1.0.2...

    Found 12 commits:
    - 5 new features
    - 4 bug fixes
    - 2 documentation updates
    - 1 internal change

    Based on the changes, I recommend a MINOR release (1.1.0).

    Should this be version 1.1.0? (Y/n)

    You: Y

    Agent: Great! [continues with process...]

### Example 2: Patch Release

    You: I fixed a small bug. Please prepare a patch release.

    Agent: I'll prepare a patch release. [analyzes commits]

    Found 2 commits since v1.0.2:
    - Fixed error in dispatch_simulation()
    - Updated documentation

    This will be version 1.0.3 (patch release - GitHub only, not CRAN).

    Proceed? (Y/n)

    You: Y

    Agent: [continues with process...]

## Conclusion

The AI agent-assisted release process: - Saves time analyzing commits -
Ensures consistent release notes - Reduces errors in version bumps -
Automates tedious tasks - Maintains quality through checks - Leaves
final decisions to you

Use `/release` in Claude Code to get started!
