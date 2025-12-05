# Release Process Guide for optic

This guide explains the automated release process for the optic R package.

## Overview

The optic package uses an automated release workflow that distinguishes between:

- **Major/Minor releases** (e.g., 1.1.0, 2.0.0): Submitted to CRAN
- **Patch releases** (e.g., 1.0.1, 1.0.2): GitHub only

The workflow is designed to work with the **gitflow branching model**.

## Quick Start

For experienced maintainers, here's the TL;DR:

```bash
# 1. Prepare release
Rscript .github/scripts/prepare-release.R [major|minor|patch]

# 2. Commit changes
git add DESCRIPTION NEWS.md cran-comments.md
git commit -m "Prepare release X.Y.Z"

# 3. Create and push tag
git tag vX.Y.Z
git push origin main vX.Y.Z

# 4. Wait for GitHub Actions to complete
# 5. If major/minor release, submit to CRAN with devtools::submit_cran()
```

## Detailed Workflow

### 1. Determine Release Type

First, decide what type of release you're making:

| Type | When to Use | Goes to CRAN? |
|------|-------------|---------------|
| **Major** (X.0.0) | Breaking changes, major new features | ✅ Yes |
| **Minor** (X.Y.0) | New features, non-breaking changes | ✅ Yes |
| **Patch** (X.Y.Z) | Bug fixes, documentation updates | ❌ No |

**Key Rule**: Only releases where the patch version is `.0` go to CRAN (e.g., 1.1.0, 2.0.0). Patch releases stay on GitHub only.

### 2. Prepare Release Locally

#### Option A: Using the Automated Script (Recommended)

Run the release preparation script:

```r
# Install required packages if needed
install.packages(c("usethis", "devtools", "desc", "cli"))

# Run preparation script
Rscript .github/scripts/prepare-release.R major  # or minor, or patch
```

The script will:
- ✅ Check git status
- ✅ Bump version in DESCRIPTION
- ✅ Prompt you to update NEWS.md
- ✅ Check/create cran-comments.md
- ✅ Run R CMD check
- ✅ Provide next steps

#### Option B: Manual Preparation

If you prefer to do it manually:

1. **Update version in DESCRIPTION**:
   ```r
   desc::desc_set_version("1.1.0")
   ```

2. **Update NEWS.md**:
   - Change the development header to the new version
   - Add all changes since last release
   - Example:
   ```markdown
   # optic 1.1.0

   ## New features

   * Added new spillover detection methods
   * Improved performance for large datasets

   ## Bug fixes

   * Fixed issue with missing data handling
   ```

3. **Update cran-comments.md** (for CRAN releases):
   - Document R CMD check results
   - Explain any NOTEs
   - List tested platforms

4. **Run checks**:
   ```r
   devtools::check()
   devtools::check_win_devel()  # For CRAN releases
   ```

### 3. Commit and Push

```bash
# Add changes
git add DESCRIPTION NEWS.md cran-comments.md

# Commit
git commit -m "Prepare release X.Y.Z"

# Push to main (or merge develop to main first if using gitflow)
git push origin main

# Create and push tag
git tag vX.Y.Z
git push origin vX.Y.Z
```

**Important**: The tag must start with `v` (e.g., `v1.1.0`)

### 4. Automated GitHub Actions Workflow

Once you push the tag, GitHub Actions automatically:

1. **Detects release type**: Checks if patch version is 0
   - If `.0` → CRAN release (major/minor)
   - If not `.0` → GitHub release only (patch)

2. **Runs comprehensive checks**:
   - R CMD check on multiple platforms:
     - macOS (latest, R release)
     - Windows (latest, R release)
     - Ubuntu (latest, R devel)
     - Ubuntu (latest, R release)
     - Ubuntu (latest, R oldrel-1)
   - Must pass with 0 errors

3. **Builds source package**: Creates `.tar.gz` tarball

4. **Creates GitHub release**:
   - Extracts release notes from NEWS.md
   - Adds release type indicator (CRAN vs patch)
   - Uploads tarball as asset
   - Generates additional release notes from commits

5. **Updates pkgdown website**:
   - Rebuilds documentation site
   - Deploys to GitHub Pages
   - Website shows new version

**Monitor the workflow**: Go to the Actions tab on GitHub to watch progress.

### 5. CRAN Submission (Major/Minor Releases Only)

If this is a major or minor release (version X.Y.0), you need to submit to CRAN:

#### Final Pre-submission Checks

```r
# Check on Windows devel (REQUIRED)
devtools::check_win_devel()

# Check on R-hub (optional but recommended)
devtools::check_rhub()

# Check reverse dependencies if any exist
revdepcheck::revdep_check(num_workers = 4)
```

#### Submit to CRAN

**Option 1**: Using devtools (recommended)
```r
devtools::submit_cran()
```

**Option 2**: Manual submission
1. Download tarball from GitHub release
2. Go to https://cran.r-project.org/submit.html
3. Upload tarball and fill form
4. Submit

#### After Submission

1. **Confirm submission**: Click link in confirmation email
2. **Wait for auto-checks**: Usually 1 hour
3. **Fix any issues**: If auto-checks fail, fix quickly and resubmit
4. **Wait for review**: 1-3 days for manual review
5. **Respond to feedback**: Reply promptly to reviewer comments

See [RELEASE_CHECKLIST.md](.github/RELEASE_CHECKLIST.md) for detailed CRAN submission steps.

### 6. After Release

Once the release is complete (published on CRAN if applicable):

1. **Start development version**:
   ```r
   usethis::use_dev_version()
   ```

2. **Update NEWS.md**:
   ```markdown
   # optic (development version)

   * No changes yet.

   # optic 1.1.0
   ...
   ```

3. **Commit and push**:
   ```bash
   git add DESCRIPTION NEWS.md
   git commit -m "Start development version"
   git push origin develop
   ```

## File Structure

The release scaffolding includes:

```
.github/
├── workflows/
│   └── release.yml              # Main release automation workflow
├── scripts/
│   └── prepare-release.R        # Interactive release preparation script
├── RELEASE_CHECKLIST.md         # Detailed CRAN submission checklist
└── RELEASE_GUIDE.md            # This file
NEWS.md                          # Release notes (auto-extracted)
cran-comments.md                 # CRAN submission notes
```

## Gitflow Integration

If you're using gitflow:

```bash
# Start release from develop
git flow release start 1.1.0

# Prepare release
Rscript .github/scripts/prepare-release.R minor

# Commit changes
git add -A
git commit -m "Prepare release 1.1.0"

# Finish release (merges to main and develop, creates tag)
git flow release finish 1.1.0

# Push everything
git push origin main develop --tags
```

The workflow will trigger when the tag is pushed.

## Workflow Configuration

The release workflow (`.github/workflows/release.yml`) can also be triggered manually:

1. Go to Actions tab on GitHub
2. Select "Prepare Release" workflow
3. Click "Run workflow"
4. Enter version and whether it's a CRAN release
5. Run

This is useful for testing or emergency releases.

## Troubleshooting

### Workflow didn't trigger

- ✅ Check tag format: Must be `vX.Y.Z` (with lowercase `v`)
- ✅ Check tag was pushed: `git push origin --tags`
- ✅ Check Actions are enabled in repository settings

### Checks failing

- Review failure logs in Actions tab
- Fix locally and create new tag with incremented version
- Delete bad tag: `git tag -d vX.Y.Z && git push origin :vX.Y.Z`

### Wrong release type

If patch was detected as CRAN or vice versa:
- Check version number format in DESCRIPTION
- CRAN releases must have patch version = 0 (e.g., 1.1.0)
- Patch releases must have patch version > 0 (e.g., 1.1.1)

### CRAN submission issues

See [RELEASE_CHECKLIST.md](.github/RELEASE_CHECKLIST.md) for detailed troubleshooting.

## Best Practices

1. **Always run checks locally first**: Don't rely solely on CI
2. **Keep NEWS.md updated**: Update as you develop, not just at release time
3. **Test on multiple platforms**: Use `check_win_devel()` and `check_rhub()`
4. **Respond quickly to CRAN**: If issues found, fix within days
5. **Use semantic versioning**: Follow X.Y.Z convention strictly
6. **Document breaking changes**: Make them prominent in NEWS.md
7. **Coordinate with team**: Ensure all changes for release are merged

## Resources

- [CRAN Repository Policy](https://cran.r-project.org/web/packages/policies.html)
- [R Packages book - Releasing](https://r-pkgs.org/release.html)
- [usethis release functions](https://usethis.r-lib.org/reference/use_release_issue.html)
- [GitHub Actions for R](https://github.com/r-lib/actions)

## Getting Help

If you encounter issues:

1. Check this guide and the checklist
2. Review GitHub Actions logs
3. Check [r-lib/actions](https://github.com/r-lib/actions) documentation
4. Ask on R-package-devel mailing list for CRAN-specific issues

## Changelog

- **2024-XX-XX**: Initial release automation setup
  - Automated checks and tarball building
  - GitHub release creation
  - pkgdown website updates
  - CRAN vs patch release detection
