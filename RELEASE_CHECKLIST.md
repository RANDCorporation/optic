# CRAN Release Checklist for optic

This checklist guides you through releasing a new version to CRAN. Note
that only **major and minor releases** (e.g., 1.1.0, 2.0.0) are
submitted to CRAN. Patch releases (e.g., 1.0.1) are GitHub-only.

## Before Starting

All issues for this milestone are closed

All PRs for this milestone are merged to `develop`

Create release branch from `develop` (for gitflow):
`git flow release start X.Y.0`

Or merge `develop` into `main` if not using release branches

## Preparation Steps (Local)

### 1. Run the Release Preparation Script

``` r
# For major release (1.0.0 -> 2.0.0)
Rscript .github/scripts/prepare-release.R major

# For minor release (1.0.0 -> 1.1.0)
Rscript .github/scripts/prepare-release.R minor

# For patch release (1.0.0 -> 1.0.1) - GitHub only
Rscript .github/scripts/prepare-release.R patch
```

The script will: - Check git status - Bump version in DESCRIPTION -
Prompt you to update NEWS.md - Check/create cran-comments.md - Run R CMD
check

### 2. Update Documentation

Update `NEWS.md` with all changes since last release

- Use format: `# optic X.Y.Z` (not “development”)
- List all new features, bug fixes, breaking changes
- Be concise but informative for users

Update `cran-comments.md` with:

- Changes in this version
- R CMD check results (should be 0 errors, 0 warnings)
- Any notes and explanations
- Downstream dependencies check results (if applicable)

Update version in `DESCRIPTION` (done by script)

Run `devtools::document()` to update man pages if needed

### 3. Run Comprehensive Checks

``` r
# Standard check
devtools::check()

# Check on Windows devel (REQUIRED for CRAN submissions)
devtools::check_win_devel()

# Check on R-hub (optional but recommended for first-time submissions)
devtools::check_rhub()

# Check reverse dependencies if any
revdepcheck::revdep_check(num_workers = 4)
```

All checks pass with 0 errors, 0 warnings

Any notes are documented and explained in `cran-comments.md`

### 4. Commit and Push

``` bash
# Commit all changes
git add DESCRIPTION NEWS.md cran-comments.md
git commit -m "Prepare release X.Y.Z"

# For gitflow: finish release and push
git flow release finish X.Y.Z
git push origin develop main --tags

# Or just push main and tags
git push origin main
git tag vX.Y.Z
git push origin vX.Y.Z
```

## Automated Steps (GitHub Actions)

After pushing the tag, GitHub Actions will automatically:

1.  ✅ Run R CMD check on multiple platforms (macOS, Windows, Ubuntu
    with multiple R versions)
2.  ✅ Build source tarball (`.tar.gz`)
3.  ✅ Create GitHub release with extracted release notes
4.  ✅ Upload tarball as release asset
5.  ✅ Update pkgdown website

**Wait for all checks to pass before proceeding to CRAN submission.**

## CRAN Submission

### Option 1: Using devtools (Recommended)

``` r
# Submit to CRAN
devtools::submit_cran()
```

This will: - Build the package - Run final checks - Upload to CRAN’s
submission system - You’ll receive a confirmation email

### Option 2: Manual Submission

1.  Download the `.tar.gz` from GitHub release
2.  Go to <https://cran.r-project.org/submit.html>
3.  Upload the tarball
4.  Fill in the submission form
5.  Submit

### After Submission

CRAN will send confirmation email - click the link to confirm

CRAN will run automatic checks (usually within 1 hour)

If issues found, fix and resubmit quickly

If checks pass, wait for manual review (can take 1-3 days)

Respond promptly to any reviewer feedback

## After CRAN Acceptance

Verify package appears on CRAN

Announce release (Twitter, blog, mailing lists, etc.)

Close milestone on GitHub

Start development version:

``` r
# Bump to development version
usethis::use_dev_version()

# Add development section to NEWS.md
usethis::use_news_md()
```

Update `NEWS.md` to add development section:

``` markdown
# optic (development version)

* No changes yet.
```

Commit and push:

``` bash
git add DESCRIPTION NEWS.md
git commit -m "Start development version X.Y.Z.9000"
git push origin develop
```

## Troubleshooting

### Common CRAN Check Issues

1.  **Examples taking too long**: Use `\donttest{}` or reduce example
    data size
2.  **Vignette build time**: Reduce number of simulations/iterations
3.  **NOTEs about package size**: Check if test data can be reduced
4.  **Depends vs Imports**: Follow CRAN guidelines strictly

### If Submission is Rejected

1.  Read reviewer comments carefully
2.  Make necessary changes
3.  Update `cran-comments.md` with explanation of changes
4.  Resubmit promptly (within 2 weeks if possible)

## Resources

- [CRAN Repository
  Policy](https://cran.r-project.org/web/packages/policies.html)
- [Releasing to CRAN (R Packages book)](https://r-pkgs.org/release.html)
- [R-hub builder](https://builder.r-hub.io/)
- [Win-builder](https://win-builder.r-project.org/)

## Quick Reference: Release Types

| Release Type | Version Example | Submitted to CRAN?  | Typical Use Case                     |
|--------------|-----------------|---------------------|--------------------------------------|
| Major        | 1.0.0 → 2.0.0   | ✅ Yes              | Breaking changes, major new features |
| Minor        | 1.0.0 → 1.1.0   | ✅ Yes              | New features, non-breaking changes   |
| Patch        | 1.0.0 → 1.0.1   | ❌ No (GitHub only) | Bug fixes, minor improvements        |
