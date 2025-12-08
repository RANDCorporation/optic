# GitHub Configuration for optic

This directory contains GitHub-specific configuration and documentation
for the optic package.

## Release Management

The optic package uses automated workflows for releasing to CRAN and
GitHub:

### Quick Links

- **[RELEASE_GUIDE.md](https://randcorporation.github.io/optic/RELEASE_GUIDE.md)** -
  Complete guide to the release process
- **[RELEASE_CHECKLIST.md](https://randcorporation.github.io/optic/RELEASE_CHECKLIST.md)** -
  Detailed CRAN submission checklist
- **[scripts/prepare-release.R](https://randcorporation.github.io/optic/scripts/prepare-release.R)** -
  Interactive release preparation script

### How to Release

``` bash
# 1. Prepare (major, minor, or patch)
Rscript .github/scripts/prepare-release.R minor

# 2. Commit and tag
git add DESCRIPTION NEWS.md cran-comments.md
git commit -m "Prepare release X.Y.Z"
git tag vX.Y.Z
git push origin main vX.Y.Z

# 3. GitHub Actions handles the rest!
# 4. For major/minor releases, submit to CRAN
```

### Release Types

| Type  | Example       | Goes to CRAN?       |
|-------|---------------|---------------------|
| Major | 1.0.0 → 2.0.0 | ✅ Yes              |
| Minor | 1.0.0 → 1.1.0 | ✅ Yes              |
| Patch | 1.0.0 → 1.0.1 | ❌ No (GitHub only) |

## Workflows

### Active Workflows

- **[release.yml](https://randcorporation.github.io/optic/workflows/release.yml)** -
  Main release automation
  - Triggered by version tags (e.g., `v1.0.0`)
  - Runs R CMD check on multiple platforms
  - Creates GitHub releases
  - Updates pkgdown website
  - Distinguishes CRAN vs patch releases
- **[R-CMD-check.yml](https://randcorporation.github.io/optic/workflows/R-CMD-check.yml)** -
  Continuous integration
  - Runs on push/PR to main and develop
  - Tests on multiple OS and R versions
- **[test-coverage.yml](https://randcorporation.github.io/optic/workflows/test-coverage.yml)** -
  Code coverage
  - Uploads coverage to Codecov

## Documentation

### For Maintainers

- Start with
  [RELEASE_GUIDE.md](https://randcorporation.github.io/optic/RELEASE_GUIDE.md)
  for the complete process
- Use
  [RELEASE_CHECKLIST.md](https://randcorporation.github.io/optic/RELEASE_CHECKLIST.md)
  when submitting to CRAN
- Run
  [scripts/prepare-release.R](https://randcorporation.github.io/optic/scripts/prepare-release.R)
  before each release

### Key Points

1.  **Only major/minor releases go to CRAN** (version X.Y.0)
2.  **Patch releases are GitHub-only** (version X.Y.Z where Z \> 0)
3.  **Automated checks must pass** before releases are created
4.  **pkgdown site updates automatically** on release

## Getting Help

- Check the
  [RELEASE_GUIDE.md](https://randcorporation.github.io/optic/RELEASE_GUIDE.md)
  for detailed instructions
- Review workflow logs in the Actions tab
- See [r-lib/actions documentation](https://github.com/r-lib/actions)
  for CI/CD issues
