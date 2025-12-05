#!/usr/bin/env Rscript
# Release Preparation Script
# Run this script before creating a release tag to ensure everything is ready
# Usage: Rscript .github/scripts/prepare-release.R [major|minor|patch]

# Check if required packages are installed
required_pkgs <- c("usethis", "devtools", "desc", "cli")
missing_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(missing_pkgs) > 0) {
  stop("Missing required packages: ", paste(missing_pkgs, collapse = ", "),
       "\nInstall with: install.packages(c('", paste(missing_pkgs, collapse = "', '"), "'))")
}

library(cli)

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
bump_type <- if (length(args) > 0) args[1] else "patch"

if (!bump_type %in% c("major", "minor", "patch", "dev")) {
  stop("Invalid version bump type. Use: major, minor, patch, or dev")
}

cli_h1("Preparing Release for optic")

# 1. Check git status
cli_h2("Checking git status")
git_status <- system2("git", c("status", "--porcelain"), stdout = TRUE)
if (length(git_status) > 0) {
  cli_alert_warning("Working directory has uncommitted changes:")
  cat(git_status, sep = "\n")
  response <- readline("Continue anyway? (y/N): ")
  if (tolower(response) != "y") {
    stop("Aborted by user")
  }
}
cli_alert_success("Git status check complete")

# 2. Get current branch
current_branch <- system2("git", c("rev-parse", "--abbrev-ref", "HEAD"), stdout = TRUE)
cli_alert_info(paste("Current branch:", current_branch))

if (bump_type != "dev" && current_branch == "develop") {
  cli_alert_warning("You're on the develop branch. For releases, you should:")
  cli_alert_info("1. Merge develop into main (or create a release branch)")
  cli_alert_info("2. Run this script from main/release branch")
  response <- readline("Continue on develop branch anyway? (y/N): ")
  if (tolower(response) != "y") {
    stop("Aborted by user")
  }
}

# 3. Show current version
current_version <- desc::desc_get_version()
cli_h2(paste("Current version:", current_version))

# 4. Bump version
if (bump_type != "dev") {
  cli_h2(paste("Bumping version:", bump_type))

  # Calculate new version
  v <- as.numeric(strsplit(as.character(current_version), "\\.")[[1]])
  if (bump_type == "major") {
    new_version <- paste0(v[1] + 1, ".0.0")
  } else if (bump_type == "minor") {
    new_version <- paste0(v[1], ".", v[2] + 1, ".0")
  } else if (bump_type == "patch") {
    new_version <- paste0(v[1], ".", v[2], ".", v[3] + 1)
  }

  cli_alert_info(paste("New version will be:", new_version))

  response <- readline("Update version in DESCRIPTION? (Y/n): ")
  if (tolower(response) != "n") {
    desc::desc_set_version(new_version)
    cli_alert_success(paste("Version updated to", new_version))

    # Determine if CRAN release
    is_cran <- v[3] == 0  # patch == 0 means major or minor
    if (is_cran) {
      cli_alert_info("This will be a CRAN release (major/minor)")
    } else {
      cli_alert_info("This will be a patch release (GitHub only)")
    }
  }
}

# 5. Check NEWS.md
cli_h2("Checking NEWS.md")
if (file.exists("NEWS.md")) {
  news_content <- readLines("NEWS.md", n = 20)

  # Check if there's a development section
  has_dev <- any(grepl("^# optic.*development", news_content, ignore.case = TRUE))

  if (has_dev && bump_type != "dev") {
    cli_alert_warning("NEWS.md has a development section. You should:")
    cli_alert_info("1. Update the development header to the new version")
    cli_alert_info("2. Add release date")
    cat("\nCurrent NEWS.md header:\n")
    cat(news_content[1:10], sep = "\n")

    response <- readline("\nPress Enter to open NEWS.md for editing (or 's' to skip): ")
    if (tolower(response) != "s") {
      file.edit("NEWS.md")
      readline("Press Enter when done editing NEWS.md...")
    }
  } else if (!has_dev && bump_type != "dev") {
    cli_alert_success("NEWS.md looks good")
  }
} else {
  cli_alert_warning("NEWS.md not found. Creating template...")
  usethis::use_news_md()
}

# 6. Check cran-comments.md
cli_h2("Checking cran-comments.md")
if (file.exists("cran-comments.md")) {
  cli_alert_success("cran-comments.md exists")
  response <- readline("Review/edit cran-comments.md? (y/N): ")
  if (tolower(response) == "y") {
    file.edit("cran-comments.md")
    readline("Press Enter when done editing...")
  }
} else {
  cli_alert_warning("cran-comments.md not found")
  response <- readline("Create cran-comments.md template? (Y/n): ")
  if (tolower(response) != "n") {
    usethis::use_cran_comments()
  }
}

# 7. Run checks
cli_h2("Running package checks")
cli_alert_info("Running R CMD check locally...")

check_results <- tryCatch({
  devtools::check(document = FALSE, quiet = TRUE)
}, error = function(e) {
  cli_alert_danger(paste("Check failed:", e$message))
  NULL
})

if (!is.null(check_results)) {
  n_errors <- length(check_results$errors)
  n_warnings <- length(check_results$warnings)
  n_notes <- length(check_results$notes)

  if (n_errors > 0) {
    cli_alert_danger(paste(n_errors, "errors found"))
    print(check_results$errors)
  }
  if (n_warnings > 0) {
    cli_alert_warning(paste(n_warnings, "warnings found"))
    print(check_results$warnings)
  }
  if (n_notes > 0) {
    cli_alert_info(paste(n_notes, "notes found"))
    print(check_results$notes)
  }

  if (n_errors == 0 && n_warnings == 0) {
    cli_alert_success("Package checks passed!")
  } else {
    cli_alert_danger("Fix errors and warnings before releasing")
    stop("Package check failed")
  }
}

# 8. Final summary and instructions
cli_h1("Release Preparation Summary")

new_version <- desc::desc_get_version()
is_cran <- as.numeric(strsplit(as.character(new_version), "\\.")[[1]])[3] == 0

cli_alert_success(paste("Package is ready for release:", new_version))
cli_alert_info(paste("Release type:", if (is_cran) "CRAN (major/minor)" else "GitHub only (patch)"))

cli_h2("Next steps:")
cli_ol(c(
  "Review and commit any changes (DESCRIPTION, NEWS.md, cran-comments.md)",
  paste0("Push to main branch: git push origin ", current_branch),
  paste0("Create and push tag: git tag v", new_version, " && git push origin v", new_version),
  "GitHub Actions will automatically:",
  "  - Run R CMD check on multiple platforms",
  "  - Build source tarball",
  "  - Create GitHub release",
  "  - Update pkgdown website",
  if (is_cran) "After GitHub release, submit to CRAN using devtools::submit_cran()" else NULL
))

if (is_cran) {
  cli_h2("CRAN submission checklist:")
  cli_ul(c(
    "Run devtools::check_win_devel() to test on Windows devel",
    "Run devtools::check_rhub() if needed for additional platforms",
    "Review CRAN policies: https://cran.r-project.org/web/packages/policies.html",
    "Submit with devtools::submit_cran() or via https://cran.r-project.org/submit.html"
  ))
}

cli_alert_success("Release preparation complete!")
