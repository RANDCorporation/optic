#!/usr/bin/env Rscript
# Draft release notes by analyzing git commits
# Usage: Rscript .github/scripts/draft-release-notes.R [version]

library(cli)

# Get command line arguments
args <- commandArgs(trailingOnly = TRUE)
new_version <- if (length(args) > 0) args[1] else NULL

cli_h1("Drafting Release Notes for optic")

# Get current version from DESCRIPTION
current_version <- desc::desc_get_version()
cli_alert_info(paste("Current version:", current_version))

# If no version specified, ask for it
if (is.null(new_version)) {
  new_version <- readline("Enter new version number (e.g., 1.1.0): ")
}

cli_alert_info(paste("New version:", new_version))

# Get last tag
last_tag <- system2("git", c("describe", "--tags", "--abbrev=0"),
                    stdout = TRUE, stderr = FALSE)

if (length(last_tag) == 0 || last_tag == "") {
  cli_alert_warning("No previous tag found. Using all commits.")
  commit_range <- "HEAD"
} else {
  cli_alert_info(paste("Comparing against:", last_tag))
  commit_range <- paste0(last_tag, "..HEAD")
}

# Get commits
commits <- system2("git",
                   c("log", commit_range, "--pretty=format:%s", "--no-merges"),
                   stdout = TRUE)

if (length(commits) == 0) {
  cli_alert_warning("No commits found since last release!")
  quit(save = "no", status = 1)
}

cli_alert_success(paste("Found", length(commits), "commits"))

# Categorize commits
categorize_commit <- function(msg) {
  msg_lower <- tolower(msg)

  # Check for conventional commit patterns
  if (grepl("^(feat|feature|add|new):", msg_lower)) {
    return("feature")
  } else if (grepl("^(fix|bug):", msg_lower)) {
    return("bugfix")
  } else if (grepl("^(break|breaking|!):", msg_lower)) {
    return("breaking")
  } else if (grepl("^(improve|enhance|perf):", msg_lower)) {
    return("improvement")
  } else if (grepl("^(docs|doc|documentation):", msg_lower)) {
    return("docs")
  } else if (grepl("^(test|tests):", msg_lower)) {
    return("test")
  } else if (grepl("^(refactor|chore|ci|build):", msg_lower)) {
    return("internal")
  }

  # Check for keywords in message
  if (grepl("(add|implement|introduce|new)", msg_lower)) {
    return("feature")
  } else if (grepl("(fix|resolve|correct|patch)", msg_lower)) {
    return("bugfix")
  } else if (grepl("(break|deprecate|remove.*function)", msg_lower)) {
    return("breaking")
  } else if (grepl("(improve|enhance|better|faster|optim)", msg_lower)) {
    return("improvement")
  } else if (grepl("(doc|vignette|readme|help)", msg_lower)) {
    return("docs")
  } else if (grepl("(test|spec|coverage)", msg_lower)) {
    return("test")
  } else if (grepl("(refactor|cleanup|reorganize)", msg_lower)) {
    return("internal")
  }

  return("other")
}

# Clean commit message
clean_commit <- function(msg) {
  # Remove conventional commit prefix
  msg <- sub("^(feat|feature|fix|bug|break|breaking|improve|enhance|perf|docs|doc|test|refactor|chore|ci|build):\\s*", "", msg, ignore.case = TRUE)

  # Capitalize first letter
  substr(msg, 1, 1) <- toupper(substr(msg, 1, 1))

  # Ensure it ends with proper punctuation
  if (!grepl("[.!?]$", msg)) {
    msg <- paste0(msg, ".")
  }

  # Convert to past tense if needed (simple heuristic)
  msg <- gsub("^Add ", "Added ", msg)
  msg <- gsub("^Fix ", "Fixed ", msg)
  msg <- gsub("^Implement ", "Implemented ", msg)
  msg <- gsub("^Improve ", "Improved ", msg)
  msg <- gsub("^Update ", "Updated ", msg)
  msg <- gsub("^Remove ", "Removed ", msg)
  msg <- gsub("^Refactor ", "Refactored ", msg)

  return(msg)
}

# Categorize all commits
commit_categories <- sapply(commits, categorize_commit)
commit_df <- data.frame(
  message = commits,
  category = commit_categories,
  cleaned = sapply(commits, clean_commit),
  stringsAsFactors = FALSE
)

# Print summary
cli_h2("Commit Summary")
cat("\n")
for (cat in unique(commit_df$category)) {
  count <- sum(commit_df$category == cat)
  cli_alert_info(sprintf("%s: %d", cat, count))
}

# Draft NEWS.md content
cli_h2("Drafting NEWS.md")

news_content <- paste0("# optic ", new_version, "\n\n")

# Add features
features <- commit_df[commit_df$category == "feature", "cleaned"]
if (length(features) > 0) {
  news_content <- paste0(news_content, "## New features\n\n")
  for (feat in features) {
    news_content <- paste0(news_content, "* ", feat, "\n")
  }
  news_content <- paste0(news_content, "\n")
}

# Add bug fixes
bugfixes <- commit_df[commit_df$category == "bugfix", "cleaned"]
if (length(bugfixes) > 0) {
  news_content <- paste0(news_content, "## Bug fixes\n\n")
  for (fix in bugfixes) {
    news_content <- paste0(news_content, "* ", fix, "\n")
  }
  news_content <- paste0(news_content, "\n")
}

# Add breaking changes
breaking <- commit_df[commit_df$category == "breaking", "cleaned"]
if (length(breaking) > 0) {
  news_content <- paste0(news_content, "## Breaking changes\n\n")
  for (brk in breaking) {
    news_content <- paste0(news_content, "* ", brk, "\n")
  }
  news_content <- paste0(news_content, "\n")
}

# Add improvements
improvements <- commit_df[commit_df$category == "improvement", "cleaned"]
if (length(improvements) > 0) {
  news_content <- paste0(news_content, "## Improvements\n\n")
  for (imp in improvements) {
    news_content <- paste0(news_content, "* ", imp, "\n")
  }
  news_content <- paste0(news_content, "\n")
}

# Add significant internal changes
internal <- commit_df[commit_df$category == "internal", "cleaned"]
if (length(internal) > 3) {  # Only if there are several
  news_content <- paste0(news_content, "## Internal\n\n")
  news_content <- paste0(news_content, "* Internal improvements and refactoring.\n\n")
}

# Draft GitHub release notes
cli_h2("Drafting GitHub Release Notes")

gh_notes <- paste0("## optic v", new_version, "\n\n")
gh_notes <- paste0(gh_notes, "### Summary\n\n")
gh_notes <- paste0(gh_notes, "[TODO: Add 2-3 sentence summary of this release]\n\n")

if (length(features) > 0) {
  gh_notes <- paste0(gh_notes, "### What's New\n\n")
  for (feat in features) {
    gh_notes <- paste0(gh_notes, "* ", feat, "\n")
  }
  gh_notes <- paste0(gh_notes, "\n")
}

if (length(bugfixes) > 0) {
  gh_notes <- paste0(gh_notes, "### Bug Fixes\n\n")
  for (fix in bugfixes) {
    gh_notes <- paste0(gh_notes, "* ", fix, "\n")
  }
  gh_notes <- paste0(gh_notes, "\n")
}

if (length(breaking) > 0) {
  gh_notes <- paste0(gh_notes, "### Breaking Changes\n\n")
  for (brk in breaking) {
    gh_notes <- paste0(gh_notes, "* ", brk, "\n")
  }
  gh_notes <- paste0(gh_notes, "\n")
}

gh_notes <- paste0(gh_notes, "## Installation\n\n")
gh_notes <- paste0(gh_notes, "```r\n")
gh_notes <- paste0(gh_notes, "# From GitHub\n")
gh_notes <- paste0(gh_notes, "remotes::install_github(\"randcorporation/optic@v", new_version, "\")\n")
gh_notes <- paste0(gh_notes, "```\n\n")

# Write files
cli_h2("Writing Draft Files")

# Write NEWS draft
news_draft_file <- "NEWS_DRAFT.md"
writeLines(news_content, news_draft_file)
cli_alert_success(paste("NEWS.md draft written to:", news_draft_file))

# Write GitHub release notes draft
gh_draft_file <- "RELEASE_NOTES_DRAFT.md"
writeLines(gh_notes, gh_draft_file)
cli_alert_success(paste("GitHub release notes draft written to:", gh_draft_file))

# Display content
cli_h2("Preview: NEWS.md Entry")
cli_rule()
cat(news_content)
cli_rule()

cat("\n")
cli_h2("Preview: GitHub Release Notes")
cli_rule()
cat(gh_notes)
cli_rule()

cat("\n")
cli_alert_info("Review and edit the draft files, then:")
cli_ul(c(
  paste0("Update NEWS.md with content from ", news_draft_file),
  paste0("Use ", gh_draft_file, " for GitHub release notes"),
  "Delete draft files when done"
))

cli_alert_success("Release notes drafted successfully!")
