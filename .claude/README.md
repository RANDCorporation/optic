# Claude Code Commands for optic

This directory contains custom slash commands for Claude Code.

## Available Commands

### `/release`

Start an AI-assisted release process for the optic package.

**Usage:**
```
/release
```

**What it does:**
- Analyzes git commits since last release
- Drafts release notes and NEWS.md entries
- Creates release branch using gitflow
- Updates version numbers and documentation
- Runs R CMD check
- Prepares everything for you to review and approve

**Learn more:**
- [Agent Release Guide](../.github/AGENT_RELEASE_GUIDE.md) - Detailed instructions for agents
- [Agent README](../.github/AGENT_README.md) - How to use agent-assisted releases
- [Release Guide](../.github/RELEASE_GUIDE.md) - Complete human-readable guide

## Creating Custom Commands

To add more commands, create `.md` files in this directory:

```markdown
---
description: Brief description of what this command does
---

Your command prompt goes here. This will be sent to Claude when the user
types /your-command-name
```

See [Claude Code documentation](https://docs.anthropic.com/claude-code) for more details.

## Tips

- Use `/release` when you've finished feature work and want to create a release
- The agent will ask clarifying questions before proceeding
- Review all drafted content before finalizing
- The agent won't merge or push - you maintain control

## Quick Start

1. Finish your feature work and merge to `develop`
2. Run `/release` in Claude Code
3. Answer the agent's questions
4. Review the drafted release notes
5. Approve and the agent will prepare the release branch
6. Review and finish with `git flow release finish X.Y.Z`
7. Push tags to trigger GitHub Actions

That's it! The release is automated from there.
