# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

poly-translate is an Emacs Lisp translation library that provides a plugin-based architecture for translating text using multiple translation backends including API-based services (Google Translate, DeepL) and LLM-based services (OpenAI, Anthropic Claude, Gemini).

## Key Requirements

- **Emacs Version**: 30.1+ (use latest Emacs features)
- **License**: GPL v3 (ensure all dependencies are compatible)
- **LLM Integration**: Use `gptel` library (https://github.com/karthink/gptel) for LLM connections

## Architecture

### Translation Engine System
- Plugin-based architecture for easy addition of new backends
- Each engine defines:
  - Name
  - Backend type
  - Input language ("auto" for auto-detection, or language codes like "ja", "en")
  - Output language (language codes only, no "auto")
  - Backend-specific configuration (API keys, etc.)

### Supported Backends
1. **API-based**:
   - Google Translate API
   - DeepL API
2. **LLM-based** (via gptel):
   - OpenAI API
   - Anthropic (Claude) API
   - Gemini API

### User Interface
- Commands for translating selected regions
- Commands for translating entire buffers
- Option to copy translation to kill-ring
- Display translations in dedicated buffer

## Development Commands

Since this is a new Emacs Lisp project, common development tasks include:

```bash
# Run Emacs with the library loaded
emacs -Q -L . --eval "(require 'poly-translate)"

# Byte compile
emacs -Q -batch -L . -f batch-byte-compile poly-translate.el

# Run tests (when implemented)
emacs -Q -batch -L . -l ert -l poly-translate-test.el -f ert-run-tests-batch-and-exit
```

## Code Structure

The project should follow standard Emacs Lisp package conventions:
- Main library file: `poly-translate.el`
- Backend implementations: `poly-translate-backend-*.el`
- User interface: Functions prefixed with `poly-translate-`
- Configuration: Use defcustom for user-configurable options

## Implementation References

Consider these existing implementations for design patterns:
- https://github.com/lorniu/go-translate
- https://github.com/Elilif/emacs-immersive-translate

## File Format Standards

### Trailing Newlines
**IMPORTANT**: All text files MUST end with a newline character. This includes:
- All `.el` (Emacs Lisp) files
- All `.yml`/`.yaml` (YAML) files
- All `.md` (Markdown) files
- All configuration files
- All documentation files

This follows POSIX standards and prevents issues with:
- Git operations and diffs
- Text processing tools
- Editor compatibility
- Makefile processing

When creating or editing files, always ensure they end with a newline character.

### Whitespace Standards
**IMPORTANT**: Keep files clean from unnecessary whitespace:

#### Trailing Whitespace
- **NO trailing spaces** at the end of any line
- **NO spaces in empty lines** - empty lines should contain only the newline character
- Trailing whitespace causes issues with:
  - Git diffs showing unnecessary changes
  - Editor warnings and linting tools
  - Inconsistent file formatting
  - Version control noise

#### Line Endings
- Use Unix-style line endings (`\n`) consistently
- Avoid Windows-style line endings (`\r\n`) or old Mac-style (`\r`)

When creating or editing files, always remove trailing whitespace and ensure empty lines contain no spaces.

## Development Workflow

### Branch and Pull Request Policy
**IMPORTANT**: All fixes and features MUST follow proper branch workflow:

- **NEVER commit directly to main branch** for fixes or features
- Always create feature branches for any changes:
  ```bash
  git checkout -b feature-name
  # Make changes
  git add .
  git commit -m "description"
  git push -u origin feature-name
  ```
- Always create pull requests for code review before merging
- Use descriptive branch names that reflect the change being made
- Examples of good branch names:
  - `fix-ci-errors`
  - `improve-translation-layout`
  - `add-deepl-backend`
  - `update-documentation`

This workflow ensures:
- Code review and quality control
- Proper change tracking
- Safe integration of changes
- Rollback capability if needed

## Important Instructions

Do what has been asked; nothing more, nothing less.
NEVER create files unless they're absolutely necessary for achieving your goal.
ALWAYS prefer editing an existing file to creating a new one.
NEVER proactively create documentation files (*.md) or README files. Only create documentation files if explicitly requested by the User.
