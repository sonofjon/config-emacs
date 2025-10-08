# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working
with code in this repository.

## Repository Overview

This is a comprehensive Emacs configuration repository (`config-emacs`)
featuring `use-package` declarative configuration of built-in and third
party packages, as well as many custom utility functions.

## Key Architecture Components

### File Structure

- **`init.el`** (~3245 lines): Main configuration file with organized
  sections for packages and settings
- **`early-init.el`**: Pre-initialization settings including package archive configuration and GUI settings
- **`lisp/aj8-lisp.el`**: Custom utility functions (~146 function
  definitions)
- **`lisp/aj8-keys.el`**: Terminal-specific escape code mappings for
  different emulators
- **`lisp/aj8-hydra.el`**: Hydra menu definitions for markdown and window
  management

### Modular Structure

**init.el**: Main configuration organized in sections:
- STARTUP: Performance monitoring and startup time display
- EARLY SETTINGS: OS-specific conditional configuration
- PACKAGES: `use-package` setup and package declarations: Built-in packages
  first followed by third-party packages, organized by category (Admin,
  Buffers, Coding, Completion, Editing, Files, Help, Navigation, Outline,
  Search, Selection, Spelling, Terminal, Theme, Version control, Windows,
  Web, Other)
- LATE SETTINGS: Final configuration and cleanup

## Packages

### Packages Management

Packages are managed via two mechanisms:
1. **`package-selected-packages`**: Standard ELPA/MELPA packages installed
   with `package-install-selected-packages`
2. **`package-vc-selected-packages`**: Git-based packages installed with
   `package-vc-install-selected-packages`

**Important**: Use `use-package` ONLY for configuration. Packages are
installed via the selected packages lists, NOT via `:ensure` in
`use-package` declarations. Built-in packages explicitly use `:ensure nil`.

### Important packages

1. **Completion**: `Vertico` + `Orderless` + `Consult` + `Embark` +
   `Corfu` + `Cape`

2. **Coding**: `eglot` and `treesit` and `magit`

3. **AI*: `claude-code`, `gptel` and `gptel-toolkot`, 

## Key Conventions

**Namespace**: All custom functions/variables use `aj8/` prefix (e.g.,
`aj8/package-list-upgrades`, `aj8/my-os`)

## Development Workflows

**Adding custom functions**:
- Use `aj8/` namespace prefix for all custom functions and variables
- Add functions to `lisp/aj8-lisp.el` in the appropriate category section
  (Admin, Buffers, Coding, etc.)
- Reference functions in `init.el` configurations within the matching
  category

## Important Notes

- Custom file is separate: `custom-file.el` (loaded if exists)
- Desktop mode enabled
