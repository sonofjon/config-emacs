# Copilot Instructions for config-emacs

This file provides guidance to GitHub Copilot when working with code in this
repository.

## Repository Overview

This is a personalized cross-platform Emacs configuration. The configuration
is written in Emacs Lisp and features `use-package` declarative
configuration of built-in and third party packages, as well as many custom
utility functions.

## Architecture Overview

Multi-file startup sequence: `early-init.el` → `init.el` → custom
modules in `lisp/`

### File Structure

- **`early-init.el`**: Pre-initialization settings including package archive
  configuration, GUI settings and OS detection
- **`init.el`** (3000+ lines): Main configuration file with organized
  sections for packages and settings
- **`lisp/`**: This directory contains custom Emacs Lisp files, separating
    custom logic from the main `init.el` file
  - **`aj8-lisp.el`** (2000+ lines): Custom utility functions (100+
  function definitions)
  - **`aj8-keys.el`**: Terminal escape codes and key translations
  - **`aj8-hydra.el`**: Hydra menus for common workflows

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

**lisp/aj8-lisp.el**: Extensive utility library organized by the same
  categories as `init.el` (Admin, Buffers, Coding, Completion, Editing,
  Files, Help, Navigation, Outline, Search, Selection, Spelling, Terminal,
  Theme, Version control, Windows, Web, Other). Functions referenced in
  `init.el` configurations are found in the corresponding category in this
  file.

## Key Conventions

**Namespace**: All custom functions/variables use `aj8/` prefix (e.g.,
`aj8/package-list-upgrades`, `aj8/my-os`)

**Packages Management:** Packages are managed via two mechanisms:
1. **`package-selected-packages`**: Standard ELPA/MELPA packages installed
   with `package-install-selected-packages`
2. **`package-vc-selected-packages`**: Git-based packages installed with
   `package-vc-install-selected-packages`

**Important**: Use `use-package` ONLY for configuration. Packages are
installed via the selected packages lists, NOT via `:ensure` in
`use-package` declarations. Built-in packages explicitly use `:ensure nil`.

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

When working with this codebase, respect the `aj8/` namespace, declarative
package management approach, and categorical organization between files.
