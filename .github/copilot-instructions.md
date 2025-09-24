# Copilot Instructions for config-emacs

This is a cross-platform Emacs configuration with custom utilities and
modular architecture.

## Architecture Overview

**Multi-file startup sequence**: `early-init.el` → `init.el` → custom
modules in `lisp/`
- `early-init.el`: OS detection, performance optimizations, package sources
  setup
- `init.el`: Main configuration with `package-selected-packages` and
  `use-package` declarations
- `lisp/aj8-*.el`: Custom utility modules loaded via `require`

## Key Conventions

**Namespace**: All custom functions/variables use `aj8/` prefix (e.g.,
`aj8/package-list-upgrades`, `aj8/my-os`)

**Package management**:
- `package-selected-packages` defines all packages for installation
- `use-package` is strictly for configuration (never uses `:ensure`)
- Install packages with `package-install-selected-packages`
- VC packages installed with `package-vc-install` and managed separately

## Modular Structure

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

**lisp/aj8-keys.el**: Terminal escape codes and key translations
**lisp/aj8-hydra.el**: Hydra menus for common workflows

## Development Workflows

**Adding custom functions**:
- Use `aj8/` namespace prefix for all custom functions and variables
- Add functions to `lisp/aj8-lisp.el` in the appropriate category section
  (Admin, Buffers, Coding, etc.)
- Reference functions in `init.el` configurations within the matching
  category


When working with this codebase, respect the `aj8/` namespace, declarative
package management approach, and categorical organization between files.
