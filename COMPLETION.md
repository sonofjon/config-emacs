# Emacs Completion System - Comprehensive Guide

This document describes how the Emacs completion system works and how it is
configured in this repository.

## Table of Contents

1. [Architecture](#architecture)
2. [Configuration Scopes](#configuration-scopes)
3. [Completion Styles](#completion-styles)
4. [Completion Sources by Context](#completion-sources-by-context)
5. [Customization Strategy](#customization-strategy)
6. [Interactive Controls](#interactive-controls)
7. [Quick Reference](#quick-reference)
8. [Related Files](#related-files)

## 1. Architecture

The Emacs completion system operates at three distinct levels:

```
[Completion Sources] -> [Completion Styles] -> [Frontends]
     (Backend)              (Filtering)         (Display)
```

Each level has a specific responsibility and they work together to provide
completion.

### 1.1. Level 1: Completion Sources (Backends)

Completion sources provide the raw candidate lists. Each source typically
tags its candidates with a category.

**Buffer completion sources** (via `completion-at-point-functions`):
- **elisp-completion-at-point**: Emacs Lisp symbols (built-in, only in
  emacs-lisp-mode, no category)
- **eglot**: LSP completions (category: eglot)
- **cape-file**: File paths (category: file)
- **cape-dabbrev**: Dynamic abbreviations from buffers (no category)
- **cape-dict**: Dictionary words (no category)
- **cape-elisp-symbol**: Emacs Lisp symbols (no category)
- **cape-keyword**: Programming keywords (no category)
- **cape-abbrev, cape-history, cape-line, cape-tex**: Available via
  keybindings but not automatically active

**Minibuffer completion sources** (command-specific):
- **Commands**: M-x (category: command)
- **Buffers**: C-x b (category: buffer)
- **Files**: C-x C-f (category: file)
- **Variables**: C-h v (category: variable)
- **Functions**: C-h f (category: function)
- **Consult commands**: consult-line, consult-grep, etc.

**Other sources**:
- **dabbrev**: Dynamic abbreviations (accessed via hippie-expand, uses its
  own matching, not part of standard completion)

### 1.2. Level 2: Completion Styles (Filtering)

Completion styles determine how the input pattern filters the candidates.

**Style resolution order** (highest to lowest priority):
1. `completion-category-overrides` - Per-category style settings
2. `completion-category-defaults` - Default styles for categories
3. `completion-styles` - Global fallback

**Within orderless**: Additional configuration options:
- `orderless-matching-styles` - How each space-separated component is
  matched
- `orderless-style-dispatchers` - Per-component overrides based on suffix
  characters

### 1.3. Level 3: Frontends (Display)

Frontends display the filtered candidates. They do not control matching or
filtering.

**Frontends used in this configuration**:
- **Corfu**: In-buffer popup overlay (displays completion-at-point results)
- **Vertico**: Minibuffer vertical list (displays minibuffer completion
  results)

**Important**: Frontends are style-agnostic. They display whatever
candidates the completion system produces after filtering.

## 2. Configuration Scopes

This section describes three scopes at which completion can be configured:
per-category, globally, and per-frontend.

**Category scope** - Per-category overrides:
```elisp
(completion-category-overrides '((eglot (styles orderless))
                                 (file (styles basic partial-completion))))
```
- Based on data type (what you're completing)
- Clean separation from where completion appears

**Global scope** - Base configuration:
```elisp
(completion-styles '(orderless))
(orderless-matching-styles '(orderless-literal orderless-regexp))
```
- Applies to all completion unless overridden by category

**Frontend scope** - Non-standard (requires custom implementation):
```elisp
;; Set buffer-local styles based on UI context
(add-hook 'corfu-mode-hook
  (lambda ()
    (setq-local completion-styles '(basic))
    (setq-local orderless-matching-styles '(orderless-literal))))
(add-hook 'minibuffer-setup-hook
  (lambda ()
    (setq-local completion-styles '(orderless))
    (setq-local orderless-matching-styles '(orderless-literal orderless-regexp))))
```
- Requires hooks and buffer-local variables
- Configures where completion appears, not what is being completed

## 3. Completion Styles

This section details the completion style settings (Level 2 from Section 1).

### 3.1. Global Setting

```elisp
(completion-styles '(orderless))
```
This applies orderless filtering to all completion sources unless overridden
by category-specific settings.

Note: No fallback style (like `basic`) is included. Empty results provide
clear feedback to refine the pattern, unlike fallback styles that show
irrelevant candidates.

**Location**: init.el: minibuffer section

### 3.2. Category-Specific Overrides

**eglot category** (init.el: eglot section):
```elisp
(completion-category-overrides '((eglot (styles orderless))))
```
- Current configuration: uses orderless only
- Note: Redundant with current global setting, but kept defensively

**file category** (init.el: minibuffer section):
```elisp
(completion-category-overrides '((file (styles basic partial-completion))))
```
- Current configuration:
  - Uses basic (prefix matching) + partial-completion
  - Does not use orderless
- Rationale: File paths benefit from simple prefix and partial matching

### 3.3. Orderless Configuration

**Location**: init.el: orderless section

```elisp
(orderless-matching-styles '(orderless-literal orderless-prefixes))
```
- Current configuration:
  - Each space-separated component in the input is matched as a literal
    substring, or, a prefix
  - **Note**: This global default is upgraded to include `orderless-regexp`
    in the Minibuffer (see [Section 5](#customization-strategy)).

## 4. Completion Sources by Context

### 4.1. Corfu Context (In-Buffer Completion)

Triggered by: TAB or completion-at-point in a buffer
Frontend: Corfu popup overlay

**Note**: Corfu displays completion-at-point candidates, which means it uses
Cape sources and other completion-at-point-functions. Corfu primarily works
in regular buffers, but can also be enabled in the minibuffer when Vertico
is not active (via corfu-enable-always-in-minibuffer in init.el: corfu
section). In those cases (like eval-expression), completion-at-point sources
become available in the minibuffer.

**In prog-mode buffers** (init.el: cape section):
```elisp
completion-at-point-functions:
1. cape-symbol+keyword+dict (orderless)
   - cape-elisp-symbol
   - cape-keyword
   - cape-dict
2. cape-file (basic + partial-completion)
3. eglot (orderless) - if LSP server active
```

**In text-mode buffers** (init.el: cape section):
```elisp
completion-at-point-functions:
1. cape-dabbrev+dict (orderless)
   - cape-dabbrev
   - cape-dict
2. cape-file (basic + partial-completion)
```

**In LLM chat modes** (init.el: cape section):
```elisp
completion-at-point-functions:
1. cape-dabbrev+dict (orderless)
   - cape-dabbrev
   - cape-dict
2. cape-file (basic + partial-completion)
```

Applies to:
- agent-shell-mode (Agent Shell)
- gptel-mode (GPTel and gptel-agent)
- claude-code-start-hook (Claude Code - uses eat-mode or vterm-mode)

Note: Regular terminal buffers (comint, eat, vterm) that are NOT LLM chat
sessions only get cape-file completion.

**Global (all other buffers)** (init.el: cape section):
```elisp
completion-at-point-functions includes:
- cape-file (basic + partial-completion)
```

This applies to buffers not covered above, such as help-mode, dired-mode,
compilation-mode, and regular terminal sessions.

### 4.2. Vertico Context (Minibuffer Completion)

Triggered by: M-x, C-x C-f, C-x b, consult commands, etc.
Frontend: Vertico vertical list

**Sources depend on command**, not on major mode:
- M-x -> command category (orderless)
- C-x C-f -> file category (basic + partial-completion)
- C-x b -> buffer category (orderless)
- consult-line -> consult-location category (orderless)
- etc.

### 4.3. Key Differences

| Aspect | Corfu | Vertico |
|--------|-------|---------|
| **Location** | In-buffer popup | Minibuffer |
| **Trigger** | TAB, completion-at-point | M-x, C-x C-f, C-x b, etc. |
| **Sources configured via** | Mode hooks (`completion-at-point-functions`) | Command invoked |
| **Mode-dependent?** | Yes (prog-mode vs text-mode) | No (depends on command) |
| **Typical sources** | eglot, cape-*, LSP | commands, files, buffers |

## 5. Customization Strategy

This configuration employs a "Global Simple + Minibuffer Upgrade" strategy to
ensure simple completion behavior in buffers while retaining full power in the
minibuffer.

**Global Default (Simple)**

The global default for `orderless-matching-styles` is set to:
```elisp
(orderless-literal orderless-prefixes)
```

**Rationale**:
- **Safety**: Typing punctuation (e.g., `*`, `.`) in a code buffer is usually
  intended as syntax, not regex patterns.
- **Predictability**: Matches are literal or prefix-based, which feels more
  natural for in-buffer completion (Corfu).

**Minibuffer Upgrade (Power)**

A hook on `minibuffer-setup-hook` locally upgrades the style to:
```elisp
(orderless-literal orderless-regexp)
```

**Rationale**:
- **Power**: When using the minibuffer (M-x, Find File, etc.), regex is a
  powerful filtering tool.
- **Context**: The Minibuffer is an interactive query interface where regex
  is expected and useful.

**Implementation Details**:
```elisp
;; init.el: orderless section
(orderless-matching-styles '(orderless-literal orderless-prefixes))

;; init.el: orderless section (:config)
(defun aj8/minibuffer-enable-orderless-regexp ()
  (setq-local orderless-matching-styles '(orderless-literal orderless-regexp)))
(add-hook 'minibuffer-setup-hook #'aj8/minibuffer-enable-orderless-regexp)
```

## 6. Interactive Controls

This section describes interactive commands that dynamically modify completion
behavior during a completion session.

### 6.1. Orderless Style Dispatchers

Suffix-based overrides that apply to individual search terms, allowing
per-term control over matching behavior.

| Suffix | Style | Example | Behavior |
|--------|-------|---------|----------|
| `~` | flex | `foo~` | Matches f-o-o with characters in order |
| `=` | literal | `foo=` | Forces exact substring "foo" |
| `<` | prefixes | `foo<` | Matches items starting with "foo" |
| `*` | regexp | `f.*o*` | Forces regexp interpretation |
| `!` | exclude | `test!` | Excludes items containing "test" |

**Functions**:
- `aj8/orderless-dispatch-flex-if-twiddle`
- `aj8/orderless-dispatch-literal-if-equal`
- `aj8/orderless-dispatch-prefixes-if-less`
- `aj8/orderless-dispatch-regexp-if-star`
- `aj8/orderless-dispatch-without-if-bang`

**Location**: lisp/aj8-lisp.el: completion section

### 6.2. Orderless Matching Style Cycling

Cycles through orderless matching styles on the fly, temporarily changing
how pattern matching works during completion without modifying the
configuration: literal -> prefixes -> regexp -> flex -> literal.

**Function**: `aj8/orderless-matching-style-cycle` (keybinding: M-o)

**Location**: lisp/aj8-lisp.el: completion section

### 6.3. Vertico Sort Order Toggling

Cycles through different sort orders for completion candidates in the
minibuffer: history-length-alpha (default) -> length-alpha -> alpha ->
history-length-alpha.

**Function**: `aj8/vertico-sort-toggle` (keybinding: M-/)

**Location**: lisp/aj8-lisp.el: completion section

## 7. Quick Reference

### 7.1. Matching Style Decision Flow

#### 7.1.1. Corfu Context (In-Buffer Completion)

```
User presses TAB or triggers completion-at-point
    |
    v
Sources from completion-at-point-functions consulted in order
    |
    v
First source to return candidates wins
    |
    v
What category (if any) did that source tag candidates with?
    |
    v
Is there a category override for this category?
    |
    +---> YES: Use override styles
    |
    +---> NO: Use global completion-styles
```

#### 7.1.2. Vertico Context (Minibuffer Completion)

```
User invokes command (M-x, C-x C-f, C-x b, etc.)
    |
    v
Command uses built-in completion function (completing-read, read-file-name, etc.)
    |
    v
Completion function provides candidates with a category
    |
    v
Is there a category override for this category?
    |
    +---> YES: Use override styles
    |
    +---> NO: Use global completion-styles
```

### 7.2. Example Scenarios

**Scenario 1**: Typing in a Python file (prog-mode)

1. Press TAB
2. Corfu triggers completion-at-point
3. Sources consulted:
   - eglot (provides Python symbols) -> eglot category -> orderless
   - cape-symbol+keyword+dict -> no category -> orderless
   - cape-file (if path detected) -> file category -> basic + partial-completion
4. Candidates filtered by appropriate styles
5. Corfu displays results

**Scenario 2**: Pressing M-x

1. M-x invoked
2. Vertico shows minibuffer
3. Source: command completion -> command category -> orderless (global)
4. Type input with space-separated components
5. Vertico displays filtered commands

**Scenario 3**: Finding a file (C-x C-f)

1. C-x C-f invoked
2. Vertico shows minibuffer
3. Source: file completion -> file category -> basic + partial-completion
4. Input matched as prefix/partial (NOT orderless)
5. Vertico displays matching file paths

## 8. Related Files

- `init.el: minibuffer section` - Global completion configuration
- `init.el: eglot section` - eglot configuration
- `init.el: corfu section` - Corfu configuration
- `init.el: cape section` - Cape configuration
- `init.el: orderless section` - orderless configuration
- `init.el: vertico section` - Vertico configuration
- `lisp/aj8-lisp.el: completion section` - orderless and Vertico custom functions
