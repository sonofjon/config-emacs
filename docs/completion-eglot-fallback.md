# Eglot Completion Fallback - Possible Solutions

## Problem Summary

When `eglot-completion-at-point` is the first item in
`completion-at-point-functions`, it returns a valid capf result even when
the LSP has no completions. This blocks other capfs (cape-dabbrev,
cape-dict, etc.) from running because the completion system uses eglot's
empty result instead of trying the next capf.

The `:exclusive nil` property doesn't help because it only triggers fallback
when the capf returns `nil` entirely, not when it returns an empty
completion table.

## Implementation Note

Capfs are added at different times:

1. Cape capfs are added via mode hooks (`text-mode-hook`, `prog-mode-hook`)
   which run when the major mode is set at buffer creation.

2. Eglot's capf is added later, when eglot connects to an LSP server.

The sequence for a markdown buffer:

1. Buffer created, `markdown-mode` activated
2. `text-mode-hook` runs (markdown derives from text-mode) ->
   `aj8/text-mode-capf` adds `cape-dabbrev+dict`
3. Eglot connects to LSP server
4. `eglot--managed-mode` runs -> prepends `eglot-completion-at-point`

Result: `(eglot-completion-at-point cape-dabbrev+dict t)`

Eglot uses `add-hook` without a depth argument, which prepends to the front.
So eglot always ends up first, regardless of what was already in the list.

This means `eglot-completion-at-point` cannot be referenced in static capf
definitions at init time (e.g., in a `defalias` with `cape-capf-super`).
The solutions require either global advice (which intercepts
`eglot-completion-at-point` wherever it is called) or
`eglot-managed-mode-hook` (to modify the capf list after eglot has added its
entry). See Solutions 3A and 3B.

## Background: Hook Management

The variable `completion-at-point-functions` is a hook - an ordered list of
functions that Emacs tries in sequence. Understanding how Emacs manages
hooks is essential for the solutions below.

### Two Ways to Modify Buffer-Local Hooks

1. `setq-local`: Direct assignment that bypasses the hook system
2. `add-hook` with local flag: Works with Emacs' hook depth system

The solutions in this document use `setq-local` for simplicity.

### The Hook Depth System

Emacs maintains ordering information in `hook--depth-alist`. When using
`add-hook`, a numeric depth can be specified - lower values run first. Emacs
uses this to maintain consistent ordering across hook modifications.

Items added via `setq-local` have no depth information. This becomes
problematic when a subsequent `add-hook` call occurs (e.g., when eglot
connects later), because Emacs may re-sort the list based on depths, moving
items without depth information to unexpected positions.

### The Re-sorting Problem

When using `setq-local` to set up the capf list, a later `add-hook` call can
trigger re-sorting based on depth information. Items that previously had
depths assigned (before the `setq-local`) retain those depths, causing them
to move unexpectedly.

Example: `text-mode-hook` uses `add-hook` with depth 10 to add
`ispell-completion-at-point`. Our capf function then uses `setq-local` to
set up the list as `(cape-dabbrev ispell-completion-at-point t)`. Later,
eglot connects and calls `add-hook` with depth 0 to add its capf. This
triggers re-sorting: eglot (depth 0) goes first, then items without depths
(cape-dabbrev, t), then ispell (depth 10) - resulting in
`(eglot-completion-at-point cape-dabbrev t ispell-completion-at-point)`.
Now ispell is after `t` and unreachable.

The fix is to clear the depth information after `setq-local`:

```elisp
(put 'completion-at-point-functions 'hook--depth-alist nil)
```

This prevents re-sorting, so subsequent `add-hook` calls simply prepend to
the front (for depth <= 0) or append to the end (for depth > 0) without
disrupting the existing order.

## Possible Solutions

### Solution 1. Reorder capf list: put eglot after cape sources

Cape sources run first, eglot runs after.

```elisp
(defun aj8/eglot-reorder-capf ()
  "Move eglot-completion-at-point after cape sources."
  (when (memq #'eglot-completion-at-point completion-at-point-functions)
    (setq-local completion-at-point-functions
                (append (remove #'eglot-completion-at-point
                                (remove t completion-at-point-functions))
                        (list #'eglot-completion-at-point t)))
    (put 'completion-at-point-functions 'hook--depth-alist nil)))
(add-hook 'eglot-managed-mode-hook #'aj8/eglot-reorder-capf)
```

Downside: eglot completions won't appear when cape sources have matches.

### Solution 2. Use `cape-capf-super` to merge eglot with other capfs

Combines candidates from multiple sources into one list.

```elisp
(defun aj8/eglot-combine-capf ()
  "Combine eglot-completion-at-point with cape-dabbrev+dict."
  (setq-local completion-at-point-functions
              (list (cape-capf-super #'eglot-completion-at-point
                                     #'cape-dabbrev
                                     #'cape-dict)
                    t))
  (put 'completion-at-point-functions 'hook--depth-alist nil))
(add-hook 'eglot-managed-mode-hook #'aj8/eglot-combine-capf)
```

Note: This is not a true fallback - it runs all capfs and merges results.
All candidates from eglot, dabbrev, and dict appear together in the popup.

### Solution 3. Use `cape-wrap-nonexclusive` to wrap eglot's capf

Makes eglot fall back to next capf when LSP returns no candidates.

**Option A - Hook-based wrapper:**
```elisp
(defun aj8/eglot-wrap-capf ()
  "Wrap eglot-completion-at-point with cape-wrap-nonexclusive."
  (setq-local completion-at-point-functions
              (mapcar (lambda (f)
                        (if (eq f #'eglot-completion-at-point)
                            (cape-capf-nonexclusive f)
                          f))
                      completion-at-point-functions))
  (put 'completion-at-point-functions 'hook--depth-alist nil))
(add-hook 'eglot-managed-mode-hook #'aj8/eglot-wrap-capf)
```

**Option B - Global advice:**
```elisp
(advice-add 'eglot-completion-at-point :around #'cape-wrap-nonexclusive)
```

Note: Eglot automatically adds `eglot-completion-at-point` to the
buffer-local `completion-at-point-functions` when it connects. Both options
handle all eglot-enabled modes (markdown, python, json, html, etc.) with a
single configuration.

### 4. Upgrade eglot to ELPA version

The ELPA version of eglot may have improvements to completion handling that
address the fallback issue.

```elisp
(package-install 'eglot)
```

Then restart Emacs to use the ELPA version instead of the built-in one.

## Solution Comparison

### Solution 1: Reorder capf list

Pros:
- Simple implementation

Cons:
- Defeats the purpose of having an LSP server - LSP completions never
  appear when other sources have matches
- Not a fallback mechanism - it's a priority reversal that sidelines LSP

### Solution 2: Merge with cape-capf-super

Pros:
- All completion sources contribute candidates simultaneously (this can also
  be a con)
- No priority conflicts - everything appears together
- Works for users who want to see all available completions at once

Cons:
- Creates noise - mixes high-quality LSP completions with matches from less
  relevant sources
- Not a true fallback - runs all capfs regardless of whether LSP has
  candidates

### Solution 3: Wrap with cape-wrap-nonexclusive

Pros:
- True fallback behavior - LSP runs first, other capfs only when LSP has
  nothing
- LSP completions get priority (correct behavior)

Cons: Hook-based approach (3A):
- Requires eglot-managed-mode-hook setup

Cons: Global advice approach (3B):
- Uses advice (some consider this less clean than direct configuration)

Why global advice (3B) is better than hook-based (3A):
- Single configuration point (one line)
- Clean separation of concerns - wraps function behavior without touching
  completion-at-point-functions

### Recommendation

Solution 3B (global advice) is the best approach:
- LSP completions get priority
- Automatic fallback to other capfs when LSP has no candidates
- Minimal configuration

---

## Alternative: Using add-hook Throughout

Instead of `setq-local` with depth-alist clearing, `add-hook` with explicit
depths can be used. This works with Emacs' hook system rather than bypassing
it:

```elisp
;; Example: reorder using add-hook with depths
(defun aj8/eglot-reorder-capf ()
  "Move eglot-completion-at-point after cape sources."
  (when (memq #'eglot-completion-at-point completion-at-point-functions)
    (remove-hook 'completion-at-point-functions #'eglot-completion-at-point t)
    (remove-hook 'completion-at-point-functions t t)
    (add-hook 'completion-at-point-functions #'eglot-completion-at-point 20 t)
    (add-hook 'completion-at-point-functions t 25 t)))
(add-hook 'eglot-managed-mode-hook #'aj8/eglot-reorder-capf)

;; Example: combine using add-hook with depths
(defun aj8/eglot-combine-capf ()
  "Combine eglot-completion-at-point with cape-dabbrev and cape-dict."
  (setq-local completion-at-point-functions nil)
  (add-hook 'completion-at-point-functions
            (cape-capf-super #'eglot-completion-at-point
                             #'cape-dabbrev
                             #'cape-dict)
            0 t)
  (add-hook 'completion-at-point-functions t 25 t))
(add-hook 'eglot-managed-mode-hook #'aj8/eglot-combine-capf)
```

This approach ensures correct ordering is maintained when packages add capfs
dynamically later.

### Comparison: setq-local vs add-hook

**setq-local approach** (used in main solutions):

Pros:
- Simpler to read and understand - shows the exact list being set

Cons:
- Bypasses Emacs' hook depth system rather than working with it
- Requires manual clearing of hook--depth-alist to prevent re-sorting
- Less robust when other packages modify capfs dynamically later

**add-hook approach** (shown in alternatives):

Pros:
- Works with Emacs' native hook system
- More robust when packages add capfs dynamically after initialization
- Explicit depth numbers make ordering intent clear

Cons:
- Slightly more code to and maintain (multiple remove-hook/add-hook calls)
- Requires tracking depth levels across multiple configuration points

Recommendation: The add-hook approach provides maximum robustness,
especially when working with packages that dynamically modify
completion-at-point-functions. The setq-local approach is preferable when
simplicity is the priority.
