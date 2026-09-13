- [ ] `C-x <left>` and `<right>` should go to previous and next buffer
      based on recency
- [ ] Stop `agent-shell-manager` from accumulating refresh timers. The
      mode stores its 2 second timer in a buffer-local variable so it can
      cancel it later, but `define-derived-mode` runs
      `kill-all-local-variables` before the mode body, which clears that
      variable while the timer itself keeps running in `timer-list`. The
      `cancel-timer` check therefore sees nothing to cancel, and every
      activation adds another timer. The problem predates upstream PR #10,
      which attempted this fix and has no effect for the same reason.
      Either set `permanent-local` on the variable locally, or fix it
      upstream and submit a PR.
- [x] Fix the manager opener in `agent-shell-mode-hook`. It calls
      `agent-shell-manager-toggle`, which hides the manager when it is
      already visible, so creating a second agent shell closes it.
      Show the manager only when it is not displayed instead. The comment
      above the hook also says first agent-shell buffer, while the hook
      runs for every one.
- [x] Quote code identifiers with backticks in the Markdown files, following
      the convention applied in the `tdm.anonymization.python` project
      (commit b6eb225, "TODO.md: Quote code identifiers with backticks",
      with its `TODO.md` as the worked example). Filenames, paths,
      functions, classes, variables and constants are quoted; field and
      column names stay unquoted in any case style. `docs/completion.md`
      and `data/directive.md` carry most of the unquoted identifiers.
- [ ] Switch gptel chat buffers from `text-mode` to `markdown-ts-mode`
      once gptel supports it. `gptel-prompt-prefix-alist` and
      `gptel-response-prefix-alist` are looked up with `(alist-get
      major-mode ...)`, and `gptel--parse-media-links` dispatches on
      `(eql 'markdown-mode)`, so neither reaches `markdown-ts-mode`. Also
      update the `pcase` in `aj8/gptel-write-buffer`.
- [x] Conform all hook registration to the convention: use `:hook` in
      either of two cases -- hooking this block's own function onto a
      foreign mode (`abbrev` puts `abbrev-mode` on `text-mode`;
      `flymake` puts `flymake-mode` on `emacs-lisp-mode`), or hooking
      any function onto this block's own mode (`info` puts
      `rename-uniquely` on its own `Info-mode`). Use `add-hook` in
      `:config` (or `:init`) for everything else, and always when a
      non-default depth/append or a buffer-local hook is needed, since
      `:hook` cannot express those. Result: audited every `:hook` in
      `init.el` -- all of them already fit this convention, so no code
      changes were needed.

