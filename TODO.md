- [ ] C-x <left> and <right> should go to previous and next buffer based on
      recency
- [ ] Stop agent-shell-manager from accumulating refresh timers. The mode
      stores its 2 second timer in a buffer-local variable so it can cancel
      it later, but define-derived-mode runs kill-all-local-variables before
      the mode body, which clears that variable while the timer itself keeps
      running in timer-list. The cancel-timer check therefore sees nothing
      to cancel, and every activation adds another timer. The problem
      predates upstream PR #10, which attempted this fix and has no effect
      for the same reason. Either set permanent-local on the variable
      locally, or fix it upstream and submit a PR.
- [x] Fix the manager opener in agent-shell-mode-hook. It calls
      agent-shell-manager-toggle, which hides the manager when it is already
      visible, so creating a second agent shell closes it. Show the manager
      only when it is not displayed instead. The comment above the hook also
      says first agent-shell buffer, while the hook runs for every one.
