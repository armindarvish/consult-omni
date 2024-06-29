---
name: Bug report
about: Create a report to help us improve
title: ''
labels: ''
assignees: ''

---

**Describe the bug**
A clear and concise description of what the bug is.

**To Reproduce**
Steps to reproduce the behavior:
1. Go to '...'
2. Click on '....'
3. Scroll down to '....'
4. See error

**Expected behavior**
A clear and concise description of what you expected to happen.

**Screenshots**
If applicable, add screenshots to help explain your problem.

**Important Information:**
 - OS: [e.g. macOS]
 - Version of Emacs [e.g. 29] (or other Emacsen you use)
 - Version of `consult` (see [pkg-info](https://github.com/emacsorphanage/pkg-info))
 - The installation method and the configuration you are using with your consult-omni.
 - If there is an error message, turn debug-on-error on (by `M-x toggle-debug-on-error`) and include the backtrace content in your report.
 - Optionally include log information, **but make sure to remove personal informaiton and secrets (e.g. API keys)**. To see the log, first set the variable `consult-omni-log-level` to `debug` and then look ta the content of ` *consult-omni-log*` (note the space at the beginning) or some other buffer name defined in `consult-omni-log-buffer-name`.
 - If the error only exists when you have some other packages installed, list those packages (e.g. problem happens when evil is installed)

**Additional context**
Add any other context about the problem here.
