kmacro-x
========

A collection of Emacs commands, modes and functions building on top of
the keyboard macros (kmacros) system.

INSTALLATION
------------

`kmacro-x` is available on MELPA.  Example configuration with
`use-package`:

```elisp
(use-package kmacro-x
  :ensure t
  :init (kmacro-x-atomic-undo-mode 1)
  :bind (("C-<" . kmacro-x-mc-mark-previous)
         ("C->" . kmacro-x-mc-mark-next)))
```

FEATURES
--------

**kmacro-x-atomic-undo-mode**

While this mode is enabled, the kmacro executions can be undo-ed
atomically, as a single operation.  Often the user doesn't want to
undo a single step of a kmacro but rather the kmacro as a whole.

**kmacro-x-mc**

Emulates the typical multiple-cursors workflow using kmacros.
This way all the pitfalls of rolling a custom implementation of
multiple-cursors are avoided, while all the kmacro facilities, such as
counters, queries and kmacro editing, are gained virtually for free.

Usage:

1. `kmacro-x-mc-mark-next` and `kmacro-x-mc-mark-previous` are used to
   find the next/previous occurrence of the symbol at point (or the
   region if active) and create a fake cursor/selection at
   its position.
2. Some arbitrary actions are being performed.
3. `RET` can be used to apply the actions to all the other cursors or
   `C-g` can be used to abort the bulk operation.

`kmacro-x-mc-atomic-undo-mode` can be enabled to make each
`macro-x-mc` change undoable as a whole, similarly to what
`kmacro-x-atomic-undo-mode` does to each cursor individually.

Notable settings, check their documentation for details:

- `kmacro-x-mc-live-preview` (default: `nil`)
- `kmacro-x-mc-mark-whole-symbol` (default: `nil`)
- `kmacro-x-mc-preserve-last-macro` (default: `t`)

Note for the existing users: The older implementation of the multiple
cursors provided by this package is still available in
`kmacro-x-mc-legacy.el`.  All the functions are the same as before,
just with the `legacy` infix added.
