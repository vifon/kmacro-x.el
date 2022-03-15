kmacro-x
========

A collection of commands, modes and functions building on top of the
keyboard macros (kmacros) system.

INSTALLATION
------------

Installation with `straight.el`:

```elisp
(use-package kmacro-x
  :straight (:host github :repo "vifon/kmacro-x.el")
  :init (kmacro-x-atomic-undo-mode 1)
  :bind ("C-c k" . kmacro-x-mc-region))
```

Coming soon to MELPA.

FEATURES
--------

**kmacro-x-atomic-undo-mode**

While this mode is enabled, the kmacro executions can be undo-ed
atomically, as a single operation.  Often the user doesn't want to
undo a single step of a kmacro but rather the kmacro as a whole.

**kmacro-x-mc-region**

Emulates the typical multiple-cursors workflow using kmacros.
This way all the pitfalls of rolling a custom implementation of
multiple-cursors are avoided, while all the kmacro facilities, such as
counters, queries and kmacro editing, are gained virtually for free.

It is assumed the user didn't rebind the basic isearch commands,
otherwise the behavior may be unpredictable.

A typical workflow with `kmacro-x-mc-region`:

1. Select the text whose occurences are to be manipulated (in
   a trivial case: a symbol to be renamed).
2. <kbd>M-x kmacro-x-mc-region RET</kbd>
3. Do the necessary edits, either within the region or in its vicinity
   outside of it (this is the part that cannot be easily achieved with
   other mc alternatives such as iedit or query-replace).  They will
   get recorded as a kmacro.
4. Press any key that would end the kmacro recording:
   <kbd>F4</kbd>, <kbd>C-x )</kbd> or <kbd>C-x C-k C-k</kbd>
5. Repeat the kmacro with <kbd>F4</kbd>, <kbd>C-x e</kbd> or
   <kbd>C-x C-k C-k</kbd>.
