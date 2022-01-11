kmacro-mc
=========

A set of Emacs commands to facilitate the usual multiple-cursors
workflows with the use of regular keyboard macros (kmacros).  This way
all the pitfalls of rolling a custom implementation are avoided, while
all the kmacro facilities, such as counters, queries and kmacro
editing, are gained virtually for free.

It is assumed the user didn't rebind the basic isearch commands,
otherwise the behavior may be unpredictable.

INSTALLATION
------------

```elisp
(use-package kmacro-mc
  :straight (:host github :repo "vifon/kmacro-mc.el")
  :bind ("C-c k" . kmacro-mc-region))
```
