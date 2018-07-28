

;; ----------  VIM KEY-BINDINGS  ----------

;; Use evil for vim-like keybindings.
(use-package evil
    :config
    (evil-mode 1))

;; Use a configurable prefix key to add custom keybindings.
;; https://github.com/cofi/evil-leader
(use-package evil-leader
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ","))
;; Load evil-leader to allow custom key bindings to be set in init.el.
(require 'evil-leader)

;; Use an emacs port of the vim commentary plugin. This provides normal mode
;; bindings to toggle comments, such as:
;;     gcc   comment/uncomment current line.
;;     gc    comment/uncomment visual line selection.
;;     2gcj  comment/uncomment current line and the next two.
(use-package evil-commentary
    :config
    (evil-commentary-mode))

;; Use evil bindings for Org mode.
(use-package org-evil
    :after org)
