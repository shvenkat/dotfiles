

;; ----------  VIM KEY-BINDINGS  ----------

;; Use evil for vim-like keybindings.
(use-package evil
    :demand
    :config
    (evil-mode 1))

(use-package undo-fu
    :demand
    :config
    (evil-set-undo-system 'undo-fu))

;; Use a configurable prefix key to add custom keybindings.
;; https://github.com/cofi/evil-leader
;; Load evil-leader immediately to allow custom key bindings in init.el.
(use-package evil-leader
    :demand
    :config
    (global-evil-leader-mode)
    (evil-leader/set-leader ","))
(evil-leader/set-key "," 'evil-repeat-find-char-reverse)

;; Use an emacs port of the vim commentary plugin. This provides normal mode
;; bindings to toggle comments, such as:
;;     gcc   comment/uncomment current line.
;;     gc    comment/uncomment visual line selection.
;;     2gcj  comment/uncomment current line and the next two.
(use-package evil-commentary
    :config
    (evil-commentary-mode))

;; Use evil bindings for Org mode.
;; There is a choice between org-evil (less actively maintained) and evil-org.
(use-package evil-org
    :ensure t
    :after org
    :hook (org-mode . evil-org-mode)
    :config
    (evil-org-set-key-theme '(navigation insert textobjects additional calendar
                                 shift todo heading)))
    ;; (require 'evil-org-agenda)
    ;; (evil-org-agenda-set-keys))


;; Reconcile evil-org and org-agenda config:
;; https://github.com/Somelauw/evil-org-mode/issues/108
