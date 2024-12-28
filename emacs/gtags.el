

;; ----------  GNU GLOBAL/GTAGS  ----------

;; Jump to definition/references using GNU global/gtags.
(use-package ggtags
    :defer t
    :hook (python-mode . ggtags-mode)
    :config
    (setq
        ggtags-highlight-tag 0.1
        ;; Use ido fuzzy matching for gtags-based completion.
        ;; See: https://github.com/leoliu/ggtags/issues/56#issuecomment-43165031
        ggtags-completing-read-function
            (lambda (&rest args)
                (apply #'ido-completing-read
                    (car args)
                    (all-completions "" ggtags-completion-table)
                    (cddr args))))
    (evil-leader/set-key "g" 'ggtags-find-file)
    ;; ggtags keymap should take precedence over other evil mode keymaps.
    (evil-make-overriding-map ggtags-mode-map 'normal)
    ;; Force update evil keymaps after ggtags-mode is loaded.
    (add-hook 'ggtags-mode-hook #'evil-normalize-keymaps))
;; (eval-after-load 'ggtags
;;   '(progn
;;      ;; ggtags keymap should take precedence over other evil mode keymaps.
;;      (evil-make-overriding-map ggtags-mode-map 'normal)
;;      ;; Force update evil keymaps after ggtags-mode is loaded.
;;      (add-hook 'ggtags-mode-hook #'evil-normalize-keymaps)
;;      ;; Enable ggtags for all Python files.
;;      (add-hook 'python-mode-hook 'ggtags-mode)))
