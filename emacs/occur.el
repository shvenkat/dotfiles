

;; ----------  OCCUR MODE  ----------

;; Show lines matching a regex in a sidebar, which can be used for navigation.
;; See: https://emacs.stackexchange.com/a/50135

;; Make the *Occur* window open on the right side at 25% of the frame width.
(setq
    display-buffer-alist `(("\\*Occur\\*"
    display-buffer-in-side-window
    (side . right)
    (slot . 0)
    (window-width . 0.25))))

;; Key bindings in the *Occur* buffer.
(evil-set-initial-state 'occur-mode 'normal)
(evil-define-key 'normal occur-mode-map (kbd "q") 'quit-window)
(evil-define-key 'normal occur-mode-map (kbd "SPC") 'occur-mode-display-occurrence)
(evil-define-key 'normal occur-mode-map (kbd "RET")
    '(lambda ()
         (interactive)
         (occur-mode-goto-occurrence-other-window)
         (kill-buffer "*Occur*")))

;; ;; Make *Occur* window size to the contents
;; (setq fit-window-to-buffer-horizontally t)
;; (add-hook 'occur-hook
;;        (lambda ()
;;          (save-selected-window
;;            (pop-to-buffer "*Occur*")
;;            (fit-window-to-buffer))))

;; ;; Automatically switch to *Occur* buffer
;; (add-hook 'occur-hook
;;           '(lambda ()
;;              (switch-to-buffer-other-window "*Occur*")))
