

;; ----------  MENU / MODE BAR  ----------

;; Disable top menu bar.
(menu-bar-mode -1)

;; Use a simple modeline:
;;   left: filename
;;   right: errors, file position and mode.
;; https://emacs.stackexchange.com/a/7542
;; http://www.hollenback.net/index.php/EmacsModeLine
(defun justify-mode-line (left right)
    "Pad left and right strings with spaces to a total length of window-total-width."
    (let* ((available-width (- (window-total-width) (length left) 2)))
        (format (format "%%s %%%ds" available-width) left right)))
(setq-default mode-line-format
  '((:eval (justify-mode-line
                (format-mode-line " %f%*")
                (concat
                    (if (featurep 'flycheck)
                        (flycheck-mode-line-status-text)
                        (""))
                    (format-mode-line " %l:%c  %m "))))))
