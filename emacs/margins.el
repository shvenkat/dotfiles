

;; ----------  MARGINS  ----------

;; Center the text area by setting a suitably wide left margin. The right margin
;; is NOT set as this would cause long lines to be wrapped.
;; https://stackoverflow.com/a/23731757
;; Add margins, when file is markdown or text.
(defun center-window (window) ""
    (let* ((max-text-width fill-column)
           (margin (max 0 (/ (- (window-total-width window) max-text-width) 2))))
        (set-window-margins window margin)
        (set-window-fringes window 0 0)
        (set-window-scroll-bars window 0 nil 0 nil)))
;; Adjust margins of all windows.
(defun center-windows () ""
    (walk-windows (lambda (window) (center-window window)) nil 1))
;; Listen to window changes.
(add-hook 'window-configuration-change-hook 'center-windows)

;; Use a straight line without gaps as the vertical window border.
;; Set the border drawing character.
(defun set-window-divider ()
    (let ((display-table (or buffer-display-table standard-display-table)))
        (set-display-table-slot display-table 5 ?│)
        (set-window-display-table (selected-window) display-table)))
;; Apply setting whenever the window configuration changes.
(add-hook 'window-configuration-change-hook 'set-window-divider)

;; Set a window to be snug i.e. just wide enough.
(defun set-window-snug-helper ()
    (interactive)
    (evil-window-set-width (+ fill-column 1))
    (set-window-margins (selected-window) 1 1)
    (set-window-fringes (selected-window) 0 0)
    (set-window-scroll-bars (selected-window) 0 nil 0 nil))
;; For some reason, the above settings have to be applied twice in succession to
;; "converge". Better calculation may remove the need for this hack.
(defun set-window-snug ()
    (interactive)
    (set-window-snug-helper)
    (set-window-snug-helper))

;; Maximize the desktop window, if running a GUI.
(if (display-graphic-p)
    (add-to-list 'initial-frame-alist '(fullscreen . maximized)))
