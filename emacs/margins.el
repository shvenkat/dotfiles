

;; ----------  MARGINS  ----------

(defun center-window (window)
    "Center the text area in WINDOW by setting a suitable left margin width.

    The right margin is NOT set as this would cause long lines to be wrapped.
    See https://stackoverflow.com/a/23731757. Fringes and scroll bars are
    removed."
    (let* ((max-text-width fill-column)
           (margin (max 0 (/ (- (window-total-width window) max-text-width) 2))))
        (set-window-margins window margin)
        (set-window-fringes window 0 0)
        (set-window-scroll-bars window 0 nil 0 nil)))

(defun center-windows ()
    "Center the text in each window."
    (walk-windows (lambda (window) (center-window window)) nil 1))

;; Listen to window changes.
(add-hook 'window-configuration-change-hook 'center-windows)

(defun set-window-divider ()
    "Use a straight line without gaps as the vertical window border."
    (let ((display-table (or buffer-display-table standard-display-table)))
        (set-display-table-slot display-table 5 ?│)
        (set-window-display-table (selected-window) display-table)))

;; Apply setting whenever the window configuration changes.
(add-hook 'window-configuration-change-hook 'set-window-divider)

(defun set-window-snug-helper ()
    "Set a window to be snug i.e. with a 1 space margin."
    (interactive)
    (evil-window-set-width (+ fill-column 1))
    (set-window-margins (selected-window) 1 1)
    (set-window-fringes (selected-window) 0 0)
    (set-window-scroll-bars (selected-window) 0 nil 0 nil))

(defun set-window-snug ()
    "The above settings have to be applied twice in succession to 'converge'.

    Better calculation may remove the need for this hack."
    (interactive)
    (set-window-snug-helper)
    (set-window-snug-helper))

;; Maximize the desktop window, if running a GUI.
(if (display-graphic-p)
    (add-to-list 'initial-frame-alist '(fullscreen . maximized)))
