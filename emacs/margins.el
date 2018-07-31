

;; ----------  MARGINS  ----------

;; Center the text area by setting a suitably wide left margin. The right margin
;; is NOT set as this would cause long lines to be wrapped.
;; https://stackoverflow.com/a/23731757
;; Add margins, when file is markdown or text.
(defun center-window (window) ""
    (let* ((max-text-width fill-column)
           (margin (max 0 (/ (- (window-total-width window) max-text-width) 2))))
        (set-window-margins window margin)))
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