;; Center the text area by setting a suitably wide left margin. The right margin
;; is NOT set as this would cause long lines to be wrapped.
;; https://stackoverflow.com/a/23731757
;; Add left and right margins, when file is markdown or text.
(defun center-window (window) ""
    (let* ((max-text-width fill-column)
           (margin (max 0 (/ (- (window-total-width window) max-text-width) 2))))
        (set-window-margins window margin)))
;; Adjust margins of all windows.
(defun center-windows () ""
    (walk-windows (lambda (window) (center-window window)) nil 1))
;; Listen to window changes.
(add-hook 'window-configuration-change-hook 'center-windows)
