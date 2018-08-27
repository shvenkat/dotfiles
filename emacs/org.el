

;; ----------  TEXT MODE  ----------

;; Customize the display of text documents (as opposed to code).
;; Use a proportional font, enable spell checking, and hide line numbers.
(add-hook 'text-mode-hook
    (lambda ()
        (progn
            (variable-pitch-mode t)
            (flyspell-mode 1)
            (blink-cursor-mode 0)
            (linum-mode 0))))


;; ----------  ORG MODE  ----------

;; To improve the readability of structured documents that combine text, math,
;; code, images, tables and notes, make the following groups of elements appear
;; distinct.
;; - Body text in a proportional font.
;; - Section headings in a bold larger monospaced font.
;; - Math and images as inline previews.
;; - Code and tables in a monospaced font.
;; - Notes as inline previews.
;; - Task and section metadata in a light italicized font in a lighter color.
;; - Block delimiters and other metadata in a small font.

(add-hook 'org-mode-hook
    (lambda ()
        (progn
            ;; Document title.
            (set-face-attribute 'org-document-title nil
                :inherit 'fixed-pitch
                :foreground (face-attribute 'default :foreground)
                :height 240)
            ;; Section headings are monospaced so tags appear right-aligned.
            (set-face-attribute 'org-level-1 nil
                :inherit 'fixed-pitch
                :height 220
                :weight 'bold)
            (set-face-attribute 'org-level-2 nil
                :inherit 'fixed-pitch
                :height 200
                :weight 'bold)
            (set-face-attribute 'org-level-3 nil
                :inherit 'fixed-pitch
                :height 180)
            (set-face-attribute 'org-level-4 nil
                :inherit 'fixed-pitch
                :height 160)
            ;; Tables are aligned in a monospaced font.
            (set-face-attribute 'org-table nil
                :inherit 'fixed-pitch
                :foreground (face-attribute 'default :foreground))
            ;; Code and verbatim regions in monospaced font.
            (set-face-attribute 'org-block nil
                :inherit 'fixed-pitch
                :foreground (face-attribute 'default :foreground))
            (set-face-attribute 'org-code nil
                :inherit 'fixed-pitch)
            (set-face-attribute 'org-verbatim nil
                :inherit 'fixed-pitch)
            ;; Links are displayed in color but not underlined. Reserve
            ;; underline for emphasis.
            (set-face-attribute 'org-link nil :underline nil)
            ;; Document metadata.
            (set-face-attribute 'org-document-info nil
                :foreground (face-attribute 'default :foreground))
            ;; Task metadata is made distinct in a light italicized font. It is
            ;; monospaced to match section headers.
            (set-face-attribute 'org-priority nil
                :inherit 'fixed-pitch
                :weight 'light)
            (set-face-attribute 'org-todo nil
                :inherit 'fixed-pitch
                :foreground (face-attribute 'org-priority :foreground)
                :background (face-attribute 'org-priority :background)
                :weight (face-attribute 'org-priority :weight)
                :slant (face-attribute 'org-priority :slant))
            (set-face-attribute 'org-done nil
                :inherit 'fixed-pitch
                :foreground (face-attribute 'org-priority :foreground)
                :background (face-attribute 'org-priority :background)
                :weight (face-attribute 'org-priority :weight)
                :slant (face-attribute 'org-priority :slant))
            (set-face-attribute 'org-checkbox nil
                :inherit 'fixed-pitch
                :foreground (face-attribute 'org-priority :foreground)
                :background (face-attribute 'org-priority :background)
                :weight (face-attribute 'org-priority :weight))
            (set-face-attribute 'org-tag nil
                :inherit 'fixed-pitch
                :foreground (face-attribute 'org-priority :foreground)
                :weight (face-attribute 'org-priority :weight)
                :slant 'normal)
            ;; Time metadata matches task metadata.
            (set-face-attribute 'org-date nil
                :inherit 'fixed-pitch
                :foreground (face-attribute 'org-priority :foreground)
                :weight (face-attribute 'org-priority :weight)
                :slant (face-attribute 'org-priority :slant)
                :underline nil)
            (set-face-attribute 'org-special-keyword nil
                :inherit 'fixed-pitch
                :foreground (face-attribute 'org-priority :foreground)
                :weight (face-attribute 'org-priority :weight)
                :slant (face-attribute 'org-priority :slant)
                :underline nil)
            ;; Make block delimiters and other metadata inconspicuous.
            (set-face-attribute 'org-meta-line nil :height 100)
            )))

;; TODO: Scale images to not exceed the width of the document body.
;; (org-display-inline-images t)

;; Scale the LaTeX preview images to match the body text font size.
(setq org-preview-latex-default-process 'dvipng)
(setq org-format-latex-options
    '(:foreground default
      :background default
      :scale 1.5
      :html-foreground "Black"
      :html-background "Transparent"
      :html-scale 1.0
      :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))


;; Automatically hide/show the LaTeX preview of math formulas, triggered by
;; cursor entry/exit. LaTeX preview must first be enabled (e.g. C-c C-x C-l).
;;
;; Copyright (C) 2015 by John Kitchin.
;; License: CC BY-SA 4.0.
;; The code below is derived from John Kitchin's work [1], available under the
;; CC BY-SA 4.0 license. The following modifications were made to update the
;; code for versions 24.3 and above of Emacs:
;;
;;   - Replacing `org-latex-fragment-image-overlays` with
;;     `(org--list-latex-overlays)`, as suggested by James Wong. See
;;     http://disq.us/p/15osv07.
;;   - Replacing `loop` with `cl-loop` and `return ov` with `do (cl-return ov)`,
;;     by Shiv Venkatasubrahmanyam. See http://disq.us/p/1v0y8gf.
;;
;; [1]: http://kitchingroup.cheme.cmu.edu/blog/2015/10/09/Automatic-latex-image-toggling-when-cursor-is-on-a-fragment/

(defvar org-latex-fragment-last nil
  "Holds last fragment/environment you were on.")

(defun org-latex-fragment-toggle ()
  "Toggle a latex fragment image."
  (and (eq 'org-mode major-mode)
       (let* ((el (org-element-context))
              (el-type (car el)))
         (cond
          ;; were on a fragment and now on a new fragment
          ((and
            ;; fragment we were on
            org-latex-fragment-last
            ;; and are on a fragment now
            (or
             (eq 'latex-fragment el-type)
             (eq 'latex-environment el-type))
            ;; but not on the last one this is a little tricky. as you edit the
            ;; fragment, it is not equal to the last one. We use the begin
            ;; property which is less likely to change for the comparison.
            (not (= (org-element-property :begin el)
                    (org-element-property :begin org-latex-fragment-last))))
           ;; go back to last one and put image back
           (save-excursion
             (goto-char (org-element-property :begin org-latex-fragment-last))
             (org-preview-latex-fragment))
           ;; now remove current image
           (goto-char (org-element-property :begin el))
           (let ((ov (cl-loop for ov in (org--list-latex-overlays)
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           do (cl-return ov))))
             (when ov
               (delete-overlay ov)))
           ;; and save new fragment
           (setq org-latex-fragment-last el))

          ;; were on a fragment and now are not on a fragment
          ((and
            ;; not on a fragment now
            (not (or
                  (eq 'latex-fragment el-type)
                  (eq 'latex-environment el-type)))
            ;; but we were on one
            org-latex-fragment-last)
           ;; put image back on
           (save-excursion
             (goto-char (org-element-property :begin org-latex-fragment-last))
             (org-preview-latex-fragment))
           ;; unset last fragment
           (setq org-latex-fragment-last nil))

          ;; were not on a fragment, and now are
          ((and
            ;; we were not one one
            (not org-latex-fragment-last)
            ;; but now we are
            (or
             (eq 'latex-fragment el-type)
             (eq 'latex-environment el-type)))
           (goto-char (org-element-property :begin el))
           ;; remove image
           (let ((ov (cl-loop for ov in (org--list-latex-overlays)
                           if
                           (and
                            (<= (overlay-start ov) (point))
                            (>= (overlay-end ov) (point)))
                           do (cl-return ov))))
             (when ov
               (delete-overlay ov)))
           (setq org-latex-fragment-last el))))))

(add-hook 'post-command-hook 'org-latex-fragment-toggle)
