

;; ----------  TEXT MODE  ----------

;; Customize the display of text documents (as opposed to code).
;; Use a proportional font, and hide line numbers.
(add-hook 'text-mode-hook
    (lambda ()
        (progn
            (variable-pitch-mode t)
            (blink-cursor-mode 0))))


;; ----------  ORG MODE  ----------

(use-package org
    :ensure t
    :init
    (setq org-adapt-indentation t
        org-hide-leading-stars t
        org-odd-levels-only t
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-log-done 'time
        org-log-into-drawer "LOGBOOK"
        org-todo-keywords '((sequence "TODO(t)" "DOING(i!)" "|" "DONE(d!)") (sequence "|" "CANCELED(c@!)"))
        org-todo-keyword-faces '(("TODO" . "brightred") ("DOING" . "yellow") ("DONE" . "green") ("CANCELED" . "brightcyan"))
        org-provide-todo-statistics '(("TODO" "DOING") ("DONE"))
        org-hierarchical-todo-statistics nil
        org-checkbox-hierarchical-statistics nil))

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
            (set-face-attribute 'org-hide nil
                :foreground "white"
                :background "white")
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
      :scale 2.0
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

;; From org-mode docs. Basic parent updates for two state TODOs.
;; (defun org-summary-todo (n-done n-not-done)
;;   "Switch entry to DONE when all subentries are done, to TODO otherwise."
;;   (let (org-log-done org-todo-log-states)   ; turn off logging
;;     (org-todo (if (= n-not-done 0) "DONE" "TODO"))))
;; (add-hook 'org-after-todo-statistics-hook #'org-summary-todo)

;; Block manual state transitions of parents.
;; (setq org-agenda-dim-blocked-tasks t)

;; Count leaf statistics.
;; (setq org-hierarchical-todo-statistics t)

;; Automatically change parent TODO state: https://emacs.stackexchange.com/a/81401
;; https://emacs.stackexchange.com/a/81401
;; (defun my/org-checkbox-summary-todo ()
;;   "Switch todo status based on status of checkbox subentries."
;;   (let ((todo-state (org-get-todo-state)) beg end)
;;     (unless (not todo-state)
;;       (save-excursion
;;         (org-back-to-heading t)
;;         (setq beg (point))
;;         (end-of-line)
;;         (setq end (point))
;;         (goto-char beg)
;;         (if (re-search-forward "\\[\\([0-9]*\\)%\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
;;                                end t)
;;             ;; `match-string' for a group in an unmatched alternative returns nil
;;             (let* ((p-done (match-string 1))
;;                    (n-done (match-string 2))
;;                    (n-total (match-string 3))
;;                    (new-state (cond ((equal p-done "0") "TODO")
;;                                     ((equal p-done "100") "DONE")
;;                                     ((equal n-done "0") "TODO")
;;                                     ((and n-done (equal n-done n-total)) "DONE")
;;                                     (t "DOING"))))
;;               (unless (string-equal todo-state new-state)
;;                   (org-todo new-state))))))))
;; (add-hook 'org-checkbox-statistics-hook #'my/org-checkbox-summary-todo)

;; https://christiantietze.de/posts/2021/02/emacs-org-todo-doing-done-checkbox-cycling/
;; (setq org-todo-keywords
;;       (quote ((sequence "TODO(t)" "DOING(g)" "|" "DONE(d)"))))

;; (defun org-todo-if-needed (state)
;;   "Change header state to STATE unless the current item is in STATE already."
;;   (unless (string-equal (org-get-todo-state) state)
;;     (org-todo state)))

;; (defun ct/org-summary-todo-cookie (n-done n-not-done)
;;   "Switch header state to DONE when all subentries are DONE, to TODO when none are DONE, and to DOING otherwise"
;;   (let (org-log-done org-log-states)   ; turn off logging
;;     (org-todo-if-needed (cond ((= n-done 0)
;;                                "TODO")
;;                               ((= n-not-done 0)
;;                                "DONE")
;;                               (t
;;                                "DOING")))))
;; (add-hook 'org-after-todo-statistics-hook #'ct/org-summary-todo-cookie)

;; (defun ct/org-summary-checkbox-cookie ()
;;   "Switch header state to DONE when all checkboxes are ticked, to TODO when none are ticked, and to DOING otherwise"
;;   (let (beg end)
;;     (unless (not (org-get-todo-state))
;;       (save-excursion
;;         (org-back-to-heading t)
;;         (setq beg (point))
;;         (end-of-line)
;;         (setq end (point))
;;         (goto-char beg)
;;         ;; Regex group 1: %-based cookie
;;         ;; Regex group 2 and 3: x/y cookie
;;         (if (re-search-forward "\\[\\([0-9]*%\\)\\]\\|\\[\\([0-9]*\\)/\\([0-9]*\\)\\]"
;;                                end t)
;;             (if (match-end 1)
;;                 ;; [xx%] cookie support
;;                 (cond ((equal (match-string 1) "100%")
;;                        (org-todo-if-needed "DONE"))
;;                       ((equal (match-string 1) "0%")
;;                        (org-todo-if-needed "TODO"))
;;                       (t
;;                        (org-todo-if-needed "DOING")))
;;               ;; [x/y] cookie support
;;               (if (> (match-end 2) (match-beginning 2)) ; = if not empty
;;                   (cond ((equal (match-string 2) (match-string 3))
;;                          (org-todo-if-needed "DONE"))
;;                         ((or (equal (string-trim (match-string 2)) "")
;;                              (equal (match-string 2) "0"))
;;                          (org-todo-if-needed "TODO"))
;;                         (t
;;                          (org-todo-if-needed "DOING")))
;;                 (org-todo-if-needed "DOING"))))))))
;; (add-hook 'org-checkbox-statistics-hook #'ct/org-summary-checkbox-cookie)


;; A Kanban (agenda) view for org-mode.
(use-package org-kanban
    :ensure t
    :after org
    :init
    (add-hook 'org-mode-hook (lambda () (global-whitespace-mode -1))))
