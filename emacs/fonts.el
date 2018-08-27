

;; ----------  FONTS  ----------

;; Set monospaced and fixed-width fonts.
(if (display-graphic-p)
    (progn
        (set-face-attribute 'default nil :family "Fira Code" :height 160)
        (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 160)
        (set-face-attribute 'variable-pitch nil :family "Droid Sans" :height 160)))


;; Use ligatures in Fira Code Symbols for source code.
;; Fira Sans Mono: https://github.com/bBoxType/FiraSans (under SIL OFL license).
;; Fira Code: https://github.com/tonsky/FiraCode (under SIL OFL license).
;; Fira Code Symbol:
;;   https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip
;;
;; Emacs (as of 25.3.1) does not support font ligatures. To use ligatures,
;; either:
;;
;; - Run Emacs in text mode in a terminal that supports ligatures and is
;;   configured to use Fira Code. However, text mode does not support the
;;   display of text in a proportional font, or inline previews of math, images,
;;   etc.
;;
;; - Use one of the following glyph substitution methods:
;;
;;   - Use Fira Sans Mono with Fira Code Symbol (just the ligatures), and place
;;     substitution rules in `prettify-symbols-alist`. The rules specify strings
;;     for glyph substitution. This method is not flexible enough as regex
;;     patterns cannot be used. See:
;;     https://github.com/Profpatsch/blog/blob/master/posts/ligature-emulation-in-emacs/post.md
;;
;;   - Use either Fira Code, or a combination of Fira Sans Mono with Fira Code
;;     Symbol (just the ligatures), and place substitution rules in
;;     `font-lock-add-keywords`. The rules are regular expressions and Unicode
;;     codepoints for the ligature glyphs. Each ligature has a separate rule,
;;     resulting in simpler code. However, this method is unsuitable for
;;     `org-mode` buffers with `variable-pitch-mode`, as it applies to the
;;     entire buffer, not only the regions in a monospaced font. See:
;;     https://github.com/tonsky/FiraCode/wiki/Emacs-instructions.
;;
;;   - Use Fira Code which includes regular glyphs and ligatures, and place
;;     substitution rules in `composition-function-table`. The rules specify the
;;     initial characters and corresponding regex patterns to identify strings
;;     for glyph substitution with `font-shape-gstring`. All ligatures starting
;;     with the same character must be grouped together, with a single regex to
;;     match them all. To make the code more readable, `regexp-opt` can be used
;;     to generate the regexs from a list of ligature strings. In addition to
;;     `prog-mode`, this method is also suitable for `org-mode` buffers in
;;     `variable-pitch-mode`, as it only applies to regions in a monospaced
;;     font. See:
;;     https://github.com/tonsky/FiraCode/wiki/Emacs-instructions
;;
;;   The last method is used below.

;; Set Fira Code Symbol as the fallback font for Unicode private codepoints
;; matching the desired ligatures.
;; (add-hook 'after-make-frame-functions
;;     (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code")))
;; The above works when using emacs --daemon + emacsclient.
;; The following should work when using a standalone emacs instance.
;; (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

;; Define ligature substitution rules as (regex, codepoint) pairs.
;; (defconst fira-code-font-lock-keywords-alist
;;     (mapcar
;;         (lambda (regex-char-pair)
;;             `(,(car regex-char-pair)
;;                  (0 (prog1 ()
;;                         (compose-region
;;                             (match-beginning 1)
;;                             (match-end 1)
;;                             ;; The whitespace argument below is a literal tab.
;;                             ,(concat "	"
;;                                  (list (decode-char 'ucs (cadr regex-char-pair)))))))))
;;         '(
;;              ("\\(www\\)"                   #Xe100)
;;              ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
;;              ("\\(\\*\\*\\*\\)"             #Xe102)
;;              ("\\(\\*\\*/\\)"               #Xe103)
;;              ("\\(\\*>\\)"                  #Xe104)
;;              ("[^*]\\(\\*/\\)"              #Xe105)
;;              ("\\(\\\\\\\\\\)"              #Xe106)
;;              ("\\(\\\\\\\\\\\\\\)"          #Xe107)
;;              ("\\({-\\)"                    #Xe108)
;;              ("\\(\\[\\]\\)"                #Xe109)
;;              ("\\(::\\)"                    #Xe10a)
;;              ("\\(:::\\)"                   #Xe10b)
;;              ("[^=]\\(:=\\)"                #Xe10c)
;;              ("\\(!!\\)"                    #Xe10d)
;;              ("\\(!=\\)"                    #Xe10e)
;;              ("\\(!==\\)"                   #Xe10f)
;;              ("\\(-}\\)"                    #Xe110)
;;              ("\\(--\\)"                    #Xe111)
;;              ("\\(---\\)"                   #Xe112)
;;              ("\\(-->\\)"                   #Xe113)
;;              ("[^-]\\(->\\)"                #Xe114)
;;              ("\\(->>\\)"                   #Xe115)
;;              ("\\(-<\\)"                    #Xe116)
;;              ("\\(-<<\\)"                   #Xe117)
;;              ("\\(-~\\)"                    #Xe118)
;;              ("\\(#{\\)"                    #Xe119)
;;              ("\\(#\\[\\)"                  #Xe11a)
;;              ("\\(##\\)"                    #Xe11b)
;;              ("\\(###\\)"                   #Xe11c)
;;              ("\\(####\\)"                  #Xe11d)
;;              ("\\(#(\\)"                    #Xe11e)
;;              ("\\(#\\?\\)"                  #Xe11f)
;;              ("\\(#_\\)"                    #Xe120)
;;              ("\\(#_(\\)"                   #Xe121)
;;              ("\\(\\.-\\)"                  #Xe122)
;;              ("\\(\\.=\\)"                  #Xe123)
;;              ("\\(\\.\\.\\)"                #Xe124)
;;              ("\\(\\.\\.<\\)"               #Xe125)
;;              ("\\(\\.\\.\\.\\)"             #Xe126)
;;              ("\\(\\?=\\)"                  #Xe127)
;;              ("\\(\\?\\?\\)"                #Xe128)
;;              ("\\(;;\\)"                    #Xe129)
;;              ("\\(/\\*\\)"                  #Xe12a)
;;              ("\\(/\\*\\*\\)"               #Xe12b)
;;              ("\\(/=\\)"                    #Xe12c)
;;              ("\\(/==\\)"                   #Xe12d)
;;              ("\\(/>\\)"                    #Xe12e)
;;              ("\\(//\\)"                    #Xe12f)
;;              ("\\(///\\)"                   #Xe130)
;;              ("\\(&&\\)"                    #Xe131)
;;              ("\\(||\\)"                    #Xe132)
;;              ("\\(||=\\)"                   #Xe133)
;;              ("[^|]\\(|=\\)"                #Xe134)
;;              ("\\(|>\\)"                    #Xe135)
;;              ("\\(\\^=\\)"                  #Xe136)
;;              ("\\(\\$>\\)"                  #Xe137)
;;              ("\\(\\+\\+\\)"                #Xe138)
;;              ("\\(\\+\\+\\+\\)"             #Xe139)
;;              ("\\(\\+>\\)"                  #Xe13a)
;;              ("\\(=:=\\)"                   #Xe13b)
;;              ("[^!/]\\(==\\)[^>]"           #Xe13c)
;;              ("\\(===\\)"                   #Xe13d)
;;              ("\\(==>\\)"                   #Xe13e)
;;              ("[^=]\\(=>\\)"                #Xe13f)
;;              ("\\(=>>\\)"                   #Xe140)
;;              ("\\(<=\\)"                    #Xe141)
;;              ("\\(=<<\\)"                   #Xe142)
;;              ("\\(=/=\\)"                   #Xe143)
;;              ("\\(>-\\)"                    #Xe144)
;;              ("\\(>=\\)"                    #Xe145)
;;              ("\\(>=>\\)"                   #Xe146)
;;              ("[^-=]\\(>>\\)"               #Xe147)
;;              ("\\(>>-\\)"                   #Xe148)
;;              ("\\(>>=\\)"                   #Xe149)
;;              ("\\(>>>\\)"                   #Xe14a)
;;              ("\\(<\\*\\)"                  #Xe14b)
;;              ("\\(<\\*>\\)"                 #Xe14c)
;;              ("\\(<|\\)"                    #Xe14d)
;;              ("\\(<|>\\)"                   #Xe14e)
;;              ("\\(<\\$\\)"                  #Xe14f)
;;              ("\\(<\\$>\\)"                 #Xe150)
;;              ("\\(<!--\\)"                  #Xe151)
;;              ("\\(<-\\)"                    #Xe152)
;;              ("\\(<--\\)"                   #Xe153)
;;              ("\\(<->\\)"                   #Xe154)
;;              ("\\(<\\+\\)"                  #Xe155)
;;              ("\\(<\\+>\\)"                 #Xe156)
;;              ("\\(<=\\)"                    #Xe157)
;;              ("\\(<==\\)"                   #Xe158)
;;              ("\\(<=>\\)"                   #Xe159)
;;              ("\\(<=<\\)"                   #Xe15a)
;;              ("\\(<>\\)"                    #Xe15b)
;;              ("[^-=]\\(<<\\)"               #Xe15c)
;;              ("\\(<<-\\)"                   #Xe15d)
;;              ("\\(<<=\\)"                   #Xe15e)
;;              ("\\(<<<\\)"                   #Xe15f)
;;              ("\\(<~\\)"                    #Xe160)
;;              ("\\(<~~\\)"                   #Xe161)
;;              ("\\(</\\)"                    #Xe162)
;;              ("\\(</>\\)"                   #Xe163)
;;              ("\\(~@\\)"                    #Xe164)
;;              ("\\(~-\\)"                    #Xe165)
;;              ("\\(~=\\)"                    #Xe166)
;;              ("\\(~>\\)"                    #Xe167)
;;              ("[^<]\\(~~\\)"                #Xe168)
;;              ("\\(~~>\\)"                   #Xe169)
;;              ("\\(%%\\)"                    #Xe16a)
;;              ("\\b\\(x\\)\\b"               #Xe16b)
;;              ("[^:=]\\(:\\)[^:=]"           #Xe16c)
;;              ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
;;              ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

;; (defun add-fira-code-symbol-keywords ()
;;     (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

;; (add-hook 'prog-mode-hook #'add-fira-code-symbol-keywords)
;; (add-hook 'org-mode-hook  #'add-fira-code-symbol-keywords)


;; Use Fira Code ligatures in monospaced sections.
;;
;; Modified from https://github.com/tonsky/FiraCode/wiki/Emacs-instructions:
;; - Ligatures disabled: ** ***
;; - Ligatures enabled:

;; (when (display-graphic-p)
;;   (set-frame-font "Fira Code"))
(defconst fira-code-ligatures
    '(
         (?! . ("!" "=" "=="))
         (?# . ("#" "##" "###" "_" "(" "?"))))
(if (display-graphic-p)
    (let (
        (alist `(
            (,(decode-char 'ucs ?!) . ,(concat "." (regexp-opt '("!" "=" "=="))))
            ;; (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
            (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{=!]\\)")
            (36 . ".\\(?:>\\)")
            (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
            (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
            (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*/\\)\\|[/>]\\)")
            (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
            (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~|-]\\)")
            (46 . ".\\(?:\\(?:\\.[=.<]\\)\\|[.=}-]\\)") ;; .
            (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")  ;; /
            (48 . ".\\(?:x[a-zA-Z]\\)")
            (58 . ".\\(?:::\\|[:=>]\\)")
            (59 . ".\\(?:;;\\|;\\)")
            (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~[~>]\\|-[<>]\\|[$*+/]>\\|--\\|<[<=-]\\|=[<=>]\\||||?\\||>\\)\\|[:*$+~/<=>|-]\\)")
            (61 . ".\\(?:\\(?:[/!]=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")  ;; =
            (62 . ".\\(?:\\(?:[=-]>\\|>[=>-]\\)\\|[=>-]\\)")
            (63 . ".\\(?:[:=?.]\\)")
            (91 . ".\\(?:|\\)")
            (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
            (93 . ".\\(?:#\\)")
            (94 . ".\\(?:=\\)")
            (95 . ".\\(?:_\\||_\\)")
            (119 . ".\\(?:ww\\)")
            (123 . ".\\(?:[.|]\\)")
            (124 . ".\\(?:||?>\\|\\(?:|[=|]\\)\\|\\]\\|[=>|}-]\\)")  ;; |
            (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)"))))
        (dolist (char-regexp alist)
            (set-char-table-range
                composition-function-table
                (car char-regexp)
                `([,(cdr char-regexp) 0 font-shape-gstring])))))
;; ;; (if (display-graphic-p)
;; ;;     (set-char-table-range
;; ;;         composition-function-table
;; ;;         120
;; ;;         `(["\\b\\(x\\)\\b" 0 font-shape-gstring])))
