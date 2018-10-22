

;; ----------  FONTS  ----------

;; Set monospaced and fixed-width fonts.
(if (display-graphic-p)
    (progn
        (set-face-attribute 'default nil :family "Fira Code" :height 160)
        (set-face-attribute 'fixed-pitch nil :family "Fira Code" :height 160)
        (set-face-attribute 'variable-pitch nil :family "Droid Sans" :height 160)))


;; ----------  Font Symbols And Ligatures  ----------

;; Some monospaced fonts include ligatures to improve the readability of certain
;; operators, e.g. ->. The instructions below enable the use of one such font --
;; Fira Code -- in graphical mode; they can be easily adapted to other fonts.
;;
;; Emacs (as of 25.3.1) does not support font ligatures. To use ligatures, run
;; text-mode Emacs in a terminal that supports ligatures and is configured to
;; use the desired font. However, unlike graphical mode, text mode does not
;; support the display of text in a proportional font, or inline previews of
;; math, images, etc. which improve readability. To use ligatures in
;; graphical-mode Emacs, use one of the glyph substitution methods summarized
;; here and elaborated below:
;;
;;  A. Applies ligatures specific to each font, and therefore correctly handles
;;     `variable-pitch-mode` buffers with multiple fonts (e.g. Org mode). Works
;;     on MacOS, but not on (Debian and Fedora) Linux.
;;
;;  B. Applies ligatures to all fonts. In `variable-pitch-mode', some ligatures
;;     may need to be disabled to avoid applying them to proportionally-spaced
;;     text. Works on MacOS and Linux.
;;
;;  C. Applies ligatures to all fonts. Some ligatures may need to be disabled,
;;     as in the case of B, in `variable-pitch-mode'. Requires whitespace around
;;     ligature characters. Works on MacOS and Linux.
;;
;; An advantage of these hacks is that, unlike OS-level typesetting interfaces,
;; specific ligatures can be disabled as desired.
;;
;; Fira Sans Mono: https://github.com/bBoxType/FiraSans (under SIL OFL license).
;; Fira Code: https://github.com/tonsky/FiraCode (under SIL OFL license).
;; Fira Code Symbol:
;;   https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip
;;   https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632


;; Method A: Font-specific ligatures using `composition-function-table`.
;;
;; Using Fira Code, which includes regular glyphs and ligatures, place
;; substitution rules in `composition-function-table`. In addition to
;; `prog-mode`, this method is also suitable for `text-mode` buffers in
;; `variable-pitch-mode`. Font-specific ligatures are used, so the Fira Code
;; ligatures apply only to regions in the `fixed-pitch` font. See
;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions.
;;
;; This method works on MacOS, where Emacs draws text using the CoreText
;; OS-level interface. It does not work on some Linux distributions
;; (e.g. Debian and Fedora), where Emacs handles typesetting directly using M17N
;; and OTF libraries. With the above method, ligatures are not drawn, and
;; moreover, incorrect kerning is used between characters corresponding to the
;; ligatures, resulting in mis-aligned text in monospaced regions (e.g. Org-mode
;; tables). This is likely a bug or missing feature in Emacs, as other programs
;; such as gedit (which may use different interfaces or libraries for
;; typesetting), display the ligatures correctly using the same font.
;;
;;  1. Set Fira Code as the fixed-pitch font. This font includes regular glyphs
;;     and ligatures.
;;
;;         ;; If you use Fira Code exclusively in all buffers, use:
;;         (when (display-graphic-p)
;;             (set-frame-font "Fira Code"))
;;
;;         ;; To use Fira Code only for monospaced regions, use:
;;         (if (display-graphic-p)
;;             (progn
;;                 (set-face-attribute 'default nil :family "Fira Code")
;;                 (set-face-attribute 'fixed-pitch nil :family "Fira Code")))
;;
;;  2. Place glyph substitution rules in `composition-function-table`. The rules
;;     specify the initial characters and corresponding regex patterns to
;;     identify strings for glyph substitution with `font-shape-gstring`. All
;;     ligatures starting with the same character must be grouped together, with
;;     a single regex to match them all. To make the code more readable,
;;     `regexp-opt` can be used to generate the regexs from a list of ligature
;;     strings.
;;
;; Modified from https://github.com/tonsky/FiraCode/wiki/Emacs-instructions.

(defun enable-firacode-by-composition-function ()
    "Add FiraCode ligatures to composition-function-table."
    (let (
        (alist `(
            (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
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
            (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
            )))
        (dolist (char-regexp alist)
            (set-char-table-range
                composition-function-table
                (car char-regexp)
                `([,(cdr char-regexp) 0 font-shape-gstring])))))

;; TODO: A more readable definition of the ligature character patterns.
;;     (?! . ("!" "=" "=="))
;;     (?# . ("#" "##" "###" "_" "(" "?"))))
;;     (,(decode-char 'ucs ?!) . ,(concat "." (regexp-opt '("!" "=" "=="))))


;; Method B: Font-agnostic ligatures using `font-lock-add-keywords'.
;;
;; Use Fira Code with ligature glyphs copied to Private Use codepoints. Place
;; substitution rules in `font-lock-add-keywords`. The rules are regular
;; expressions and Unicode codepoints for the ligature glyphs. Each ligature has
;; a separate rule, resulting in simpler code.
;;
;; This method works for `prog-mode`, where all text is monospaced. However, it
;; is unsuitable for `text-mode` buffers in `variable-pitch-mode` (e.g. Org-mode
;; documents), as it applies to the entire buffer, not only the regions in a
;; monospaced font. With proportional fonts, the character spacing is incorrect.
;;
;; In order to achieve correct spacing in monospaced regions, the ligature
;; glyphs must be composed with the matching number of spaces on the left. Using
;; a single tab results in subtle kerning errors that lead to mis-aligned lines.
;;
;; This method works on MacOS and Linux (Debian and Fedora).
;;
;; Adapted from:
;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions and
;; https://github.com/tonsky/FiraCode/issues/211

(defconst firacode-glyph-regexps
    '(
         ;; (#Xe200  "!!"    "\\(!!\\)")
         (#Xe201  "!="    "[^=]\\(!=\\)[^=]")   ;; not !== =!=
         ;; (#Xe202  "!=="   "\\(!==\\)")
         (#Xe203  "#!"    "\\(#!\\)")
         ;; (#Xe204  "##"    "[^#]\\(##\\)[^#]")   ;; not ### ####
         ;; (#Xe205  "###"   "[^#]\\(###\\)[^#]")  ;; not ####
         ;; (#Xe206  "####"  "\\(####\\)")
         ;; (#Xe207  "#("    "\\(#(\\)")
         ;; (#Xe208  "#="    "\\(#=\\)")
         ;; (#Xe209  "#?"    "\\(#\\?\\)")
         ;; (#Xe20a  "#["    "\\(#\\[\\)")
         ;; (#Xe20b  "#_"    "\\(#_\\)[^(]")       ;; not #_(
         ;; (#Xe20c  "#_("   "\\(#_(\\)")
         ;; (#Xe20d  "#{"    "\\(#{\\)")
         ;; (#Xe20e  "$>"    "[^<]\\(\\$>\\)")     ;; not <$>
         ;; (#Xe20f  "%%"    "\\(%%\\)")
         (#Xe210  "&&"    "\\(&&\\)")
         ;; (#Xe211  "**"    "[^*]\\(\\*\\*\\)[^*]")  ;; not ***
         ;; (#Xe212  "***"   "\\(\\*\\*\\*\\)")
         ;; (#Xe213  "*/"    "\\(\\*/\\)")
         ;; (#Xe214  "*>"    "[^<]\\(\\*>\\)")     ;; not <*>
         (#Xe215  "++"    "[^+]\\(\\+\\+\\)[^+]")  ;; not +++
         ;; (#Xe216  "+++"   "\\(\\+\\+\\+\\)")
         ;; (#Xe217  "+>"    "[^<]\\(\\+>\\)")     ;; not <+>
         (#Xe218  "--"    "[^<!-]\\(--\\)[^>-]")   ;; not --- --> <!-- <--
         ;; (#Xe219  "---"   "\\(---\\)")
         ;; (#Xe21a  "-->"   "\\(-->\\)")
         ;; (#Xe21b  "-<"    "[^<]\\(-<\\)[^<]")   ;; not -<< <-<
         ;; (#Xe21c  "-<<"   "\\(-<<\\)")
         (#Xe21d  "->"    "[^<>-]\\(->\\)[^>]")    ;; not --> ->> <-> >->
         ;; (#Xe21e  "->>"   "\\(->>\\)")
         ;; (#Xe21f  "-|"    "\\(-|\\)")
         ;; (#Xe220  "-~"    "\\(-~\\)")
         ;; (#Xe221  ".-"    "\\(\\.-\\)")
         ;; (#Xe222  ".."    "[^.]\\(\\.\\.\\)[^.<=]")  ;; not ... ..< ..=
         ;; (#Xe223  "..."   "\\(\\.\\.\\.\\)")
         ;; (#Xe224  "..<"   "\\(\\.\\.<\\)")
         ;; (#Xe225  "..="   "\\(\\.\\.=\\)")
         ;; (#Xe226  ".="    "[^.]\\(\\.=\\)")     ;; not ..=
         ;; (#Xe227  ".?"    "\\(\\.\\?\\)")
         (#Xe228  "/*"    "\\(/\\*\\)")
         (#Xe229  "//"    "[^/]\\(//\\)[^/]")      ;; not ///
         ;; (#Xe22a  "///"   "\\(///\\)")
         ;; (#Xe22b  "/="    "[^=]\\(/=\\)[^=]")   ;; not /== =/=
         ;; (#Xe22c  "/=="   "\\(/==\\)")
         ;; (#Xe22d  "/>"    "[^<]\\(/>\\)")       ;; not </>
         ;; (#Xe22e  "::"    "[^:]\\(::\\)[^:=]")  ;; not ::: ::=
         ;; (#Xe22f  ":::"   "\\(:::\\)")
         ;; (#Xe230  "::="   "\\(::=\\)")
         (#Xe231  ":="    "[^:=]\\(:=\\)")         ;; not ::= =:=
         ;; (#Xe232  ":>"    "\\(:>\\)")
         ;; (#Xe233  ";;"    "\\(;;\\)")
         ;; (#Xe234  "<!--"  "\\(<!--\\)")
         ;; (#Xe235  "<$"    "\\(<\\$\\)[^>]")     ;; not <$>
         ;; (#Xe236  "<$>"   "\\(<\\$>\\)")
         ;; (#Xe237  "<*"    "\\(<\\*\\)[^>]")     ;; not <*>
         ;; (#Xe238  "<*>"   "\\(<\\*>\\)")
         ;; (#Xe239  "<+"    "\\(<\\+\\)[^>]")     ;; not <+>
         ;; (#Xe23a  "<+>"   "\\(<\\+>\\)")
         (#Xe23b  "<-"    "[^<]\\(<-\\)[^<>-]")    ;; not <-- <-< <-> <<-
         ;; (#Xe23c  "<--"   "\\(<--\\)")
         ;; (#Xe23d  "<-<"   "\\(<-<\\)")
         ;; (#Xe23e  "<->"   "\\(<->\\)")
         ;; (#Xe23f  "</"    "\\(</\\)[^>]")       ;; not </>
         ;; (#Xe240  "</>"   "\\(</>\\)")
         ;; (#Xe241  "<:"   "\\(<:\\)")
         (#Xe242  "<<"    "[^<=-]\\(<<\\)[^<=-]")  ;; not -<< <<- <<< <<= =<<
         ;; (#Xe243  "<<-"   "\\(<<-\\)")
         ;; (#Xe244  "<<<"   "\\(<<<\\)")
         ;; (#Xe245  "<<="   "\\(<<=\\)")
         (#Xe246  "<="    "[^<]\\(<=\\)[^<=>]")    ;; not <<= <=< <== <=>
         ;; (#Xe247  "<=<"   "\\(<=<\\)")
         ;; (#Xe248  "<=="   "\\(<==\\)")
         ;; (#Xe249  "<=>"   "\\(<=>\\)")
         (#Xe24a  "<>"    "\\(<>\\)")
         ;; (#Xe24b  "<|"    "\\(<|\\)[^>|]")      ;; not <|> <|| <|||
         ;; (#Xe24c  "<|>"   "\\(<|>\\)")
         ;; (#Xe24d  "<||"   "\\(<||\\)[^|]"       ;; not <|||
         ;; (#Xe24e  "<|||"  "\\(<|||\\)")
         ;; (#Xe24f  "<~"    "\\(<~\\)[^>~]"       ;; not <~> <~~
         ;; (#Xe250  "<~>"   "\\(<~>\\)")
         ;; (#Xe251  "<~~"   "\\(<~~\\)")
         ;; (#Xe252  "=!="   "\\(=!=\\)")
         ;; (#Xe253  "=/="   "\\(=/=\\)")
         ;; (#Xe254  "=:="   "\\(=:=\\)")
         ;; (#Xe255  "=<<"   "\\(=<<\\)")
         (#Xe256  "=="    "[^!/<=]\\(==\\)[^=>]")  ;; not !== /== <== === ==>
         ;; (#Xe257  "==="   "\\(===\\)")
         ;; (#Xe258  "==>"   "\\(==>\\)")
         (#Xe259  "=>"    "[^<=>]\\(=>\\)[^>]")    ;; not <=> ==> =>> >=>
         ;; (#Xe25a  "=>>"   "\\(=>>\\)")
         ;; (#Xe25b  ">-"    "[^>]\\(>-\\)[^>]")   ;; not >-> >>-
         ;; (#Xe25c  ">->"   "\\(>->\\)")
         (#Xe25d  ">="    "[^>]\\(>=\\)[^>]")      ;; not >=> >>=
         ;; (#Xe25e  ">=>"   "\\(>=>\\)")
         (#Xe25f  ">>"    "[^=>-]\\(>>\\)[^=>-]")  ;; not ->> =>> >>- >>= >>>
         ;; (#Xe260  ">>-"   "\\(>>-\\)")
         ;; (#Xe261  ">>="   "\\(>>=\\)")
         ;; (#Xe262  ">>>"   "\\(>>>\\)")
         ;; (#Xe263  "?."    "\\(\\?\\.\\)")
         ;; (#Xe264  "?:"    "\\(\\?:\\)")
         ;; (#Xe265  "?="    "\\(\\?=\\)")
         ;; (#Xe266  "??"    "\\(\\?\\?\\)")
         ;; (#Xe267  "[|"    "\\(\\[|\\)")
         ;; (#Xe268  "]#"    "\\(\\]#\\)")
         ;; (#Xe269  "^="    "\\(\\^=\\)")
         (#Xe26a  "__"    "\\(__\\)")
         ;; (#Xe26b  "_|_"   "\\(_|_\\)")
         ;; (#Xe281  "www"   "\\(www\\)")
         ;; (#Xe283  "{|"    "\\({|\\)")
         ;; (#Xe284  "|-"    "\\(|-\\)")
         ;; (#Xe285  "|="    "[^|]\\(|=\\)")      ;; not ||=
         ;; (#Xe286  "|>"    "[^|<]\\(|>\\)")     ;; not <|> ||> |||>
         ;; (#Xe287  "|]"    "\\(|\\]\\)")
         (#Xe288  "||"    "[^<|]\\(||\\)[^|=>]")  ;; not <|| <||| ||= ||> |||>
         ;; (#Xe289  "||="    "\\(||=\\)")
         ;; (#Xe28a  "||>"    "[^|]\\(||>\\)")    ;; not |||>
         ;; (#Xe28b  "|||>"    "\\(|||>\\)")
         ;; (#Xe28c  "|}"    "\\(|}\\)")
         ;; (#Xe28d  "~-"    "\\(~-\\)")
         ;; (#Xe28e  "~="    "\\(~=\\)")
         ;; (#Xe28f  "~>"    "[^<~]\\(~>\\)")     ;; not <~> ~~>
         ;; (#Xe290  "~@"    "\\(~@\\)")
         ;; (#Xe291  "~~"    "[^<]\\(~~\\)[^>]")  ;; not <~~ ~~>
         ;; (#Xe292  "~~>"    "\\(~~>\\)")
    )
    "(codepoint, characters, regexp) for substitute glyphs in Fira Code.")

(defconst firacode-font-lock-keywords
    (mapcar
        (lambda (el)
            (let* ((codepoint (car el))
                   (nchar (string-width (cadr el)))
                   (regex (caddr el))
                   (comp-rule
                       (append
                           (apply #'append (make-list (- nchar 1) '(?\s (Br . Bl))))
                           '(?\s (Br . Br))
                           (list (decode-char 'ucs codepoint)))))
                `(,regex
                  (0 (prog1 nil
                         (compose-region
                             (match-beginning 1)
                             (match-end 1)
                             ',comp-rule))))))
        firacode-glyph-regexps))

(defun add-firacode-font-lock-keywords ()
    (font-lock-add-keywords nil firacode-font-lock-keywords))

(defun enable-firacode-by-font-lock ()
    (progn
        (add-hook 'prog-mode-hook #'add-firacode-font-lock-keywords)
        (add-hook 'org-mode-hook  #'add-firacode-font-lock-keywords)))


;; Method C: Font-agnostic ligatures using `prettify-symbols-mode'.

;; Use Fira Code with ligature glyphs copied to Private Use codepoints. Place
;; substitution rules in `prettify-symbols-alist`. The rules specify strings and
;; Unicode codepoints for the ligature glyphs. This method is not flexible
;; enough to accomodate all ligatures as regex patterns cannot be used.
;;
;; This method works for `prog-mode`, where all text is monospaced. However, it
;; is unsuitable for `text-mode` buffers in `variable-pitch-mode` (e.g. Org-mode
;; documents), as it applies to the entire buffer, not only the regions in a
;; monospaced font. With proportional fonts, the character spacing is incorrect.
;;
;; In addition, this method requires whitespace around ligatures, and hence and
;; hence does not work with some ligatures, such as __ (e.g. __init__).
;;
;; In order to achieve correct spacing in monospaced regions, the ligature
;; glyphs must be composed with the matching number of spaces on the left. Using
;; a single tab results in subtle kerning errors that lead to mis-aligned lines.
;;
;; This method works on MacOS and Linux (Debian and Fedora).
;;
;; Adapted from:
;; https://github.com/Profpatsch/blog/blob/master/posts/ligature-emulation-in-emacs/post.md
;; https://github.com/tonsky/FiraCode/issues/211

(defconst firacode-prettify-symbols
    (mapcar (lambda (el) `(,(cadr el) . ,(car el))) firacode-glyph-regexps)
    "(characters, codepoint) for substitute glyphs in FiraCode.")

(defun fix-spacing (symbol-char)
    "Compose glyph with the matching number of spaces on the left."
    (let* (
              (symbol (car symbol-char))
              (char (cdr symbol-char))
              (nchar (string-width symbol)))
        (cons symbol
            (append
                (apply #'append (make-list (- nchar 1) '(?\s (Br . Bl))))
                '(?\s (Br . Br))
                (list (decode-char 'ucs char))))))


(defun add-firacode-prettify-symbols ()
    (let* ((symbols (mapcar #'fix-spacing firacode-prettify-symbols)))
        (progn
            (setq prettify-symbols-alist (append symbols prettify-symbols-alist))
            (prettify-symbols-mode))))

(defun enable-firacode-by-prettify-symbols ()
    (progn
        (add-hook 'prog-mode-hook #'add-firacode-prettify-symbols)
        (add-hook 'org-mode-hook  #'add-firacode-prettify-symbols)))


;; Select one of methods A, B and C.
(if (display-graphic-p)
    ;; (enable-firacode-by-composition-function))     ;; Method A.
    (enable-firacode-by-font-lock))                ;; Method B.
    ;; (enable-firacode-by-prettify-symbols))         ;; Method C.
