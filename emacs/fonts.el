

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
;; Fira Sans Mono: https://github.com/bBoxType/FiraSans (under SIL OFL license).
;; Fira Code: https://github.com/tonsky/FiraCode (under SIL OFL license).
;; Fira Code Symbol:
;;   https://github.com/tonsky/FiraCode/files/412440/FiraCode-Regular-Symbol.zip
;;   https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632
;;
;; www  \ue100     **   \ue101    ***  \ue102    **/  \ue103
;;  *>  \ue104     */   \ue105     \\  \ue106    \\\  \ue107
;;  {-  \ue108     []   \ue109     ::  \ue10a    :::  \ue10b
;;  :=  \ue10c     !!   \ue10d     !=  \ue10e    !==  \ue10f
;;  -}  \ue110     --   \ue111    ---  \ue112    -->  \ue113
;;  ->  \ue114    ->>   \ue115     -<  \ue116    -<<  \ue117
;;  -~  \ue118     #{   \ue119     #[  \ue11a     ##  \ue11b
;; ###  \ue11c   ####   \ue11d     #(  \ue11e     #?  \ue11f
;;  #_  \ue120    #_(   \ue121     .-  \ue122     .=  \ue123
;;  ..  \ue124    ..<   \ue125    ...  \ue126     ?=  \ue127
;;  ??  \ue128     ;;   \ue129     /*  \ue12a    /**  \ue12b
;;  /=  \ue12c    /==   \ue12d     />  \ue12e     //  \ue12f
;; ///  \ue130     &&   \ue131     ||  \ue132    ||=  \ue133
;;  |=  \ue134     |>   \ue135     ^=  \ue136     $>  \ue137
;;  ++  \ue138    +++   \ue139     +>  \ue13a     +>  \ue13a
;; =:=  \ue13b     ==   \ue13c    ===  \ue13d    ==>  \ue13e
;;  =>  \ue13f    =>>   \ue140     <=  \ue141    =<<  \ue142
;; =/=  \ue143     >-   \ue144     >=  \ue145    >=>  \ue146
;;  >>  \ue147    >>-   \ue148    >>=  \ue149    >>>  \ue14a
;;  <*  \ue14b    <*>   \ue14c     <|  \ue14d    <|>  \ue14e
;;  <$  \ue14f    <$>   \ue150   <!--  \ue151     <-  \ue152
;; <--  \ue153    <->   \ue154     <+  \ue155    <+>  \ue156
;;  <=  \ue157    <==   \ue158    <=>  \ue159    <=<  \ue15a
;;  <>  \ue15b     <<   \ue15c    <<-  \ue15d    <<=  \ue15e
;; <<<  \ue15f     <~   \ue160    <~~  \ue161     </  \ue162
;; </>  \ue163     ~@   \ue164     ~-  \ue165     ~=  \ue166
;;  ~>  \ue167     ~~   \ue168    ~~>  \ue169     %%  \ue16a
;;   x  \ue16b      :   \ue16c      +  \ue16d      *  \ue16f


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
;; Modified from https://github.com/tonsky/FiraCode/wiki/Emacs-instructions:

;; (if (display-graphic-p)
;;     (let (
;;         (alist `(
;;             ;; (,(decode-char 'ucs ?!) . ,(concat "." (regexp-opt '("!" "=" "=="))))
;;             (33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
;;             (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{=!]\\)")
;;             (36 . ".\\(?:>\\)")
;;             (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
;;             (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
;;             (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*/\\)\\|[/>]\\)")
;;             (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
;;             (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~|-]\\)")
;;             (46 . ".\\(?:\\(?:\\.[=.<]\\)\\|[.=}-]\\)") ;; .
;;             (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")  ;; /
;;             (48 . ".\\(?:x[a-zA-Z]\\)")
;;             (58 . ".\\(?:::\\|[:=>]\\)")
;;             (59 . ".\\(?:;;\\|;\\)")
;;             (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~[~>]\\|-[<>]\\|[$*+/]>\\|--\\|<[<=-]\\|=[<=>]\\||||?\\||>\\)\\|[:*$+~/<=>|-]\\)")
;;             (61 . ".\\(?:\\(?:[/!]=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")  ;; =
;;             (62 . ".\\(?:\\(?:[=-]>\\|>[=>-]\\)\\|[=>-]\\)")
;;             (63 . ".\\(?:[:=?.]\\)")
;;             (91 . ".\\(?:|\\)")
;;             (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
;;             (93 . ".\\(?:#\\)")
;;             (94 . ".\\(?:=\\)")
;;             (95 . ".\\(?:_\\||_\\)")
;;             (119 . ".\\(?:ww\\)")
;;             (123 . ".\\(?:[.|]\\)")
;;             (124 . ".\\(?:||?>\\|\\(?:|[=|]\\)\\|\\]\\|[=>|}-]\\)")  ;; |
;;             (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
;;             )))
;;         (dolist (char-regexp alist)
;;             (set-char-table-range
;;                 composition-function-table
;;                 (car char-regexp)
;;                 `([,(cdr char-regexp) 0 font-shape-gstring])))))

;; TODO: A more readable definition of the ligature character patterns.
;; (defconst fira-code-ligatures
;;     '(
;;          (?! . ("!" "=" "=="))
;;          (?# . ("#" "##" "###" "_" "(" "?"))))

;; TODO: A pattern for the use of the multiplication (cross) symbol.
;; (if (display-graphic-p)
;;     (set-char-table-range
;;         composition-function-table
;;         120
;;         `(["\\b\\(x\\)\\b" 0 font-shape-gstring])))


;; Method B: Font-agnostic ligatures using `font-lock-add-keywords'.
;;
;; Use either Fira Code or Fira Mono for regular glyphs, and Fira Code Symbol
;; for the ligatures. Place substitution rules in `font-lock-add-keywords`. The
;; rules are regular expressions and Unicode codepoints for the ligature
;; glyphs. Each ligature has a separate rule, resulting in simpler code.
;;
;; This method works for `prog-mode`, where all text is monospaced. However, it
;; is unsuitable for `text-mode` buffers in `variable-pitch-mode` (e.g. Org-mode
;; documents), as it applies to the entire buffer, not only the regions in a
;; monospaced font. With proportional fonts, the character spacing is incorrect.
;;
;; This method works on MacOS and Linux (Debian and Fedora).
;;
;; See:
;; https://github.com/tonsky/FiraCode/wiki/Emacs-instructions and
;; https://github.com/tonsky/FiraCode/issues/211#issuecomment-239058632.

;; Use glyphs from Fira Code Symbol for Unicode private codepoints matching the
;; desired ligatures.
;; For emacs --daemon + emacsclient:
;; (add-hook 'after-make-frame-functions
;;     (lambda (frame)
;;         (set-fontset-font nil '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; The following should work when using a standalone emacs instance.
;; (set-fontset-font nil '(#Xe100 . #Xe16f) "Fira Code Symbol")

(defconst fira-code-glyph-regexps
    '(
         ;; (#Xe200  "!!"    "\\(!!\\)")
         (#Xe201  "!="    "[^=]\\(!=\\)[^=]")  ;; not !== =!=
         ;; (#Xe202  "!=="   "\\(!==\\)")
         (#Xe203  "#!"    "\\(#!\\)")
         ;; (#Xe204  "##"    "[^#]\\(##\\)[^#]")  ;; not ### ####
         ;; (#Xe205  "###"   "[^#]\\(###\\)[^#]")  ;; not ####
         ;; (#Xe206  "####"  "\\(####\\)")
         ;; (#Xe207  "#("
         ;; (#Xe208  "#="
         ;; (#Xe209  "#?"
         ;; (#Xe20a  "#["
;; #_	#_(
         ;; (#Xe20b  "#_"
         ;; (#Xe20c  "#_("
         ;; (#Xe20d  "#{"
;; $>	<$>
         ;; (#Xe20e  "$>"
         ;; (#Xe20f  "%%"
         (#Xe210  "&&"    "\\(&&\\)")
;; **	***
         ;; (#Xe211  "**"
         ;; (#Xe212  "***"
         ;; (#Xe213  "*/"    "\\(\\*/\\)")
         ;; (#Xe214  "*>"
;; *>	<*>
         (#Xe215  "++"    "[^+]\\(\\+\\+\\)[^+]")  ;; not +++
         ;; (#Xe216  "+++"
;; +>	<+>
         ;; (#Xe217  "+>"
         (#Xe218  "--"    "[^<!-]\\(--\\)[^>-]")  ;; not --- --> <!-- <--
         ;; (#Xe219  "---"
         ;; (#Xe21a  "-->"   "\\(-->\\)")
;; -<	-<< <-<
         ;; (#Xe21b  "-<"
         ;; (#Xe21c  "-<<"
         (#Xe21d  "->"    "[^<>-]\\(->\\)[^>]")  ;; not --> ->> <-> >->
         ;; (#Xe21e  "->>"
         ;; (#Xe21f  "-|"
         ;; (#Xe220  "-~"
         ;; (#Xe221  ".-"
         ;; (#Xe222  ".."
;; ..	... ..< ..=
         ;; (#Xe223  "..."
         ;; (#Xe224  "..<"
         ;; (#Xe225  "..="
         ;; (#Xe226  ".="
;; .=	..=
         ;; (#Xe227  ".?"
         (#Xe228  "/*"    "\\(/\\*\\)")
         (#Xe229  "//"    "[^/]\\(//\\)[^/]")  ;; not ///
         ;; (#Xe22a  "///"
         ;; (#Xe22b  "/="
;; /=	/== =/=
         ;; (#Xe22c  "/=="
         ;; (#Xe22d  "/>"
;; />	</>
         ;; (#Xe22e  "::"
;; ::	::: ::=
         ;; (#Xe22f  ":::"
         ;; (#Xe230  "::="
         (#Xe231  ":="    "[^:=]\\(:=\\)")  ;; not ::= =:=
;; :=	::= =:=
         ;; (#Xe232  ":>"
         ;; (#Xe233  ";;"
         ;; (#Xe234  "<!--"
         ;; (#Xe235  "<$"
;; <$	<$>
         ;; (#Xe236  "<$>"
         ;; (#Xe237  "<*"
;; <*	<*>
         ;; (#Xe238  "<*>"
         ;; (#Xe239  "<+"
;; <+	<+>
         ;; (#Xe23a  "<+>"
         (#Xe23b  "<-"    "[^<]\\(<-\\)[^<>-]")  ;; not <-- <-< <-> <<-
         ;; (#Xe23c  "<--"
         ;; (#Xe23d  "<-<"
         ;; (#Xe23e  "<->"
         ;; (#Xe23f  "</"
;; </	</>
         ;; (#Xe240  "</>"
         ;; (#Xe241  "<:"
         (#Xe242  "<<"    "[^<=-]\\(<<\\)[^<=-]")  ;; not -<< <<- <<< <<= =<<
         ;; (#Xe243  "<<-"
         ;; (#Xe244  "<<<"
         ;; (#Xe245  "<<="
         (#Xe246  "<="    "[^<]\\(<=\\)[^<=>]")  ;; not <<= <=< <== <=>
         ;; (#Xe247  "<=<"
         ;; (#Xe248  "<=="
         ;; (#Xe249  "<=>"
         (#Xe24a  "<>"    "\\(<>\\)")
         ;; (#Xe24b  "<|"
;; <|	<|> <|| <|||
         ;; (#Xe24c  "<|>"
         ;; (#Xe24d  "<||"
;; <||	<|||
         ;; (#Xe24e  "<|||"
         ;; (#Xe24f  "<~"
;; <~	<~> <~~
         ;; (#Xe250  "<~>"
         ;; (#Xe251  "<~~"
         ;; (#Xe252  "=!="
         ;; (#Xe253  "=/="
         ;; (#Xe254  "=:="
         ;; (#Xe255  "=<<"
         (#Xe256  "=="    "[^!/<=]\\(==\\)[^=>]")  ;; not !== /== <== === ==>
         ;; (#Xe257  "==="
         ;; (#Xe258  "==>"
         (#Xe259  "=>"    "[^<=>]\\(=>\\)[^>]")  ;; not <=> ==> =>> >=>
         ;; (#Xe25a  "=>>"
         ;; (#Xe25b  ">-"
;; >-	>-> >>-
         ;; (#Xe25c  ">->"
         (#Xe25d  ">="    "[^>]\\(>=\\)[^>]")  ;; not >=> >>=
         ;; (#Xe25e  ">=>"
         (#Xe25f  ">>"    "[^=>-]\\(>>\\)[^=>-]")  ;; not ->> =>> >>- >>= >>>
         ;; (#Xe260  ">>-"
         ;; (#Xe261  ">>="
         ;; (#Xe262  ">>>"
         ;; (#Xe263  "?."
         ;; (#Xe264  "?:"
         ;; (#Xe265  "?="
         ;; (#Xe266  "??"
         ;; (#Xe267  "[|"
         ;; (#Xe268  "]#"
         ;; (#Xe269  "^="
         (#Xe26a  "__"    "\\(__\\)")
         ;; (#Xe26b  "_|_"
         ;; (#Xe26c  "asterisk.lc"
         ;; (#Xe26d  "braceleft.case"
         ;; (#Xe26e  "braceright.case"
         ;; (#Xe26f  "bracketleft.case"
         ;; (#Xe270  "bracketright.case"
         ;; (#Xe271  "bullet"
         ;; (#Xe272  "colon.uc"
         ;; (#Xe273  "fraction"
         ;; (#Xe274  "hyphen.case"
         ;; (#Xe275  "hyphen.lc"
         ;; (#Xe276  "parenleft.case"
         ;; (#Xe277  "parenright.case"
         ;; (#Xe278  "part.backslash"
         ;; (#Xe279  "plus.lc"
         ;; (#Xe27a  "uni207A"
         ;; (#Xe27b  "uni207C"
         ;; (#Xe27c  "uni207D"
         ;; (#Xe27d  "uni207E"
         ;; (#Xe27e  "uni208A"
         ;; (#Xe27f  "uni208D"
         ;; (#Xe280  "uni208E"
         ;; (#Xe281  "www"
         ;; (#Xe282  "x.multiply"
         ;; (#Xe283  "{|"
         ;; (#Xe284  "|-"
         ;; (#Xe285  "|="
;; |=	||=
         ;; (#Xe286  "|>"
;; |>	<|> ||> |||>
         ;; (#Xe287  "|]"
         (#Xe288  "||"    "[^<|]\\(||\\)[^|=>]")  ;; not <|| <||| ||= ||> |||>
         ;; (#Xe289  "||="
         ;; (#Xe28a  "||>"
;; ||>	|||>
         ;; (#Xe28b  "|||>"
         ;; (#Xe28c  "|}"
         ;; (#Xe28d  "~-"
         ;; (#Xe28e  "~="
         ;; (#Xe28f  "~>"
;; ~>	<~> ~~>
         ;; (#Xe290  "~@"
         ;; (#Xe291  "~~"
;; ~~	<~~ ~~>
         ;; (#Xe292  "~~>"
    )
    "(codepoint, characters, regexp) for substitute glyphs in Fira Code.")

(defconst fira-ligature-font-lock-keywords
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
        fira-code-glyph-regexps))

;; (defconst fira-ligature-keywords
;;     (let*
;;         (
;;             (regex "[^-]\\(->\\)")
;;             (nchar 2)
;;             (ligg #Xe114)
;;             (comp-rule
;;                 (append
;;                     (apply #'append (make-list (- nchar 1) '(?\s (Br . Bl))))
;;                     '(?\s (Br . Br))
;;                     (list (decode-char 'ucs ligg))))
;;         )
;;         `((
;;               ,regex
;;               (0
;;                   (prog1 nil
;;                       (compose-region (match-beginning 1) (match-end 1) ',comp-rule)))
;;         ))
;;     )
;; )
;; (defconst my-keywords
;;     `(("[^-]\\(->\\)"
;;        (0 (prog1 nil
;;               (compose-region
;;                   (match-beginning 1)
;;                   (match-end 1)
;;                   (list ?\s '(Br . Bl) ?\s '(Br . Br) 57620)))))))
;;                   ;; '(?\s (Br . Bl)
;;                   ;;   ?\s (Br . Br)
;;                   ;;   57620)))))))
;;                     ;; ,(decode-char 'ucs #Xe114))))))))

(defconst fira-code-font-lock-keywords-alist
    (mapcar
        (lambda (regex-char-pair)
            `(,(car regex-char-pair)
                 (0 (prog1 ()
                        (compose-region
                            (match-beginning 1)
                            (match-end 1)
                            ;; The whitespace argument below is a literal tab.
                            ,(concat "	"
                                 (list (decode-char 'ucs (cadr regex-char-pair)))))))))
        '(
             ("\\(www\\)"                   #Xe281)
             ;; ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
             ;; ("\\(\\*\\*\\*\\)"             #Xe102)
             ;; ("\\(\\*\\*/\\)"               #Xe103)
             ;; ("\\(\\*>\\)"                  #Xe104)
             ;; ("[^*]\\(\\*/\\)"              #Xe105)
             ;; ("\\(\\\\\\\\\\)"              #Xe106)
             ;; ("\\(\\\\\\\\\\\\\\)"          #Xe107)
             ;; ("\\({-\\)"                    #Xe108)
             ;; ("\\(\\[\\]\\)"                #Xe109)
             ;; ("\\(::\\)"                    #Xe10a)
             ;; ("\\(:::\\)"                   #Xe10b)
             ;; ("[^=]\\(:=\\)"                #Xe10c)
             ;; ("\\(!!\\)"                    #Xe10d)
             ;; ("\\(!=\\)"                    #Xe10e)
             ;; ("\\(!==\\)"                   #Xe10f)
             ;; ("\\(-}\\)"                    #Xe110)
             ;; ("[^-]\\(--\\)[^-]"            #Xe111)
             ;; ("[^-]\\(---\\)[^-]"           #Xe112)
             ;; ("\\(-->\\)"                   #Xe113)
             ;; ;; ("[^-]\\(->\\)"                #Xe114)
             ;; ("\\(->>\\)"                   #Xe115)
             ;; ("\\(-<\\)"                    #Xe116)
             ;; ("\\(-<<\\)"                   #Xe117)
             ;; ("\\(-~\\)"                    #Xe118)
             ;; ("\\(#{\\)"                    #Xe119)
             ;; ("\\(#\\[\\)"                  #Xe11a)
             ;; ("\\(##\\)"                    #Xe11b)
             ;; ("\\(###\\)"                   #Xe11c)
             ;; ("\\(####\\)"                  #Xe11d)
             ;; ("\\(#(\\)"                    #Xe11e)
             ;; ("\\(#\\?\\)"                  #Xe11f)
             ;; ("\\(#_\\)"                    #Xe120)
             ;; ("\\(#_(\\)"                   #Xe121)
             ;; ("\\(\\.-\\)"                  #Xe122)
             ;; ("\\(\\.=\\)"                  #Xe123)
             ;; ("\\(\\.\\.\\)"                #Xe124)
             ;; ("\\(\\.\\.<\\)"               #Xe125)
             ;; ("\\(\\.\\.\\.\\)"             #Xe126)
             ;; ("\\(\\?=\\)"                  #Xe127)
             ;; ("\\(\\?\\?\\)"                #Xe128)
             ;; ("\\(;;\\)"                    #Xe129)
             ;; ("\\(/\\*\\)"                  #Xe12a)
             ;; ("\\(/\\*\\*\\)"               #Xe12b)
             ;; ("\\(/=\\)"                    #Xe12c)
             ;; ("\\(/==\\)"                   #Xe12d)
             ;; ("\\(/>\\)"                    #Xe12e)
             ;; ("\\(//\\)"                    #Xe12f)
             ;; ("\\(///\\)"                   #Xe130)
             ;; ("\\(&&\\)"                    #Xe131)
             ;; ("\\(||\\)"                    #Xe132)
             ;; ("\\(||=\\)"                   #Xe133)
             ;; ("[^|]\\(|=\\)"                #Xe134)
             ;; ("\\(|>\\)"                    #Xe135)
             ;; ("\\(\\^=\\)"                  #Xe136)
             ;; ("\\(\\$>\\)"                  #Xe137)
             ;; ("\\(\\+\\+\\)"                #Xe138)
             ;; ("\\(\\+\\+\\+\\)"             #Xe139)
             ;; ("\\(\\+>\\)"                  #Xe13a)
             ;; ("\\(=:=\\)"                   #Xe13b)
             ;; ("[^!/]\\(==\\)[^>]"           #Xe13c)
             ;; ("\\(===\\)"                   #Xe13d)
             ;; ("\\(==>\\)"                   #Xe13e)
             ;; ("[^=]\\(=>\\)"                #Xe13f)
             ;; ("\\(=>>\\)"                   #Xe140)
             ;; ("\\(<=\\)"                    #Xe141)
             ;; ("\\(=<<\\)"                   #Xe142)
             ;; ("\\(=/=\\)"                   #Xe143)
             ;; ("\\(>-\\)"                    #Xe144)
             ;; ("\\(>=\\)"                    #Xe145)
             ;; ("\\(>=>\\)"                   #Xe146)
             ;; ("[^-=]\\(>>\\)"               #Xe147)
             ;; ("\\(>>-\\)"                   #Xe148)
             ;; ("\\(>>=\\)"                   #Xe149)
             ;; ("\\(>>>\\)"                   #Xe14a)
             ;; ("\\(<\\*\\)"                  #Xe14b)
             ;; ("\\(<\\*>\\)"                 #Xe14c)
             ;; ("\\(<|\\)"                    #Xe14d)
             ;; ("\\(<|>\\)"                   #Xe14e)
             ;; ("\\(<\\$\\)"                  #Xe14f)
             ;; ("\\(<\\$>\\)"                 #Xe150)
             ;; ("\\(<!--\\)"                  #Xe151)
             ;; ("\\(<-\\)"                    #Xe152)
             ;; ("\\(<--\\)"                   #Xe153)
             ;; ("\\(<->\\)"                   #Xe154)
             ;; ("\\(<\\+\\)"                  #Xe155)
             ;; ("\\(<\\+>\\)"                 #Xe156)
             ;; ("\\(<=\\)"                    #Xe157)
             ;; ("\\(<==\\)"                   #Xe158)
             ;; ("\\(<=>\\)"                   #Xe159)
             ;; ("\\(<=<\\)"                   #Xe15a)
             ;; ("\\(<>\\)"                    #Xe15b)
             ;; ("[^-=]\\(<<\\)"               #Xe15c)
             ;; ("\\(<<-\\)"                   #Xe15d)
             ;; ("\\(<<=\\)"                   #Xe15e)
             ;; ("\\(<<<\\)"                   #Xe15f)
             ;; ("\\(<~\\)"                    #Xe160)
             ;; ("\\(<~~\\)"                   #Xe161)
             ;; ("\\(</\\)"                    #Xe162)
             ;; ("\\(</>\\)"                   #Xe163)
             ;; ("\\(~@\\)"                    #Xe164)
             ;; ("\\(~-\\)"                    #Xe165)
             ;; ("\\(~=\\)"                    #Xe166)
             ;; ("\\(~>\\)"                    #Xe167)
             ;; ("[^<]\\(~~\\)"                #Xe168)
             ;; ("\\(~~>\\)"                   #Xe169)
             ;; ("\\(%%\\)"                    #Xe16a)
             ;; ("0\\(x\\)[0-9A-Fa-f]"         #Xe16b)
             ;; ;; ("[^:=]\\(:\\)[^:=]"           #Xe16c)
             ;; ;; ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
             ;; ;; ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f)
             )))

(defun add-fira-code-symbol-keywords ()
    ;; (font-lock-add-keywords nil fira-code-font-lock-keywords-alist)
    (font-lock-add-keywords nil fira-ligature-font-lock-keywords)
    )

(if (display-graphic-p)
    (progn
        (add-hook 'prog-mode-hook #'add-fira-code-symbol-keywords)
        (add-hook 'org-mode-hook  #'add-fira-code-symbol-keywords)))


;; Method C: Font-agnostic ligatures using `prettify-symbols-mode'.

;; Use either Fira Code or Fira Mono for regular glyphs, and Fira Code Symbol
;; for the ligatures. Place substitution rules in `prettify-symbols-alist`. The
;; rules specify strings and Unicode codepoints for the ligature glyphs. This
;; method is not flexible enough to accomodate all ligatures as regex patterns
;; cannot be used.
;;
;; This method works for `prog-mode`, where all text is monospaced. However, it
;; is unsuitable for `text-mode` buffers in `variable-pitch-mode` (e.g. Org-mode
;; documents), as it applies to the entire buffer, not only the regions in a
;; monospaced font. With proportional fonts, the character spacing is incorrect.
;;
;; In addition, this method requires whitespace around ligatures, and hence and
;; hence does not work with some ligatures, such as __ (e.g. __init__).
;;
;; This method works on MacOS and Linux (Debian and Fedora).
;;
;; See:
;; https://github.com/Profpatsch/blog/blob/master/posts/ligature-emulation-in-emacs/post.md

;; (require 'dash)

;; ;; NOTE: The following set-fontset-font is not needed.
;; ;; (add-hook 'after-make-frame-functions
;; ;;     (lambda (frame)
;; ;;         (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; ;; (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")

;; (defun replicate (list num)
;;     (if (<= num 0) '() (append list (replicate list (- num 1)))))

;; (defun make-spaces (el)
;;     (let ((space-width (string-width (car el))))
;;         (append
;;             (replicate '(?\s (Br . Bl)) (- space-width 1))
;;             ;; (make-list (- space-width 1) '(?\s (Br . Bl)))
;;             '(?\s (Br . Br))
;;             (list (decode-char 'ucs (cdr el))))))

;; (defun make-tabs (el)
;;     (string ?\t (cdr el)))

;; (defun my-correct-symbol-bounds (pretty-alist)
;;     "Prepend a TAB character to each symbol in this alist,
;; this way compose-region called by prettify-symbols-mode
;; will use the correct width of the symbols
;; instead of the width measured by char-width."
;;     (let ((out (mapcar (lambda (el) (setcdr el (make-spaces el)) el) pretty-alist)))
;;         (progn (print out) out)))

;; ;; (defun my-correct-symbol-bounds (pretty-alist)
;; ;;     "Prepend a TAB character to each symbol in this alist,
;; ;; this way compose-region called by prettify-symbols-mode
;; ;; will use the correct width of the symbols
;; ;; instead of the width measured by char-width."
;; ;;     (mapcar (lambda (el)
;; ;;               (setcdr el (string ?\t (cdr el)))
;; ;;               el)
;; ;;             pretty-alist))

;; (defun my-ligature-list (ligatures codepoint-start)
;;     "Create an alist of strings to replace with
;; codepoints starting from codepoint-start."
;;     (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
;;       (-zip-pair ligatures codepoints)))

;; (setq my-fira-code-ligatures
;;     (let* ((ligs '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
;;                   "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
;;                   "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
;;                   "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
;;                   ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
;;                   "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
;;                   "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
;;                   "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
;;                   ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
;;                   "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
;;                   "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
;;                   "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
;;                   "x" ":" "+" "+" "*")))
;;       (my-correct-symbol-bounds (my-ligature-list ligs #Xe100))))

;; ;; nice glyphs for haskell with hasklig
;; (defun my-set-hasklig-ligatures ()
;;     "Add hasklig ligatures for use with prettify-symbols-mode."
;;     (setq prettify-symbols-alist
;;           (append my-fira-code-ligatures prettify-symbols-alist))
;;       (prettify-symbols-mode))

;; (add-hook 'prog-mode-hook 'my-set-hasklig-ligatures)
;; (add-hook 'org-mode-hook 'my-set-hasklig-ligatures)
;; (add-hook 'python-mode-hook 'my-set-hasklig-ligatures)


;; prettify-symbols-mode hacks.

;; (add-hook 'python-mode-hook
;;           (lambda ()
;;             (push '("<=" . ?L) prettify-symbols-alist)))
;; (add-hook 'python-mode-hook
;;     (lambda ()
;;         (push `("www" . ,(decode-char 'ucs #Xe100))
;;         ;; (push `("www" . ,(decode-char 'ucs #X0025))
;;             prettify-symbols-alist)))



;; OTF ligature support hacks.

;; http://emacs.1067599.n8.nabble.com/otf-ligature-rendering-support-td345082.html
;; http://emacs-devel.gnu.narkive.com/vNaMVvP8/otf-ligature-rendering-support
;; https://groups.google.com/forum/#!topic/gnu.emacs.help/c5yY4akk3vk
;; (set-fontset-font nil 'latin
;;                   (font-spec :family "PragmataPro"
;;                              :otf '(latn nil (liga))))

;; (set-fontset-font
;;     "fontset-auto2"
;;     ascii
;;     (font-spec
;;         :registry "iso10646-1"
;;         :otf))
