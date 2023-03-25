;;; codestyle.el -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coding style definitions
;; ------------------------
;;
;; see all the possible variables at [emacsshare]/lisp/progmodes/cc-vars.el
;; c-set-stylevar-fallback 'c-offsets-alist
;;
;; get syntax/indent info by C-c C-s
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; closing template <> should line up.
(defun c-lineup-c++-template-args-cont (langelem)
  "Indentation of template params for a closing '>'.
return values:
0   : If the first non-whitespace char is '>'. Line it up under 'template'.
nil : Otherwise, return nil and run next lineup function."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^[\t ]*>" (line-end-position) t)
        0)))

(defun jj/create-c-codestyles ()
  ;; codestyle definitions

  ;; linux kernel indentation style
  (c-add-style
   "linux-kernel"
   '("linux" ;; based on the builtin linux style
     (c-offsets-alist . (
                         (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist)
                         (arglist-close . 0)
                         ))
     ))

  (c-add-style
   "sft-java"
   '("java" ;; based on the builtin java style
     (c-offsets-alist . (
                         (arglist-intro . +)
                         (arglist-close . 0)
                         (statement-cont        . (first c-lineup-string-cont
                                                         c-lineup-cascaded-calls
                                                         c-lineup-assignments))
                         ))
     ))

  ;; sft coding style
  (defconst sft-c-style
    '("linux"  ;; base it on linux code style
      (c-doc-comment-style        . javadoc)
      (c-block-comment-prefix     . " * ")
      (indent-tabs-mode           . t)
      (c-basic-offset             . 4)
      (c-tab-always-indent        . t)
      (c-comment-only-line-offset . 4)
      (c-hanging-colons-alist     . ((access-label after)
                                     (case-label after)
                                     (inher-intro)
                                     (label after)
                                     (member-init-intro before)))
      (c-cleanup-list             . (brace-else-brace
                                     brace-elseif-brace
                                     brace-catch-brace
                                     empty-defun-braces
                                     defun-close-semi
                                     list-close-comma
                                     scope-operator))
      (c-comment-only-line-offset . 0)
      (c-comment-only-line-offset . 0)
      (c-hanging-braces-alist . ((arglist-cont-nonempty)
                                 (block-close . c-snug-do-while)
                                 (brace-entry-open)
                                 (brace-list-open)
                                 (substatement-open after)
                                 (defun-open after)
                                 (defun-close before after)
                                 (class-open after)
                                 (class-close before after)
                                 (inexpr-class-open after)
                                 (inexpr-class-close before)
                                 (namespace-open after)
                                 (inline-open after)
                                 (inline-close before after)
                                 (block-open after)
                                 (extern-lang-open after)
                                 (extern-lang-close after)
                                 (statement-case-open after)))
      (c-offsets-alist . (
                          ; documentation:
                          ; https://www.gnu.org/software/emacs/manual/html_node/ccmode/c_002doffsets_002dalist.html
                          ; symbols:
                          ; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Syntactic-Symbols.html
                          ;
                          ; arg indent helper funcs: c-lineup-*
                          ; arglist = indent to matching (|here, stuff
                          ; argcont = indent to (stuff, |here
                          ; casecaded calls = ->lol\n->stuff
                          ; absolute offset: [0]
                          ; defaults are defined in cc-vars.el
                          (access-label          . -)   ; public: or private:
                          (arglist-intro         . +)   ; first arg in newline
                          (arglist-cont          . 0)   ; wrapped function args: func(\nthisone
                                                        ; wrapped function args after func(arg,\nthisone:
                          (arglist-cont-nonempty . (first c-lineup-string-cont
                                                          c-lineup-cascaded-calls
                                                          ;c-lineup-argcont
                                                          c-lineup-arglist))
                          (arglist-close         . 0)   ; intentation of ) which closes tabbed args
                          (block-open            . 0)   ; { to open a block
                          (block-close           . 0)   ; } after a block
                          (brace-list-open       . 0)
                                                        ; first element in {\nthisone:
                          (brace-list-intro      . (first c-lineup-2nd-brace-entry-in-arglist
                                                          c-lineup-class-decl-init-+
                                                          +))
                                                        ;; much improved through emacs commit aa1a4cceca2d93d83c721ce83950230739073727
                                                        ; other elements in {\nelem\nthisone
                          (brace-list-entry      . 0)
                          (brace-list-close      . 0)   ; } of brace list
                          (brace-entry-open      . 0)   ; {...,\n{thisone
                          (case-label            . 0)   ; case 1337:
                          (statement-case-open   . 0)   ; { after case 1337:
                          (statement-case-intro  . +)   ; code after case 1337:
                          (cpp-macro             . [0])   ; #define, etcetc
                          (defun-block-intro     . +)   ; code in block: keyword (...) {\nthisone
                          (inclass               . +)   ; members of struct or class
                          (inexpr-class          . 0)   ; class declaration within expression
                          (inexpr-statement      . 0)   ; statement block within expression
                          (inher-intro           . +)   ; beginning of inheritance def
                          (inher-cont            . c-lineup-multi-inher)   ; inheritance continuation
                          (inlambda              . 0)   ; function body of a lambda
                          (inline-open           . 0)   ; brace opening in-class method definition a rolf()\n{<-
                          (innamespace           . 0)   ; namespace lol {\nthisstatement
                          (knr-argdecl-intro     . 0)
                          (label                 . 0)   ; gotolabel:
                          (member-init-intro     . +)   ; member initializing for class lol : var(val)
                          (member-init-cont      . c-lineup-multi-inher)   ; further members
                          (statement             . 0)
                          (statement-block-intro . +)   ; line in if () {\nthisline
                                                        ; int a =\nthisone or return B{\nthisone
                                                        ; or B{bla +\nthisone
                                                        ; TODO: return bla+\nthis should be indented to bla
                          (statement-cont        . (first c-lineup-string-cont
                                                          c-lineup-cascaded-calls
                                                          c-lineup-assignments))
                          (stream-op             . c-lineup-streamop)
                          (substatement          . +)
                          (substatement-label    . 0)
                          (substatement-open     . 0)
                          (template-args-cont    . (first c-lineup-c++-template-args-cont
                                                          c-lineup-template-args
                                                          +))
                          (topmost-intro         . 0)   ; indentation of file start
                          (topmost-intro-cont    . c-lineup-topmost-intro-cont)
                          (comment-intro         . c-lineup-comment)   ; start of comment
                          (c                     . c-lineup-C-comments)  ; multiline comment continuation. what a name.
                          ))

      ;; information about indent parsing on TAB
      ;; this is also triggered by C-c C-s
      (c-echo-syntactic-information-p . nil))
    "The SFT C++ programming style"
    )
  (c-add-style "sft-cpp" sft-c-style)

  ;; https://github.com/llvm-mirror/llvm/blob/master/utils/emacs/emacs.el
  (defun llvm-lineup-statement (langelem)
    (let ((in-assign (c-lineup-assignments langelem)))
      (if (not in-assign)
          '++
        (aset in-assign 0
              (+ (aref in-assign 0)
                 (* 2 c-basic-offset)))
        in-assign)))

  ;; ciip style based on llvm
  ;; https://gitlab.lrz.de/IP/elsa/-/blob/master/.clang-format
  (c-add-style "ciipstyle"
               '("gnu"
                 (fill-column . 100)
                 (c++-indent-level . 4)
                 (c-basic-offset . 4)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . +)
                                     (arglist-close         . 0)
                                     (innamespace . +)  ;; ohh whyyyy, pls change to 0
                                     (member-init-intro . ++)
                                     (inlambda . 0)   ; lambda function body
                                     (statement-cont . llvm-lineup-statement)))))

  ;; google c++ style
  ;; https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
  (after! google-c-style
      (c-add-style "Google" google-c-style t))
)
