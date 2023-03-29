;;; hook.el -*- lexical-binding: t; -*-

;; config for all prog-modes
(defun jj/coding-hook ()
  ;; highlight important words
  (font-lock-add-keywords nil '(("\\<\\(TODO\\|todo\\|ASDF\\|asdf\\)" 1 font-lock-warning-face t)))

  (use-package! idle-highlight-mode)

  ;; highlight word under cursor
  (when (fboundp 'idle-highlight-mode)
    (idle-highlight-mode t))

  (jj/codenav-keybinds))


;; c-like-language setup
(defun jj/cstyle-hook ()

  ;; standalone cc cc-mode doesn't run prog-mode-hook
  ;; since it doesn't derive prog-mode
  (when (not (provided-mode-derived-p 'c-mode '(prog-mode)))
    (run-hooks 'prog-mode-hook))

  ;; magic region formatting
  (with-library
    clang-format
    (global-set-key (kbd "C-M-<tab>") #'clang-format-region))

  ;; restore so we indent line or region.
  ;; (does no completion since we set tab-always-indent)
  ;; cc-mode replaces this the other way round.
  (substitute-key-definition 'c-indent-command
                             'indent-for-tab-command
                             c-mode-base-map)

  (add-to-list 'lsp-before-initialize-hook
               (lambda ()
                 (add-to-list 'lsp-clients-clangd-args "--header-insertion=never")))  ; don't auto-insert #includes

  ;; create codestyles
  (jj/create-c-codestyles)

  ;; default to sft style
  (c-set-style "sft-cpp")

  ;; default to smart-tab indentation
  (setq-local
    tab-width 4
    indent-tabs-mode t)

  ;; smart tabs: mix tabs and spaces the right way
  (smart-tabs-mode)
  (smart-tabs-advice c-indent-line c-basic-offset)
  (smart-tabs-advice c-indent-region c-basic-offset)

  (c-toggle-auto-newline -1) ; no automatic newlines
  (c-toggle-electric-state -1) ; no auto reformatting
  )

;; hook for all c-like languages
(defun jj/c-base-hook ()
  (setq flycheck-gcc-language-standard "c++20")

  ;; c-codingstyle
  (jj/cstyle-hook)
  )


;; c++ special stuff
(defun jj/c++-coding-hook ()
  ;; comment parsing and word highlighting

  ;; placing regexes into `c-mode-common-hook' may work but their
  ;; evaluation order matters.
  (font-lock-add-keywords
    nil '(
          ;; missing C++ keywords
          ("\\<\\(static_assert\\|concept\\|requires\\|consteval\\|co_await\\|co_yield\\|co_return\\|export\\|import\\|module\\)\\>" . font-lock-keyword-face)

          ;; custom defined types
          ("\\<[A-Za-z_]+[A-Za-z_0-9]*_t\\>" . font-lock-type-face)
          ))
  )

;; java
(defun jj/java-coding-hook ()
  (c-set-style "sft-java")

  (setq-local
   indent-tabs-mode t
   tab-width 4
   c-basic-offset 4))

;; py
(defun jj/python-coding-hook ()
  (setq-local
    python-indent 4
    indent-tabs-mode nil
    tab-width 4)

  (setq flycheck-checker 'python-pylint
        flycheck-checker-error-threshold 300)

  ;; don't show anaconda mode error popup gaaarrhhgh
  (remove-hook 'anaconda-mode-response-read-fail-hook
               'anaconda-mode-show-unreadable-response)

  (use-package! blacken
    :after python)

  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  ;; disable docstring view
  (setq-local
    lsp-signature-doc-lines 0
    lsp-eldoc-enable-hover nil
    lsp-signature-auto-activate nil
    lsp-signature-render-documentation nil)

  ;; smart tabs
  (smart-tabs-mode)
  (smart-tabs-advice py-indent-line py-indent-offset)
  (smart-tabs-advice py-newline-and-indent py-indent-offset)
  (smart-tabs-advice py-indent-region py-indent-offset))

;; elisp
(defun jj/lisp-coding-hook ()
  (jj/codenav-keybinds)
  (setq-local
   indent-tabs-mode nil
   tab-width 8)

  (whitespace-tail-disable)

  ;; so plists and other things are properly indented
  ;; apparently doom has fixes for this already in place :)
  ;;(setq lisp-indent-function 'common-lisp-indent-function)
  ;;(put 'cl-flet 'common-lisp-indent-function
  ;;     (get 'flet 'common-lisp-indent-function))
  ;;(put 'cl-labels 'common-lisp-indent-function
  ;;     (get 'labels 'common-lisp-indent-function))
  ;;(put 'if 'common-lisp-indent-function 2)
  ;;(put 'dotimes-protect 'common-lisp-indent-function
  ;;     (get 'when 'common-lisp-indent-function))

  (prettify-symbols-mode t)
  (smart-tabs-advice lisp-indent-line lisp-indent-offset)

  ;; when editing dir-locals, reload all affected files
  (when (and (buffer-file-name)
             (equal dir-locals-file  ;; doesn't support .dir-locals-2.el :)
                    (file-name-nondirectory (buffer-file-name))))
    (message (format "installing auto-refresh hook when saving %s" (current-buffer)))
    (add-hook 'after-save-hook
              'reload-dir-locals-below-default-dir
              nil  ;; no insert offset
              t ;; only for this buffer
              )))

;; javascript / ecmascript
(defun jj/javascript-coding-hook ()
  (setq-local
    js-indent-level 2
    tab-width 2
    indent-tabs-mode nil))

;; TeX
(defun jj/latex-coding-hook ()
  ;; set latex indent offset so it doesn't fuck up
  ;; (i.e. use values != n*tab-width)
  (setq-local
    tab-width 4
    fill-column 80
    LaTeX-indent-level 4
    LaTeX-item-indent 0
    indent-tabs-mode nil

    ;; disable indenting after paste
    spacemacs-yank-indent-threshold 0
    ;; so completes start with 2 chars already
    company-minimum-prefix-length 2)

  (whitespace-tail-disable)

  ;; other settings are in jj/defaults
  (setq-default TeX-master nil) ; query for master file
  (visual-line-mode t)
  (LaTeX-math-mode t)
  (turn-on-reftex)

    ;; so pdf-tools buffer gets reverted
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

  ;; don't do non-company completions
  ;; otherwise a funny new buffer appears with "useful" completions
  (remove-hook 'completion-at-point-functions 'TeX--completion-at-point t)

  ;; minted codehighlighting needs shell execution for pygments
  (add-to-list 'TeX-command-list
                '("LaTeX-shellescape" "%`%l -shell-escape %(mode) %(extraopts) %' %t" TeX-run-TeX nil
                  (latex-mode doctex-mode) :help "Run LaTeX -shell-escape")
                t)
  (add-to-list 'TeX-command-list
                '("XeLaTeX" "%`xelatex %(mode) %(extraopts) %' %t" TeX-run-TeX nil
                  (latex-mode doctex-mode) :help "Run XeLaTeX")
                t)
  (add-to-list 'TeX-command-list
                '("XeLaTeXMk" "latexmk -xelatex %(-PDF)%S%(mode) %(file-line-error) %(extraopts) %t" TeX-run-latexmk nil
                  (plain-tex-mode latex-mode doctex-mode) :help "Run LaTeXMk with XeLaTeX")
                t))

;; BibTeX
(defun jj/bibtex-coding-hook ()
  (setq-local
    tab-width 2
    indent-tabs-mode nil
    bibtex-comma-after-last-field t
    bibtex-align-at-equal-sign t))

;; html
(defun jj/html-coding-hook ()
  (setq-local
    sgml-basic-offset 4
    indent-tabs-mode t))

;; haskell
(defun jj/haskell-coding-hook ()
  ;; haskell interpreter: C-c C-z or C-c C-l
  ;;(haskell-indentation-mode)
  (setq-local
    indent-tabs-mode nil))

;; vhdl
(defun jj/vhdl-coding-hook ()
  (setq-local
    indent-tabs-mode nil)
  (smart-tabs-mode)
  (smart-tabs-advice vhdl-indent-line vhdl-basic-offset))

;; org-mode
(defun jj/org-mode-hook ()
  (visual-line-mode t)
  (org-modern-mode t)
  (whitespace-tail-disable)

  (setq-local
    indent-tabs-mode nil))

;; markdown-mode
(defun jj/markdown-mode-hook ()
  (setq-local
    indent-tabs-mode nil
    markdown-toc-indentation-space 2
    markdown-toc-header-toc-start "<!-- markdown-toc start -->")

  (whitespace-tail-disable))

(defun jj/cmake-mode-hook ()
  (setq-local
    indent-tabs-mode t
    tab-width 4
    cmake-tab-width 4))

(defun jj/compilation-mode-hook ()
  ;; colorized compilation
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(defun jj/sql-indent-begin-block (context offset)
  "depending on the block kind, either do an indent, or not.
  no indent for defun, all other block kinds are indented."
  (let ((syntax (sqlind-syntax context))
        (anchor (sqlind-anchor-point context))
        (syntax-symbol (sqlind-syntax-symbol context)))

    (when (not (eq syntax-symbol 'in-begin-block))
      (error "indent-begin-block used for wrong syntax symbol '%s'"
             syntax-symbol))
    (let ((block-kind (nth 1 syntax))
          (block-label (nth 2 syntax)))
      (cond
        ;; defun block content should not be indented
        ((eq block-kind 'defun)
         offset)
        (t
         ;; indent all other types by basic offset
         (+ offset sqlind-basic-offset))
        ))))

(defun jj/sql-mode-hook ()
  (setq-local
    indent-tabs-mode nil
    tab-width 4
    sqlind-basic-offset 4
    sql-product 'postgres)

  ;; customize indentation - very simple basic-offset based indents
  ;; see current line analysis with sqlind-show-syntax-of-line
  (setq
   sqlind-indentation-offsets-alist
   `((select-clause 0)
     (in-select-clause +)
     (select-join-condition +)
     (select-table-continuation +)
     (delete-clause 0)
     (in-delete-clause +)
     (update-clause 0)
     (in-update-clause +)
     (insert-clause 0)
     (in-insert-clause +)
     (in-begin-block jj/sql-indent-begin-block)
     (with-clause-cte-cont 0)
     ,@sqlind-default-indentation-offsets-alist))

  ;; TODO: detect psql prompt, or disable custom prompt
  ;;(sql-set-product-feature 'postgres)

  (smart-tabs-mode)
  (smart-tabs-advice sqlind-indent-line sqlind-basic-offset))


(defun jj/sql-interactive-mode-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
      (let ((filename
             (locate-user-emacs-file
              (concat "sql/"
                      (symbol-name (symbol-value rval))
                      "-history.sql"))))
        (set (make-local-variable lval) filename))
      (error
        (format "SQL history will not be saved because %s is nil"
                (symbol-name rval))))))

(defun jj/emacs-server-visit-hook ()
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(defun jj/sh-mode-hook ()
  (setq-local
   tab-width 4
   sh-basic-offset 4
   indent-tabs-mode -1))

(defun jj/shell-mode-hook ()
  ;; correct zsh coloring in shell:
  (ansi-color-for-comint-mode-on))

(defun jj/eshell-mode-hook ()
  (add-hook 'eshell-preoutput-filter-functions
            'ansi-color-filter-apply))

(defun jj/artist-mode-hook ()
  ;; disable whitespace highlighting as long as artist mode is active.
  (setq show-trailing-whitespace (not artist-mode)))

(defun jj/magit-log-mode-hook ()
  ;; git log outputs some interesting whitespace errors
  (whitespace-mode -1)
  (setq show-trailing-whitespace nil))


(defun jj/mode-hooks ()
  "hooks are registered here"

  ;; hooks to be inherited:
  (add-hook 'prog-mode-hook       'jj/coding-hook)

  ;; language-specific hooks:
  (add-hook 'python-mode-hook            'jj/python-coding-hook)
  (add-hook 'lisp-mode-hook              'jj/lisp-coding-hook)
  (add-hook 'emacs-lisp-mode-hook        'jj/lisp-coding-hook)
  (add-hook 'js-mode-hook                'jj/javascript-coding-hook)
  (add-hook 'html-mode-hook              'jj/html-coding-hook)
  (add-hook 'haskell-mode-hook           'jj/haskell-coding-hook)
  (add-hook 'c-mode-common-hook          'jj/c-base-hook)
  (add-hook 'c++-mode-hook               'jj/c++-coding-hook)
  (add-hook 'java-mode-hook              'jj/java-coding-hook)
  (add-hook 'LaTeX-mode-hook             'jj/latex-coding-hook)
  (add-hook 'bibtex-mode-hook            'jj/bibtex-coding-hook)
  (add-hook 'vhdl-mode-hook              'jj/vhdl-coding-hook)
  (add-hook 'org-mode-hook               'jj/org-mode-hook)
  (add-hook 'markdown-mode-hook          'jj/markdown-mode-hook)
  (add-hook 'cmake-mode-hook             'jj/cmake-mode-hook)
  (add-hook 'compilation-mode-hook       'jj/compilation-mode-hook)
  (add-hook 'sql-mode-hook               'jj/sql-mode-hook)
  (add-hook 'server-visit-hook           'jj/emacs-server-visit-hook)
  (add-hook 'sh-mode-hook                'jj/sh-mode-hook)
  (add-hook 'shell-mode-hook             'jj/shell-mode-hook)
  (add-hook 'eshell-mode-hook            'jj/eshell-mode-hook)
  (add-hook 'artist-mode-hook            'jj/artist-mode-hook)
  (add-hook 'magit-log-mode-hook         'jj/magit-log-mode-hook)
  (add-hook 'compilation-filter-hook
            (lambda () (ansi-color-apply-on-region (point-min) (point-max))))
  ;; don't echo passwords when using interactive terminal programs
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)
  ;; man pages with clickable links
  (add-hook 'Man-mode-hook 'goto-address)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

  ;; tangle before exporting: https://orgmode.org/manual/Extracting-Source-Code.html
  (add-hook 'org-export-before-processing-functions #'org-babel-tangle)

  ;; hook removals
  (remove-hook 'doom-first-buffer-hook #'smartparens-global-mode)

  ;; some modes don't inherit from prog-mode...
  (multi-hook-add
   (lambda ()
     (run-hooks 'prog-mode-hook))
   '(python-mode-hook
     haskell-mode-hook)))
