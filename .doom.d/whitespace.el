;;; whitespace.el -*- lexical-binding: t; -*-

;; bad whitespace highlighting
(defun jj/whitespace-highlight ()
  ;; see whitespace.el
  (interactive)
  (setq whitespace-display-mappings
        '(
          ;;(space-mark   ?\     [?\u00B7]     [?.])      ; space - centered dot
          (space-mark   ?\xA0  [?\u00A4]   [?_])          ; hard space - currency
          ;;(newline-mark ?\n    [?¬ ?\n]    [?$ ?\n])      ; eol - ¬ symbol
          (tab-mark     ?\t    [?∘ ?\t]    [?> ?\t]))     ; tab - ∘ symbol
        ;; disabled: lines-tail for long line highlighting
        whitespace-style '(face tabs trailing
                           newline indentation
                           space-before-tab space-after-tab
                           space-mark tab-mark newline-mark)
        ;; emacs-builtin trailing space highlight
        show-trailing-whitespace t)

  (global-whitespace-mode t))
