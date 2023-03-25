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
        whitespace-style '(face tabs trailing
                           newline indentation
                           space-before-tab space-after-tab
                           space-mark tab-mark newline-mark lines-tail)
        ;; emacs-builtin trailing space highlight
        show-trailing-whitespace t)

  (global-whitespace-mode t))

(defun whitespace-tail-disable ()
  "disable whitespace-mode's long line highlighting"
  ;; whitespace-highlight may not be initialized yet, thus handle both cases..
  (let ((no-lines-tail (lambda ()
                         (setq whitespace-style (delete 'lines-tail whitespace-style)))))
    (if (boundp 'whitespace-style)
      (funcall no-lines-tail)
      (progn
        (add-hook 'global-whitespace-mode-hook no-lines-tail)))))
