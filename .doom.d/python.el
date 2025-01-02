; python.el -*- lexical-binding: t; -*-


(after! python
  (setq
   python-fill-docstring-style 'symmetric
   python-shell-prompt-detect-failure-warning nil
   python-indent-def-block-scale 1))  ; multi-line function argument indent

(use-package! blacken
  :after python)

(defun jj/python-coding-hook ()
  (setq-local
    python-indent 4
    indent-tabs-mode nil
    tab-width 4
    flycheck-checker 'python-pylint
    flycheck-checker-error-threshold 300
    )

  ;; include _ as part of a word
  (modify-syntax-entry ?_ "w")

  ;; disable for now - better rely on linters and visual appearance...
  (whitespace-tail-disable)

  ;; don't show anaconda mode error popup gaaarrhhgh
  (remove-hook 'anaconda-mode-response-read-fail-hook
               'anaconda-mode-show-unreadable-response)

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
