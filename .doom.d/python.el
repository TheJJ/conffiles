; python.el -*- lexical-binding: t; -*-


(after! python
  (setq
   python-fill-docstring-style 'symmetric
   python-indent-def-block-scale 1)  ; multi-line function argument indent

  (set-formatter! 'ruff :modes '(python-mode python-ts-mode)))


(defun jj/python-coding-hook ()
  (setq-local
    python-indent-offset 4
    indent-tabs-mode nil
    tab-width 4)

  ;; include _ as part of a word
  (modify-syntax-entry ?_ "w")

  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  ;; disable docstring view
  (setq-local
    lsp-signature-doc-lines 0
    lsp-signature-render-documentation nil)

  ;; smart tabs
  (smart-tabs-mode)
  (smart-tabs-advice py-indent-line py-indent-offset)
  (smart-tabs-advice py-newline-and-indent py-indent-offset)
  (smart-tabs-advice py-indent-region py-indent-offset))
