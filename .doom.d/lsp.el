;;; lsp.el -*- lexical-binding: t; -*-

;; adjust lsp-mode internals
(after! lsp-mode
  (setq lsp-enable-indentation nil       ; don't ask the language server for indentations
        lsp-enable-imenu nil
        lsp-enable-xref t
        lsp-headerline-breadcrumb-enable t
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-enable-on-type-formatting nil  ; using t funnily changes screen content whenever lsp thinks it can do "formatting"
        lsp-enable-file-watchers nil       ; lsp server can do inotify itself, but that may slow emacs down (https://github.com/MaskRay/ccls/issues/354)
        lsp-eldoc-enable-hover t           ; display info about thing at cursor in minibuffer
        lsp-eldoc-render-all nil
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-signature-doc-lines 1
        lsp-enable-snippet t
        lsp-warn-no-matched-clients nil    ; don't warn if there's no lsp client.
        dap-python-debugger 'debugpy)

  ; remote lsp clients
  ;(lsp-register-client
  ; (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
  ;                  :major-modes '(c++-mode)
  ;                  :remote? t
  ;                  :server-id 'clangd-remote))
  ;(lsp-register-client
  ; (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
  ;                  :major-modes '(python-mode)
  ;                  :remote? t
  ;                  :server-id 'pylsp-remote))

  ;; update treemacs folders from lsp
  (after! lsp-treemacs
    (add-hook 'lsp-workspace-folders-changed-functions #'lsp-treemacs--sync-folders)))
