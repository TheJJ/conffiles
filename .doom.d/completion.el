;;; completion.el -*- lexical-binding: t; -*-

(use-package! company-statistics
  :after company
  :config
  (progn
    (setq company-statistics-file (concat doom-cache-dir
                                          "company-statistics-cache.el"))
    (add-hook 'company-mode-hook 'company-statistics-mode)))

;; be more patient about highlighting stuff
(after! flycheck
  (setq flycheck-idle-change-delay 1.5))

;; we can try again once performance is better...
(after! lsp-pylsp
  (setq lsp-pylsp-plugins-flake8-enabled nil
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pylint-enabled nil))
