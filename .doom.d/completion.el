;;; completion.el -*- lexical-binding: t; -*-

(use-package! company-statistics
  :after company
  :config
  (progn
    (setq company-statistics-file (concat doom-cache-dir
                                          "company-statistics-cache.el"))
    (add-hook 'company-mode-hook 'company-statistics-mode)))
