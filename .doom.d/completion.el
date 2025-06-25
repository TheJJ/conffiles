;;; completion.el -*- lexical-binding: t; -*-

(when (modulep! :input company)
  (use-package! company-statistics
    :after company
    :config
    (progn
      (setq company-statistics-file (concat doom-cache-dir
                                            "company-statistics-cache.el"))
      (add-hook 'company-mode-hook 'company-statistics-mode))))

(after! flycheck
  ;; be more patient about highlighting stuff
  (setq flycheck-idle-change-delay 1.2
        flycheck-checker-error-threshold 5000))

;; we can try again once performance is better...
(after! lsp-pylsp
  (setq lsp-pylsp-plugins-flake8-enabled nil
        lsp-pylsp-plugins-mccabe-enabled nil
        lsp-pylsp-plugins-pylint-enabled nil))

(after! vertico
  (setq vertico-resize nil  ;; don't wildly resize the window
        vertico-count 18    ;; number of candidates
        vertico-cycle nil)) ;; no wrap in vertico candidates
