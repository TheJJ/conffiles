;;; org.el -*- lexical-binding: t; -*-

;; must be set before org loads
(setq org-directory "~/org/")

(use-package! org-modern
  :after org)

;; org settings
(after! org

  ;; tame org-open-file, which uses org-file-apps, and finally mailcap.el
  ;; to determine how to open pdf files
  ;; if we do not set this in mailcap-user-mime-data, it returns pdf-view-mode
  ;; test with:
  ;; (mailcap-mime-info (mailcap-extension-to-mime ".pdf"))
  (setcdr (assoc "\\.pdf\\'" org-file-apps) 'default)

  ;; TODO: scale according to display dpi and zoom!
  ;;       i.e. Xft.dpi/96 * zoomfactor
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (setq org-hide-emphasis-markers t  ; hide syntax elements
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-appear-autolinks nil      ; let links be invisible because they expand to long lines
        org-appear-autoentities t
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-reverse-note-order t         ; new items at top
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-enforce-todo-dependencies t
        org-src-window-setup 'current-window ; edit in current window
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-latex-compiler "xelatex"
        org-latex-pdf-process '("latexmk -f -%latex -interaction=nonstopmode -shell-escape -output-directory=%o %f")
        org-latex-src-block-backend 'minted
        org-confirm-babel-evaluate nil   ; sure, just execute org code snippets, what can go wrong
        org-babel-default-header-args:cpp '((:flags . "-std=c++20 -Wall -Wextra"))
        org-log-done nil
        org-cycle-level-after-item/entry-creation nil)

  ;; so pressing tab in insert mode doesn't indent the headline.
  (remove-hook! 'org-tab-first-hook #'+org-indent-maybe-h)
  ;; pressing tab should cycle also child items, not just the current heading
  (remove-hook! 'org-tab-first-hook #'+org-cycle-only-current-subtree-h))

;; org roam note system
(after! org-roam
  (setq org-roam-directory (expand-file-name "~/org/zettel")))

;; create hooks to redirect bibtex notes handling into org-roam
(use-package! org-roam-bibtex
  :after org-roam
  :config
  (progn
    (require 'org-ref)
    (add-hook 'org-roam-hook #'org-roam-bibtex-mode)))

(use-package! org-super-agenda
  :after org-agenda
  :config
  (setq org-super-agenda-groups
        '((:name "Due today"
           :deadline today)
          (:name "Overdue"
           :deadline past)
          (:name "Upcoming Deadline"
           :deadline future)
          (:name "Scheduled Today"
           :scheduled today)
          (:name "Scheduled Past"
           :scheduled past)
          (:name "Future Tasks"
           :scheduled future)
          (:discard (:anything t)))))
