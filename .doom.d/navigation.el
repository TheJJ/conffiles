;;; navigation.el -*- lexical-binding: t; -*-

(defvar jj/fileext-priority
  '("py" ("cpp" "c" "h" "hpp") ("org" "tex" "md") "pdf")
  "Priority of file extensions to show for file selections.
Groups have the same priority.")

(defun jj/sort-file-priority (dir-contents)
  "sort the input list by file extension priority"
  (let* ((priolist jj/fileext-priority)
         (leastprio (length priolist))
         (priolookup (lambda (ext) (or (double-list-index ext priolist) leastprio))))
    ;; if the sort method is by extension
    (let-alist (seq-group-by #'file-directory-p dir-contents)
      (nconc
       .t                            ; folders
       (sort
        .nil                         ; files
        (lambda (fa fb)              ; file comparison
          (let ((exta (or (file-name-extension fa) ""))
                (extb (or (file-name-extension fb) ""))
                (basea (or (file-name-sans-extension fa) ""))
                (baseb (or (file-name-sans-extension fb) "")))
            (cond
              ;; extensions are equal, sort filename by name.
              ((string= exta extb)
               (string< fa fb))
              ;; look up extension priority and sort by it.
              (t (let ((exta-prio (funcall priolookup exta))
                       (extb-prio (funcall priolookup extb)))
                   (if (= exta-prio extb-prio)
                       (string< basea baseb)
                     (< exta-prio extb-prio))))))))))))

;; to test: (jj/sort-file-priority '("A.aux" "A.tex" "A.cpp" "B.aux" "B.tex" "B.cpp"))

;;;; helm find file sorting by extension, and preferred ones..
(defun jj/helm-sort-files (dir-contents)
  "enhance the helm-list-directory function by allowing file extension priorities"

  (pcase helm-ff-initial-sort-method
    ;; if the sort method is by extension
    ('ext (jj/sort-file-priority dir-contents))

    ;; all other helm sort methods
    (_ dir-contents)))

(after! helm
  (advice-add 'helm-list-directory :filter-return #'jj/helm-sort-files))

;; TODO; filter return values of vertico/embark/consult


(after! treemacs
  (setq treemacs-project-follow-cleanup t
        treemacs-follow-after-init t
        treemacs-width 30)

  ;; single-click expansion
  (define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action)

  (treemacs-project-follow-mode 1)

  (defun jj/treemacs-ignore-file-predicate (file _)
    (or (string= file ".gitignore")
        (string-suffix-p ".pyc" file)
        (string= file "__pycache__")
        (string-prefix-p ".cache" file)))
  (push #'jj/treemacs-ignore-file-predicate treemacs-ignored-file-predicates))

(after! neotree
  (setq neo-theme 'nerd-icons))


(defun jj/codenav-keybinds ()
  (interactive)
  (local-set-key (kbd "C-<mouse-1>") #'xref-find-definitions-at-mouse)
  (local-set-key (kbd "C-S-<mouse-1>") #'xref-find-references-at-mouse)
  (local-set-key (kbd "M-g d") #'xref-find-definitions)
  (local-set-key (kbd "M-g f") #'xref-find-references)
  (local-set-key (kbd "M-g G") #'xref-find-definitions-other-frame)

  ;; defaults:
  ;; M-, pop-tagmark in slime-nav-mode-map ->xref-pop-marker-stack
  ;; M-. evil-repeat-pop-next in evil-normal-state-map
  ;; M-. evil-slime-nav-find-elisp-thing-at-point
  ;; M-. xref-find-definitions in global-map

  ;; navigation on marker ring to go back and forward by xrefs
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "M-.") nil)
    (define-key evil-normal-state-map (kbd "M-,") nil))

  (if (<= emacs-major-version 28)
      (error "emacs <= 28 not supported for xref jumping")
    (progn
      ;; there's also evil-jump-backward/forward
      (local-set-key (kbd "M-.") #'xref-go-forward)
      (local-set-key (kbd "M-,") #'xref-go-back))))
