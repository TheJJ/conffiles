;;; latex.el -*- lexical-binding: t; -*-



;; bibtex settings
(after! ebib
  ebib-bibtex-dialect 'biblatex
  ebib-index-default-sort '("Year" . descend)
  ebib-file-associations '())                   ; so find-file handles the opening

(after! bibtex
  (setq bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool  ; open with system viewer
        bibtex-completion-pdf-field "file")) ; field in bibtex file for pdf name

(after! LaTeX-mode
  (setq TeX-engine 'default  ;; or xetex, but conflicts with inputenc package
        TeX-PDF-mode t
        TeX-save-query nil
        TeX-parse-self t  ;; enable parse on load
        TeX-auto-save t   ;; enable parse on save
        reftex-plug-into-AUCTeX t

        ;; last is most preferred
        +latex-viewers '(pdf-tools zathura okular evince))

  (add-to-list 'org-latex-classes
               '("acmart"
                 "\\documentclass[]{acmart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))


(after! ox-latex
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted")))

;; synchronize bibliography customization settings to other packages
(after! ebib
  (sync-variable 'ebib-file-search-dirs 'bibtex-completion-library-path)
  (sync-variable 'ebib-preload-bib-files 'bibtex-completion-bibliography))

(after! citar
  (sync-variable 'citar-bibliography 'bibtex-completion-bibliography))
