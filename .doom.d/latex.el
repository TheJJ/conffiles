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
        reftex-plug-into-AUCTeX t))

;; doom uses this list to register the pdfviewers non-lazily.
;; +latex-viewers is evaluated after! tex, too.
;; TODO: validate if this is run in the correct order....
(after! tex
  ;; reset the contents, since +latex-viewers will do add-to-list, and evince already is in the list
  ;; hence it won't be bumped to the front...
  (setq TeX-view-program-selection '())
  (setq +latex-viewers '(evince okular zathura pdf-tools)))


(after! ox-latex
  (add-to-list 'org-latex-packages-alist '("newfloat" "minted"))

  (add-to-list 'org-latex-classes
               '("acmart"
                 "\\documentclass[]{acmart}"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

;; synchronize bibliography customization settings to other packages
(after! ebib
  (sync-variable 'ebib-file-search-dirs 'bibtex-completion-library-path)
  (sync-variable 'ebib-preload-bib-files 'bibtex-completion-bibliography))

(after! citar
  (sync-variable 'citar-bibliography 'bibtex-completion-bibliography))
