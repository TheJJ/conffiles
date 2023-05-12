;;; keys.el -*- lexical-binding: t; -*-

(defun jj/keybindings ()
  ;; support for emacs bindings in insert mode
  ;; this isn't very good, but missing keys are fixed below.
  (setq evil-disable-insert-state-bindings t)

  ;; vim insert-mode emacs compatibility improvements
  (define-key evil-insert-state-map (kbd "C-x C-s") #'save-buffer)
  (define-key evil-insert-state-map (kbd "C-x C-f") #'find-file)
  (define-key evil-insert-state-map (kbd "C-y") #'yank)
  (define-key evil-insert-state-map (kbd "C-k") #'jj/delete-line)
  (define-key evil-insert-state-map (kbd "C-S-k") #'jj/delete-line-backward)
  (define-key evil-insert-state-map (kbd "M-w") #'kill-ring-save)
  (define-key evil-insert-state-map (kbd "C-w") #'kill-region)
  (define-key evil-insert-state-map (kbd "C-SPC") #'set-mark-command)
  (define-key evil-insert-state-map (kbd "C-r") #'isearch-repeat-backward)
  (define-key evil-insert-state-map (kbd "M->") #'jj/shift-right)
  (define-key evil-insert-state-map (kbd "M-<") #'jj/shift-left)

  ;; enable the alternative leader key also in normal and visual states
  (setq doom-leader-alt-key-states '(normal visual motion emacs insert))

  ;; arrow key stuff
  (global-set-key (kbd "M-<left>")  #'windmove-left)
  (global-set-key (kbd "M-<right>") #'windmove-right)
  (global-set-key (kbd "M-<up>")    #'windmove-up)
  (global-set-key (kbd "M-<down>")  #'windmove-down)

  ;; word jumping
  (global-set-key (kbd "C-<left>")  #'backward-word)
  (global-set-key (kbd "C-<right>") #'forward-word)
  (global-set-key (kbd "C-<up>")    #'backward-paragraph)
  (global-set-key (kbd "C-<down>")  #'forward-paragraph)
  ;; no word jumping with shift - instead make shift-selection work in evil
  (define-key evil-insert-state-map (kbd "S-<left>") nil)
  (define-key evil-insert-state-map (kbd "S-<right>") nil)
  (define-key evil-insert-state-map (kbd "S-<up>") nil)
  (define-key evil-insert-state-map (kbd "S-<down>") nil)

  ;; line+region movement (like in org :)
  (global-set-key (kbd "M-S-<left>")  #'drag-stuff-left)
  (global-set-key (kbd "M-S-<right>") #'drag-stuff-right)
  (global-set-key (kbd "M-S-<up>")    #'drag-stuff-up)
  (global-set-key (kbd "M-S-<down>")  #'drag-stuff-down)

  ;; line nativation/deletion
  (global-set-key (kbd "C-k") #'jj/delete-line)
  (global-set-key (kbd "C-S-k") #'jj/delete-line-backward)
  (global-set-key (kbd "C-l") #'recenter-top-bottom)
  (global-set-key (kbd "C-j") #'nowrap-newline-and-indent)
  (global-set-key (kbd "C-S-<backspace>") #'jj/delete-whole-line)

  ;; word deletion (instead of killing)
  (global-set-key (kbd "C-<delete>")    #'jj/delete-word)
  (global-set-key (kbd "C-<backspace>") #'jj/delete-word-backward)

  ;; terminal fu
  (global-set-key (kbd "M-[ d") #'left-word)  ;backward-word
  (global-set-key (kbd "M-[ c") #'right-word) ;forward-word
  (global-set-key (kbd "M-[ a") #'backward-paragraph)
  (global-set-key (kbd "M-[ b") #'forward-paragraph)

  (map! :leader :desc "Search project" "p /" #'jj/projectsearch)
  (global-set-key (kbd "C-x C-b") #'bs-show) ; buffer selector
  (global-set-key (kbd "C-x M-b") #'speedbar)
  (global-set-key (kbd "C-c C-a") #'mark-whole-buffer)
  (global-set-key (kbd "C-x B") #'bury-buffer)
  (global-set-key (kbd "C-x E") #'apply-macro-to-region-lines)
  (global-set-key (kbd "C-x I") #'insert-buffer)

  ;; align the current region to = or whatever
  (global-set-key (kbd "M-A") #'align-current)

  (global-set-key (kbd "M-p") (lambda ()
                                (interactive)
                                (join-line -1)))

  ;; text shifting. evil-normal-state-map has these anyway.
  (define-key evil-emacs-state-map (kbd "M-<") #'jj/shift-left)
  (define-key evil-emacs-state-map (kbd "M->") #'jj/shift-right)

  ;; really insert a fucking tab
  (global-set-key (kbd "C-<tab>") 'insert-tab)

  ;; disable annoying character swapping
  (global-unset-key (kbd "C-t"))

  (after! org-keys
    ;; org's default C-M-arrow bindings are kinda useless,
    ;; so we use them so M-arrow is free for window movements!
    (define-key org-mode-map (kbd "C-M-<right>") #'org-metaright)
    (define-key org-mode-map (kbd "C-M-<left>") #'org-metaleft)
    (define-key org-mode-map (kbd "C-M-<up>") #'org-metaup)
    (define-key org-mode-map (kbd "C-M-<down>") #'org-metadown)
    ;; unset those in orgmodemap so the global window mappings are used
    (define-key org-mode-map (kbd "M-<right>") nil)
    (define-key org-mode-map (kbd "M-<left>") nil)
    (define-key org-mode-map (kbd "M-<up>") nil)
    (define-key org-mode-map (kbd "M-<down>") nil))

  (after! markdown-mode
    ;; markdown mode default C-M-arrow can also be overridden for window movements.
    (define-key markdown-mode-map (kbd "C-M-<right>") #'markdown-demote)
    (define-key markdown-mode-map (kbd "C-M-<left>") #'markdown-promote)
    (define-key markdown-mode-map (kbd "C-M-<up>") #'markdown-move-up)
    (define-key markdown-mode-map (kbd "C-M-<down>") #'markdown-move-down)
    (define-key markdown-mode-map (kbd "M-<right>") nil)
    (define-key markdown-mode-map (kbd "M-<left>") nil)
    (define-key markdown-mode-map (kbd "M-<up>") nil)
    (define-key markdown-mode-map (kbd "M-<down>") nil))


  (after! helm-for-files
    (global-set-key (kbd "C-x C-S-f") #'helm-for-files))

  (after! helm-files
    ;; otherwise it would do helm-ff-run-toggle-auto-update wat
    (define-key helm-find-files-map (kbd "C-<backspace>") nil))

  (after! helm
    (global-set-key (kbd "C-x b") #'helm-mini)
    (global-set-key (kbd "C-x j b") #'helm-bibtex)
    (global-set-key (kbd "C-S-s") #'helm-occur))

  (after! blacken
    (map! :leader :desc "Blacken Buffer" "m b b" #'python-black-buffer)
    (map! :leader :desc "Blacken Region" "m b r" #'python-black-region)
    (map! :leader :desc "Blacken Statement" "m b s" #'python-black-statement)))
