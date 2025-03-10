;;; visual.el -*- lexical-binding: t; -*-

(after! idle-highlight-mode
  (setq idle-highlight-idle-time 0.2
        idle-highlight-visible-buffers t))

(after! centaur-tabs-mode
  (setq centaur-tabs-show-navigation-buttons t))

(after! jit-lock-mode
  (setq jit-lock-stealth-time 0.3   ;; fontify unfontified areas when idle for this time
        jit-lock-stealth-nice 0.3   ;; time between fontifying chunks
        jit-lock-chunk-size 4096))  ;; number of characters to fontify at once

;; when exiting isearch, register the search term as regexp-highlight
(defadvice isearch-done (after ysph-hl-search activate compile)
           "highlight the search term after isearch has quit"
           (unhighlight-regexp t)
           (highlight-regexp (car (if isearch-regexp
                                    regexp-search-ring
                                    search-ring))
                             'lazy-highlight))

(after! whitespace-mode
  ;; synchronize the fill-column setting to the whitespace long line highlight
  (sync-variable 'whitespace-line-column 'fill-column))

(defun jj/scrolling ()
  ;; mouse-wheel scrolling
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                    ((control)))    ; one line at a time
        mouse-wheel-progressive-speed t             ; accelerate scrolling
        mouse-wheel-follow-mouse t)                 ; scroll- window under mouse

  (setq scroll-preserve-screen-position t           ; keep relative column position when scrolling
        scroll-margin 4                             ; start scrolling n lines before window borders
        scroll-conservatively 25                    ; scroll up to n lines to bring pointer back on screen
        scroll-step 0                               ; try scrolling n lines when pointer moves out
        auto-window-vscroll nil)

  ;; smooth scrolling :)
  (when (>= emacs-major-version 29)
    (pixel-scroll-precision-mode t)))
