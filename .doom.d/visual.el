;;; visual.el -*- lexical-binding: t; -*-

(after! idle-highlight-mode
  (setq idle-highlight-idle-time 0.2
        idle-highlight-visible-buffers t))

(after! centaur-tabs-mode
  (setq centaur-tabs-show-navigation-buttons t))

;; when exiting isearch, register the search term as regexp-highlight
(defadvice isearch-done (after ysph-hl-search activate compile)
           "highlight the search term after isearch has quit"
           (unhighlight-regexp t)
           (highlight-regexp (car (if isearch-regexp
                                    regexp-search-ring
                                    search-ring))
                             'lazy-highlight))

(after! whitespace-mode
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
        scrollbar-mode 'right
        auto-window-vscroll nil)

  ;; smooth scrolling :)
  (when (>= emacs-major-version 29)
    (pixel-scroll-precision-mode t)))
