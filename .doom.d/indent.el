;;; indent.el -*- lexical-binding: t; -*-


;; smart tabs - mix tabs and spaces :)
;; they need to be activated per programming mode.
(defmacro no-tabs-mode-advice (function)
  `(unless (ad-find-advice ',function 'around 'smart-tabs)
     (defadvice ,function (around smart-tabs activate)
       (if smart-tabs-mode
           (let ((indent-tabs-mode nil)) ad-do-it)
         ad-do-it))))


(define-minor-mode smart-tabs-mode
  "Indent with tabs, align with spaces!
   So cool, so good, so beautiful."

  :init-value nil

  (progn
    (no-tabs-mode-advice align)
    (no-tabs-mode-advice align-regexp)
    (no-tabs-mode-advice indent-relative)
    (no-tabs-mode-advice comment-dwim)
    (no-tabs-mode-advice comment-box)
    (no-tabs-mode-advice comment-indent)

    (unless
        (ad-find-advice 'indent-according-to-mode 'around 'smart-tabs)
      (defadvice indent-according-to-mode (around smart-tabs activate)
        (if smart-tabs-mode
            (let ((indent-tabs-mode indent-tabs-mode))
              (if (memq indent-line-function
                        '(indent-relative
                          indent-relative-maybe))
                  (setq indent-tabs-mode nil))
              ad-do-it)
          ad-do-it)))
    ))

(defmacro smart-tabs-advice (function offset)
  `(progn
     (defadvice ,function (around smart-tabs activate)
       (cond
        ((and smart-tabs-mode indent-tabs-mode)
         ;; remove spaces before or in between tabs
         (save-excursion
           (beginning-of-line)
           (while (looking-at "\t*\\( +\\)\t+")
             (replace-match "" nil nil nil 1)))
         (setq tab-width tab-width)
         (let
             (;;set tabwidth to really high value (fill-column)
              (tab-width fill-column)
              ;; set the offset-variable to the same high thing
              (,offset fill-column)
              ;;(wstart (window-start))
              )
           (unwind-protect
               (progn ad-do-it)
             ;;(set-window-start (selected-window) wstart)
             )))
        (t ad-do-it)))))


(defun indent-file (&optional fpath)
  "indent file stored at FPATH. default: use current buffer"
  (interactive)
  ;; TODO: inhibit most other modes, e.g. lsp-mode

  (let (openbuffer
        buffer-already-open)
    (if (not fpath)
        (setq buffer-already-open t)
      (setq openbuffer (get-file-buffer fpath))
      (if openbuffer
          (setq buffer-already-open t)
        (setq openbuffer (find-file-noselect fpath))
        (switch-to-buffer openbuffer)))

    (message "indenting %s..." (buffer-file-name))
    (indent-region (point-min) (point-max) nil)

    (when (not buffer-already-open)
      (save-buffer)
      (message "saved %s!" (buffer-file-name))
      (kill-buffer openbuffer))))


(defun jj/shift-text (beg end shift-block-fun shift-line-fun)
  "shift text in region or line using evil like S-v with < and > do in Vim.
  It takes special care of preserving or even extending the region to the moved text lines."
  (if (use-region-p)
      (progn
        (let ((point-at-end (< (mark) (point))))

          ;; fix up current region end to grab the whole line
          (if point-at-end
              (end-of-line)
            (beginning-of-line))

          ;; then fix up the other region end
          (exchange-point-and-mark)
          (if point-at-end
              (beginning-of-line)
            (end-of-line))

          ;; restore mark-point order
          (exchange-point-and-mark)

          (let ((linebeg (if point-at-end (mark) (point)))
                (lineend (if point-at-end (point) (mark))))
            ;; shift the text
            (save-mark-and-excursion
              (funcall shift-block-fun linebeg lineend)
              ;; "In Transient Mark mode, every buffer-modifying primitive sets deactivate-mark"
              ;; but we wanna keep it active :)
              (setq deactivate-mark nil)))))

    (funcall shift-line-fun 1)))

(defun jj/shift-left (beg end)
  (interactive "r")
  (jj/shift-text beg end #'evil-shift-left #'evil-shift-left-line))

(defun jj/shift-right (beg end)
  (interactive "r")
  (jj/shift-text beg end #'evil-shift-right #'evil-shift-right-line))
