;;; util.el -*- lexical-binding: t; -*-

(defun what-face (pos)
  "display the face of the char under point"
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name) (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


(defvar watch-buffer-command nil "Command to be executed upon buffer save.")
(make-variable-buffer-local 'watch-buffer-command)

(defun watch-buffer-run-command ()
  "execute the background shell for the command."
  (when watch-buffer-command
    (async-shell-command watch-buffer-command "*Watch-Process*")))

(defun watch-buffer (command)
  "Run command when saving this buffer."
  (interactive "sCommand: ")
  (setq watch-buffer-command command)
  (add-hook 'after-save-hook 'watch-buffer-run-command t t))

(defun unwatch-buffer ()
  "Disable previously defined watch-command."
  (interactive)
  (setq watch-buffer-command nil)
  (remove-hook 'after-save-hook 'watch-buffer-run-command t))


(defun insert-tab ()
  (interactive)
  (insert-char ?\t))

(defun tabs-disable ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun tabs-enable  ()
  (interactive)
  (setq indent-tabs-mode t))

(defun nowrap-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
  Prefixed with negative \\[universal-argument], sorts in reverse.

  The variable `sort-fold-case' determines whether alphabetic case
  affects the sort order.

  See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "[a-zA-Z0-9_-]+" "\\&" beg end))

(defun sort-words-nocase (reverse beg end)
  "Sort words in region alphabetically, case insensitively"
  (interactive "*P\nr")
  (let ((sort-fold-case t))
    (call-interactively 'sort-words)))

(defun sort-lines-nocase ()
  "Sort lines in region alphabetically, case insensitively"
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

(defun reload-dir-locals-current-buffer ()
  "reload the .dir-locals.el for the current buffer"
  (interactive)
  (message (format "reloading dir-locals for %s (%s)..." (current-buffer) buffer-file-name))
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun reload-dir-locals-projectile ()
  "For every buffer in the current projectile project,
reload dir-locals"
  (interactive)
  (let ((pdir (projectile-project-root)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal (projectile-project-root) pdir)
          (reload-dir-locals-current-buffer))))))

(defun reload-dir-locals-below-default-dir ()
  "Reload dir-locals for all buffers that are potentially affected
from a change in by prefix-matching the current buffer's `default-directory`"
  (interactive)
  (let ((cdir default-directory))
    (message (format "reloading dir-locals below %s..." cdir))
    (dolist (buffer (buffer-list))
      (when (buffer-file-name buffer)
        (with-current-buffer buffer
          ;; todo: symlink resolving...
          (when (string-prefix-p cdir buffer-file-name)
            (reload-dir-locals-current-buffer)))))))

(defun jj/projectsearch ()
  "Search for something in the current project."
  (interactive)
  (cond ((executable-find "rg")
         (call-interactively #'projectile-ripgrep))
        ((executable-find "ag")
         (call-interactively #'projectile-ag))
        (t (call-interactively #'projectile-grep))))


(defun jj/delete-word (arg)
  "forward delete until end of word.
  argument specifies repetitions.
  does not yank to kill-ring."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun jj/delete-word-backward (arg)
  "backward delete until end of word.
  argument specifies repetitions.
  does not yank to kill-ring."
  (interactive "p")
  (jj/delete-word (- arg)))


(defun jj/delete-line ()
  "delete until end of line.
  if point is at line-end, delete the whole line.
  does not yank to kill-ring."
  (interactive)
  (save-excursion
    (let ((orig-point (point)))
      (end-of-line)
      (if (= orig-point (point))
        (jj/delete-whole-line)
        (delete-region
          orig-point
          (line-end-position))))))

(defun jj/delete-line-backward ()
  "delete from beginning of line to point.
  does not yank to kill-ring."
  (interactive)
  (save-excursion
    (let ((orig-point (point)))
      (beginning-of-line 1)
      (delete-region
        (point)
        orig-point))))

(defun jj/delete-whole-line ()
  "delete the current line, from beginning to end of line.
  does not yank to kill-ring."
  (interactive)
  (save-excursion
    (delete-region
      (line-beginning-position)
      (line-end-position))
    (delete-char 1))) ; the newline


;; helper functions
(defmacro with-package (package-name &rest body)
  "if the given package is available, do something."
  `(when (package-installed-p ',package-name)
     ,@body))

(defun sync-variable (destinationvar sourcevar)
  "synchronize the state of a given variable to another one.
the value is copied when setting up the sync."

  (let ((updatefunc (lambda (symbol newval operation where)
                      (set destinationvar newval))))
    ;; install update-hook
    (add-variable-watcher sourcevar updatefunc)
    ;; call for initial update
    (funcall updatefunc nil (symbol-value sourcevar) nil nil)))


(defun append-to-list (list-var elements)
  "Append ELEMENTS to the end of LIST-VAR.

The return value is the new value of LIST-VAR."
  (unless (consp elements)
    (error "ELEMENTS must be a list"))
  (let ((list (symbol-value list-var)))
    (if list
        (setcdr (last list) elements)
      (set list-var elements)))
  (symbol-value list-var))


(defun display-dpi ()
  "return the display dpi, but xorg lies to us and reports 96 dpi,
because of 35af1299e73483eaf93d913a960e1d1738bc7de6"
  (let ((pixel-width (display-pixel-width))
        (mm-width (display-mm-width)))
    (if (and pixel-width mm-width)
      (round (/ pixel-width (/ mm-width 25.4)))
      96)))


(defun list-index (item list)
  "Get zero-indexed offset of ITEM in LIST, or nil if absent."
  (catch 'nth-elt
    (cl-loop for idx from 0
          for elem in list
          do (when (equal item elem)
               (throw 'nth-elt idx)))
    nil))

(defun double-list-index (item doublelist)
  "Get zero-indexed offset of ITEM in the outer DOUBLELIST of lists,
or nil if absent.
Example (double-list-index 5 '(1 (2 3) (4 5) 6)) == 2
"
  (catch 'nth-elt
    (cl-loop for idx from 0
          for elem in doublelist
          do
          (cond ((listp elem)
                 (let ((inneridx (list-index item elem)))
                   (when inneridx
                     (throw 'nth-elt idx))))
                (t
                 (when (equal item elem)
                   (throw 'nth-elt idx)))))
    nil))

(defun multi-hook-add (function hooks)
  "add a function to multiple hooks"
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defun jj/extract-kv-re-seq (regexp)
  "return an alist of REGEXP matches in a buffer.
group-1 becomes key,
group-2 3 or 4 becomes value."

  (save-excursion
    (goto-char (point-min))

    (save-match-data
      (let (matches)
        (while (re-search-forward regexp nil t)
          (let ((key (match-string-no-properties 1))
                (value (or (match-string-no-properties 2)
                           (match-string-no-properties 3)
                           (match-string-no-properties 4))))
            (push `(,key . ,value) matches)))
        matches))))
