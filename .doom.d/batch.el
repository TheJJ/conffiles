;;; batch.el -*- lexical-binding: t; -*-


(defun batch-indent ()
  "Run `indent-region' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.

Probably called with:
emacs -batch -l ~/.emacs.d/init.el --eval '(batch-indent)' file file file...

Each of them will be indented as if it was opened in the editor."

  (when (not noninteractive)
    (error "`batch-indent` only works in -batch mode"))

  ;; need to init some parts of emacs
  (run-hooks 'emacs-startup-hook)

  ;; command-line-args-left is what is left of the command line, from startup.el
  (message "starting batch indentation...")
  (defvar command-line-args-left)
  (let ((error nil))
    (while command-line-args-left
      (let ((filename (car command-line-args-left)))
        (if (file-directory-p (expand-file-name filename))
            (message "ignoring directory %s" filename)
          (if (null (indent-file filename))
              (setq error t))))
      (setq command-line-args-left (cdr command-line-args-left)))
    (kill-emacs (if error 1 0))))
