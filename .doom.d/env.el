;;; env.el -*- lexical-binding: t; -*-

;;; this is a bit similar to exec-path-from-shell
;;; inject environment content from user's default shell into emacs.
;;;
;;; yes, this may increase Emacs' startup time if your shell config is crappy.
;;; to fix this, do an early exit after setting variables with
;;;   [[ $- != *i* ]] && return
;;; this stops execution when running the shell non-interactively (which we do).


(defvar jj/env-re
  (rx
   line-start
   (0+ space)
   (optional "export" (0+ space)) ;; optional export
   (group (1+ (in "_" alnum))) ;; key
   (or
    (and (0+ space) "=" (0+ space))
    (and ":" (1+ space))) ;; separator
   (or
    line-start
    (and "'" (group (0+ (or "\\'" (not (any "'"))))) "'") ;; single quoted value
    (and ?\" (group (0+ (or "\\\"" (not (any "\""))))) ?\") ;; double quoted value
    (group (1+ (not (in "#" "\n")))) ;; unquoted value
    )
   (0+ space)
   (optional "#" (0+ any))
   )
  "Regexp to match env vars in a string returned in the output of `env`"
  )


(defun jj/env--set (env-vars)
  "Set environment variables from key value lists from ENV-VARS."

  (let ((convert-to-os-path (if (memq system-type '(windows-nt ms-dos))
                                (apply-partially #'subst-char-in-string ?/ ?\\)
                              #'identity))
        (convert-from-os-path (apply-partially #'subst-char-in-string ?\\ ?/)))

    (cl-loop
     for (key . value) in env-vars do

     (if (string-equal "PATH" key)
         (let ((paths (split-string value path-separator)))
           ;; clean and construct emacs'
           (setq exec-path
                 (cl-remove-duplicates
                  (mapcar #'directory-file-name
                          (append (mapcar convert-from-os-path paths)
                                  exec-path))
                  :test #'string-equal :from-end t))

           (setenv "PATH" (mapconcat convert-to-os-path exec-path path-separator)))

       (setenv key value)))))


(defvar jj/env-import-deny
  '("^_=" "^LS_COLORS=" "^INSIDE_EMACS=" "^\\(OLD\\)?PWD=" "^SHLVL=" "^PS1=" "^R?PROMPT=" "^TERM\\(CAP\\)?=")
  "Environment variables to omit from the shell environment import")

(defun jj/import-shell-env ()
  "Loads environment variables from you user's shell.

This executes the default shell and fetches the environment variables with `env'
in order to get the current env-vars (like the ssh-agent socket path)
on each emacs launch.
But why are we not happy with the env vars emacs got anyway from the kernel?
Because in .zshrc, .bashrc, ... users usually define more,
and would expect emacs to know about them, even though they didn't launch emacs
from their shell, but their desktop environment instead.
We basically inject the shell-env into non-shell-launched emacs.

Important:
The shell config has to export the env vars non-interactively - it must not exit
in its config file before env vars are set because the shell is not interactive!

In your zshrc this means:

export STUFF=...
# more env-vars
# don't execute the rest of the file in non-interactive mode.
[[ $- != *i* ]] && return
# rest of your zshrc
"
  (with-temp-buffer
    (let ((executable ;; what tool to run to extract the shell environment
           (cond ((or (eq system-type 'darwin)
                      (eq system-type 'cygwin)
                      (eq system-type 'gnu/linux))
                  "env")
                 ((eq system-type 'windows-nt)
                  "set")
                 (t (warn "unsupported system type for fetching env: %s" system-type)
                    nil))))
      (let ((process-environment initial-environment)
            (point-start (point)))
        ;; uses the default shell (shell-file-name)
        (call-process-shell-command executable nil t)

        ;; sort envvars and remove duplicates
        (sort-regexp-fields nil "^.*$" ".*?=" point-start (point-max))
        (delete-duplicate-lines point-start (point-max) nil t)

        ;; remove ignored environment variables
        (dolist (v jj/env-import-deny)
          (flush-lines v point-start (point-max)))))

    ;; apply env vars into emacs
    (let ((env-vars (jj/extract-kv-re-seq jj/env-re)))
      (jj/env--set env-vars)))

  ;; delete the `doom sync` generated env cache file to prevent loading it.
  (when (and doom-env-file
             (file-exists-p doom-env-file))
    (delete-file doom-env-file)))

;; instead, we import the environment by running the user's shell once.
(jj/import-shell-env)

;; in doom-start.el the cached doom-env-file is loaded if it's set.
(setq doom-env-file nil)
