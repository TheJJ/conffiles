;;; env.el -*- lexical-binding: t; -*-

;; TODO doom has this env-from-file loading mechanism somewhere

(defvar load-env-vars-env-var-regexp
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
  "Regexp to match env vars in a string."
  )

(defun load-env-vars-re-seq (regexp)
  "Get a list of all REGEXP matches in a buffer."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (let (matches)
        (while (re-search-forward regexp nil t)
          (push (list (match-string-no-properties 1) (or (match-string-no-properties 2) (match-string-no-properties 3) (match-string-no-properties 4))) matches))
        matches))))

(defun load-env-vars-extract-env-vars ()
  "Extract environment variable name and value from STRING."
  (load-env-vars-re-seq load-env-vars-env-var-regexp))


(defun load-env-vars-set-env (env-vars)
  "Set environment variables from key value lists from ENV-VARS."
  (setq exec-path (cl-remove-duplicates
                   (mapcar #'directory-file-name exec-path)
                   :test #'string-equal :from-end t))
  (let ((convert-to-os-path (if (memq system-type '(windows-nt ms-dos))
                                (apply-partially #'subst-char-in-string ?/ ?\\)
                              ;; Assume that we start with forward slashes.
                              #'identity)))
    (dolist (element env-vars)
      (let ((key (car element)) (value (cadr element)))
        (if (string-equal "PATH" key)
            (let ((paths (split-string value path-separator)))
              (setq exec-path (cl-remove-duplicates
                               (append (mapcar (lambda (path) (directory-file-name (subst-char-in-string ?\\ ?/ path))) paths) exec-path)
                               :test #'string-equal :from-end t)
                    )
              (setenv "PATH" (mapconcat convert-to-os-path exec-path path-separator)))
          (setenv key value))))))


(defvar jj/env-import-deny
  '("^INSIDE_EMACS$" "^\\(OLD\\)?PWD$" "^SHLVL$" "^PS1$" "^R?PROMPT$" "^TERM\\(CAP\\)?$")
  "Environment variables to omit from the shell environment import")

(defun jj/import-shell-env ()
  "Loads environment variables from you user's shell.

This executes the default shell and fetches the environment variables with `env'
in order to get the current env-vars (like the ssh-agent socket path)
on each emacs launch.
But why are we not happy with the env vars emacs got anyway from the kernel?
Because in .zshrc, .bashrc, ... users usually define more, and would expect emacs
to know about them, even though they didn't launch emacs from their shell,
but their desktop environment instead.
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
            (env-point (point)))
        (call-process-shell-command executable nil t)
        ;; sort envvars and remove duplicates
        (sort-regexp-fields nil "^.*$" ".*?=" env-point (point-max))
        (delete-duplicate-lines env-point (point-max) nil t)
        ;; remove ignored environment variables
        (dolist (v jj/env-import-deny)
          (flush-lines v env-point (point-max)))))

    ;; apply env vars into emacs
    (let ((env-vars (load-env-vars-extract-env-vars)))
      (load-env-vars-set-env env-vars))))

;; in doom-start.el the cached doom-env-file is loaded if it's set.
;(setq doom-env-file nil)
;; instead, we import the environment by running the user's shell once.
;(jj/import-shell-env)
