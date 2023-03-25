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
  "Regexp to match env vars in file."
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
  (setq exec-path (cl-remove-duplicates (mapcar #'directory-file-name exec-path)
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


(defun jj/import-shell-env ()
  "Loads environment variables from you user's shell"
  ;; execute the default shell twice and
  ;; fetch the environment variables with `env'.
  ;; in order to get the current env-vars (like the ssh-agent socket path)
  ;; on each emacs launch, this variant exists.
  ;; but why are we not happy with the env vars emacs got anyway from the kernel?
  ;; because in .zshrc, .bashrc, ... users usually define more, and would expect emacs
  ;; to know about them, even though they didn't launch emacs from their shell,
  ;; but their desktop environment instead.
  ;; we basically inject the shell-env into non-shell-launched emacs.
  ;;
  ;; important: the shell really has to export the env vars,
  ;;            and not exit before env vars are set because the shell
  ;;            is not interactive!
  (with-temp-buffer
    (let ((shell-command-switches (cond
                                   ((or (eq system-type 'darwin)
                                        (eq system-type 'cygwin)
                                        (eq system-type 'gnu/linux))
                                    ;; execute env twice, once with a
                                    ;; non-interactive login shell and
                                    ;; once with an interactive shell
                                    ;; in order to capture all the init
                                    ;; files possible.
                                    '("-lc" "-ic"))
                                   ((eq system-type 'windows-nt) '("-c"))))
          (executable (cond ((or (eq system-type 'darwin)
                                 (eq system-type 'cygwin)
                                 (eq system-type 'gnu/linux))
                             "env")
                            ((eq system-type 'windows-nt)
                             "set")
                            (t (warn "unsupported system type for fetching env: %s" system-type)
                               nil))))
      (let ((process-environment initial-environment)
            (env-point (point)))
        (dolist (shell-command-switch shell-command-switches)
          (call-process-shell-command executable nil t))
        ;; sort envvars and remove duplicates
        (sort-regexp-fields nil "^.*$" ".*?=" env-point (point-max))
        (delete-duplicate-lines env-point (point-max) nil t)
        ;; remove ignored environment variables
        (dolist (v spacemacs-ignored-environment-variables)
          (flush-lines v env-point (point-max)))))

    ;; apply env vars into emacs
    (let ((env-vars (load-env-vars-extract-env-vars)))
      (load-env-vars-set-env env-vars))))
