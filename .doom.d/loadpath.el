;;; loadpath.el -*- lexical-binding: t; -*-


(defun jj/loadpath-discover ()
  "here manual elisp load paths can be defined.
directories in ~/.doom.d/themes/ and ~/.doom.d/lisp/ are automatically
added to the respective load-paths."
  (let* ((basepath doom-user-dir)
         (load-specs `((,(concat basepath "themes/") . "custom-theme-load-path")
                       (,(concat basepath "lisp/") . "load-path"))))
    (dolist (load-spec load-specs)
      (let ((load-dir (car load-spec))
            (load-var (cdr load-spec)))

        (message "trying %s" load-dir)
        (when (file-directory-p load-dir)
          (dolist (load-dir-file (directory-files load-dir))
            (let ((load-dir-path (expand-file-name (concat load-dir load-dir-file))))

              (when (and (not (or (equal load-dir-file ".")
                                  (equal load-dir-file "..")))
                         (file-directory-p load-dir-path))

                (add-to-list (intern load-var) load-dir-path)))))))))
