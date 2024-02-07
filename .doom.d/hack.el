;;; hack.el -*- lexical-binding: t; -*-

;; the restart-emacs desktop-file restore function overrides "desktop-base-file-name" to (file-name-base arg)
;; -> the new name is ".emacs.desktop", so the .desktop gets stripped away.
;; proper fix would be not to use file-name-base there.
;; so we fake a extension that can happily be stripped away.
(after! restart-emacs
  (advice-add 'restart-emacs--restore-frames-using-desktop
              :filter-args
              (lambda (args) (let ((filename (car args)))
                               (list (concat filename ".justadirtyhack"))))))

;; disable some doom-snippets snippets
;; https://github.com/doomemacs/snippets
(after! yasnippet
  (after! doom-snippets
    (let ((disables-for-modes '((python-mode ("." "cl" "dec" "for" "s" "size"))
                                (c++-mode ("pack")))))
      (dolist (disables-for-mode disables-for-modes)
        (let ((table (yas--table-get-create (car disables-for-mode)))
              (uuids (cdr disables-for-mode)))

          (dolist (uuid uuids)
            (yas--remove-template-by-uuid table uuid)))))))
