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
