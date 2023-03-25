;;; mail.el -*- lexical-binding: t; -*-


;; wanderlust email \o/
;; per-device config is in ~/.wl/config and folders
(after! wl
  (setq wl-stay-folder-window t                       ;; left folder pane
        wl-folder-window-width 25                     ;; toggle on/off with i
        elmo-localdir-folder-path "~/Mail"
        elmo-msgdb-directory "~/.wl/elmo/msgdb"
        elmo-network-session-idle-timeout 300
        elmo-imap4-use-modified-utf7 t

        wl-temporary-file-directory "~/.wl/tmp/"
        wl-init-file "~/.wl/config"
        wl-folders-file "~/.wl/folders"
        wl-alias-file "~/.wl/aliases"
        wl-x-face-file "~/.wl/xface"
        wl-address-file "~/.wl/addresses"))
