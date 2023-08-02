;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; jj's doom flavor
;; - aims to be hybrid vim+emacs experience
;; - without any person-bound specializations (so anyone can use it out of the box)
;; - platform-agnostic, should work on any machine with emacs+doom installed.
;;
;; run 'doom sync' after modifying this file!


;; Functions/macros to help with further configuration of Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys


(load! "loadpath.el")
(load! "env.el")
(load! "util.el")
(load! "batch.el")
(load! "indent.el")
(load! "codestyle.el")
(load! "completion.el")
(load! "org.el")
(load! "latex.el")
(load! "lsp.el")
(load! "mail.el")
(load! "hook.el")
(load! "settings.el")
(load! "hack.el")

;; device-specific settings, before executions
(load! "device-pre.el" nil t)

(jj/loadpath-discover)
(jj/defaults)
(jj/mode-hooks)

(when (not noninteractive)
  (load! "theme.el")
  (load! "visual.el")
  (load! "whitespace.el")
  (load! "navigation.el")
  (load! "keys.el")

  (jj/theming)
  (jj/scrolling)
  (jj/keybindings)
  (jj/whitespace-highlight))

;; device specific settings
(load! "device.el" nil t)
