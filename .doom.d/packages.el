;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


(package! blacken)
(package! breadcrumb)
(package! casual)
(package! idle-highlight-mode)
(package! org-modern)
(package! org-ref)
(package! org-roam-bibtex)
(package! org-super-agenda)
(package! ripgrep)
(package! sql-indent)
(package! starlit-theme)

;; no snippets
;; better would be to not complete snippets on TAB, but C-x C-s or something.
;; some annoying snippets are hard-disabled in hack.el
(package! doom-snippets :ignore t :disable t)

;; to fix https://github.com/joaotavora/eglot/discussions/1127#discussioncomment-8017640
;; until a newer emacs >= 29.4 is released (or rather a newer jsonrpc is included)
(package! jsonrpc :pin "74268ee45494f40b3534164001b994e315490c27")

;; drag-stuff is useful even though we use evil
;; it was disabled in doom with 816db4a62addf7ac5e658123ba081069d224d310
(package! drag-stuff :pin "6d06d846cd37c052d79acd0f372c13006aa7e7c8")

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/radian-software/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see radian-software/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; device specific packages (e.g. for testing)
;;
;; example content to have a local non-byte-compiled repo instead of
;; an upstream emacs package (for development):
;;
;; (package! some-package
;;   :recipe (:local-repo "~/src/emacs/some-package" :build nil))
;;
(load! "device-packages.el" nil t)
