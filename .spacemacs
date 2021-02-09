;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; vim: ft=lisp

;; jj's spacemacs configuration
;; Copyright (c) 2011-2021 Jonas Jelten <jj@sft.lol>
;; Licensed GPLv3 or later

(defun dotspacemacs/layers ()
  "Layer configuration:
This function should only modify configuration layer settings."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs

   ;; Lazy installation of layers (i.e. layers are installed only when a file
   ;; with a supported type is opened). Possible values are `all', `unused'
   ;; and `nil'. `unused' will lazy install only unused layers (i.e. layers
   ;; not listed in variable `dotspacemacs-configuration-layers'), `all' will
   ;; lazy install any layer that support lazy installation even the layers
   ;; listed in `dotspacemacs-configuration-layers'. `nil' disable the lazy
   ;; installation feature and you have to explicitly list a layer in the
   ;; variable `dotspacemacs-configuration-layers' to install it.
   ;; (default 'unused)
   dotspacemacs-enable-lazy-installation 'unused

   ;; If non-nil then Spacemacs will ask for confirmation before installing
   ;; a layer lazily. (default t)
   dotspacemacs-ask-for-lazy-installation t

   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()

   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; active layers select which of the magic of spacemacs
     ;; shall be activated.

     asm
     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-help-tooltip t)
     better-defaults
     bibtex
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-backend 'lsp-ccls)
            ;c-c++-backend 'rtags
            ;c-c++-enable-rtags-completion t)
     (cmake :variables
            cmake-enable-cmake-ide-support nil)
     csv
     dap
     emacs-lisp
     major-modes  ;; qml-mode, openscad
     git
     (gtags :variables
            gtags-enable-by-default nil)
     (haskell :variables
              haskell-completion-backend 'intero)
     helm
     html
     javascript
     (latex :variables
            latex-enable-auto-fill nil
            latex-enable-magic nil
            latex-enable-folding nil)
     lua
     (lsp :variables
          lsp-ui-remap-xref-keybindings t
          lsp-ui-sideline-enable nil
          lsp-ui-doc-enable nil)
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     multiple-cursors
     org
     python
     (ranger :variables
             ranger-show-preview t)
     restructuredtext
     rust
     salt
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     shell-scripts
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t
                     enable-flyspell-auto-completion nil)

     (sql :variables
          sql-capitalize-keywords t)
     systemd
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips t)
     theming
     treemacs
     version-control
     xclipboard
     yaml
     )

   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   ;; To use a local version of a package, use the `:location' property:
   ;; '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     afternoon-theme
     ag
     bison-mode
     google-c-style
     idle-highlight-mode
     pdf-tools
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     sqlup-mode
     wolfram-mode
     smartparens
     auto-highlight-symbol
     helm-company  ;; so C-/ is not mapped to it when completing...
     )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and deletes any unused
   ;; packages as well as their unused dependencies. `used-but-keep-unused'
   ;; installs only the used packages but won't delete unused ones. `all'
   ;; installs *all* packages supported by Spacemacs and never uninstalls them.
   ;; (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization:
This function is called at the very beginning of Spacemacs startup,
before layer configuration.
It should only modify the values of Spacemacs settings."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non-nil then enable support for the portable dumper. You'll need
   ;; to compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;; (default nil)
   dotspacemacs-enable-emacs-pdumper nil

   ;; Name of executable file pointing to emacs 27+. This executable must be
   ;; in your PATH.
   ;; (default "emacs")
   dotspacemacs-emacs-pdumper-executable-file "emacs"

   ;; Name of the Spacemacs dump file. This is the file will be created by the
   ;; portable dumper in the cache directory under dumps sub-directory.
   ;; To load it when starting Emacs add the parameter `--dump-file'
   ;; when invoking Emacs 27.1 executable on the command line, for instance:
   ;;   ./emacs --dump-file=$HOME/.emacs.d/.cache/dumps/spacemacs-27.1.pdmp
   ;; (default (format "spacemacs-%s.pdmp" emacs-version))
   dotspacemacs-emacs-dumper-dump-file (format "spacemacs-%s.pdmp" emacs-version)

   ;; If non-nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t

   ;; Maximum allowed time in seconds to contact an ELPA repository.
   ;; (default 5)
   dotspacemacs-elpa-timeout 5

   ;; Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
   ;; This is an advanced option and should not be changed unless you suspect
   ;; performance issues due to garbage collection operations.
   ;; (default '(100000000 0.1))
   dotspacemacs-gc-cons '(100000000 0.1)

   ;; Set `read-process-output-max' when startup finishes.
   ;; This defines how much data is read from a foreign process.
   ;; Setting this >= 1 MB should increase performance for lsp servers
   ;; in emacs 27.
   ;; (default (* 1024 1024))
   dotspacemacs-read-process-output-max (* 1024 1024)

   ;; If non-nil then Spacelpa repository is the primary source to install
   ;; a locked version of packages. If nil then Spacemacs will install the
   ;; latest version of packages from MELPA. Spacelpa is currently in
   ;; experimental state please use only for testing purposes.
   ;; (default nil)
   dotspacemacs-use-spacelpa nil

   ;; If non-nil then verify the signature for downloaded Spacelpa archives.
   ;; (default t)
   dotspacemacs-verify-spacelpa-archives t

   ;; If non-nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil

   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'. (default 'emacs-version)
   dotspacemacs-elpa-subdirectory 'emacs-version

   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid

   ;; If non-nil show the version string in the Spacemacs buffer. It will
   ;; appear as (spacemacs version)@(emacs version)
   ;; (default t)
   dotspacemacs-startup-buffer-show-version t

   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official

   ;; List of items to show in startup buffer or an association list of
   ;; the form `(list-type . list-size)`. If nil then it is disabled.
   ;; Possible values for list-type are:
   ;; `recents' `recents-by-project' `bookmarks' `projects' `agenda' `todos'.
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   ;; The exceptional case is `recents-by-project', where list-type must be a
   ;; pair of numbers, e.g. `(recents-by-project . (7 .  5))', where the first
   ;; number is the project limit and the second the limit on the recent files
   ;; within a project.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))

   ;; True if the home buffer should respond to resize events. (default t)
   dotspacemacs-startup-buffer-responsive t

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent nil

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(afternoon
                         deeper-blue
                         reverse
                         spacemacs-dark
                         monokai)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               ;;:size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)

   ;; The leader key (default "SPC")
   dotspacemacs-leader-key "SPC"

   ;; The key used for Emacs commands `M-x' (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"

   ;; The key used for Vim Ex commands (default ":")
   dotspacemacs-ex-command-key ":"

   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"

   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","

   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m" for terminal mode, "<M-return>" for GUI mode).
   ;; Thus M-RET should work as leader key in both GUI and terminal modes.
   ;; C-M-m also should work in terminal mode, but not in GUI mode.
   dotspacemacs-major-mode-emacs-leader-key (if window-system "<M-return>" "C-M-m")

   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs `C-i', `TAB' and `C-m', `RET'.
   ;; Setting it to a non-nil value, allows for separate commands under `C-i'
   ;; and TAB or `C-m' and `RET'.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil

   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"

   ;; If non-nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil

   ;; If non-nil then the last auto saved layouts are resumed automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil

   ;; If non-nil, auto-generate layout name when creating new layouts. Only has
   ;; effect when using the "jump to layout by number" commands. (default nil)
   dotspacemacs-auto-generate-layout-names nil

   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1

   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache

   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5

   ;; If non-nil, the paste transient-state is enabled. While enabled, after you
   ;; paste something, pressing `C-j' and `C-k' several times cycles through the
   ;; elements in the `kill-ring'. (default nil)
   dotspacemacs-enable-paste-transient-state nil

   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4

   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom

   ;; Control where `switch-to-buffer' displays the buffer. If nil,
   ;; `switch-to-buffer' displays the buffer in the current window even if
   ;; another same-purpose window is available. If non-nil, `switch-to-buffer'
   ;; displays the buffer in a same-purpose window even if the buffer can be
   ;; displayed in the current window. (default nil)
   dotspacemacs-switch-to-buffer-prefers-purpose nil

   ;; If non-nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil

   ;; If non-nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil

   ;; If non-nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil

   ;; If non-nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil

   ;; If non-nil the frame is undecorated when Emacs starts up. Combine this
   ;; variable with `dotspacemacs-maximized-at-startup' in OSX to obtain
   ;; borderless fullscreen. (default nil)
   dotspacemacs-undecorated-at-startup nil

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90

   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90

   ;; If non-nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t

   ;; If non-nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t

   ;; If non-nil unicode symbols are displayed in the mode line.
   ;; If you use Emacs as a daemon and wants unicode characters only in GUI set
   ;; the value to quoted `display-graphic-p'. (default t)
   dotspacemacs-mode-line-unicode-symbols t

   ;; If non-nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but lines are only visual lines are counted. For example, folded lines
   ;; will not be counted and wrapped lines are counted as multiple lines.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :visual nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; When used in a plist, `visual' takes precedence over `relative'.
   ;; (default nil)
   dotspacemacs-line-numbers t

   ;; Code folding method. Possible values are `evil', `origami' and `vimish'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil

   ;; If non-nil `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc...
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil

   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all

   ;; If non-nil, start an Emacs server if one is not already running.
   ;; (default nil)
   dotspacemacs-enable-server nil

   ;; Set the emacs server socket location.
   ;; If nil, uses whatever the Emacs default is, otherwise a directory path
   ;; like \"~/.emacs.d/server\". It has no effect if
   ;; `dotspacemacs-enable-server' is nil.
   ;; (default nil)
   dotspacemacs-server-socket-dir nil

   ;; If non-nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil

   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `rg', `ag', `pt', `ack' and `grep'.
   ;; (default '("rg" "ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("rg" "ag" "pt" "ack" "grep")

   ;; Format specification for setting the frame title.
   ;; %a - the `abbreviated-file-name', or `buffer-name'
   ;; %t - `projectile-project-name'
   ;; %I - `invocation-name'
   ;; %S - `system-name'
   ;; %U - contents of $USER
   ;; %b - buffer name
   ;; %f - visited file name
   ;; %F - frame name
   ;; %s - process status
   ;; %p - percent of buffer above top of window, or Top, Bot or All
   ;; %P - percent of buffer above bottom of window, perhaps plus Top, or Bot or All
   ;; %m - mode name
   ;; %n - Narrow if appropriate
   ;; %z - mnemonics of buffer, terminal, and keyboard coding systems
   ;; %Z - like %z, but including the end-of-line format
   ;; (default "%I@%S")
   dotspacemacs-frame-title-format "%I@%S"

   ;; Format specification for setting the icon title format
   ;; (default nil - same as frame-title-format)
   dotspacemacs-icon-title-format nil

   ;; Show trailing whitespace (default t)
   dotspacemacs-show-trailing-whitespace t

   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed' to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil

   ;; If non nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfer with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; If non-nil shift your number row to match the entered keyboard layout
   ;; (only in insert state). Currently supported keyboard layouts are:
   ;; `qwerty-us', `qwertz-de' and `querty-ca-fr'.
   ;; New layouts can be added in `spacemacs-editing' layer.
   ;; (default nil)
   dotspacemacs-swap-number-row nil

   ;; Either nil or a number of seconds. If non-nil zone out after the specified
   ;; number of seconds. (default nil)
   dotspacemacs-zone-out-when-idle nil

   ;; Run `spacemacs/prettify-org-buffer' when
   ;; visiting README.org files of Spacemacs.
   ;; (default nil)
   dotspacemacs-pretty-docs nil

   ;; If nil the home buffer shows the full path of agenda items
   ;; and todos. If non nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (spacemacs/load-spacemacs-env))


;;####################################################
;; jj-improvements
;;####################################################

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; conditional package settings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-library (symbol &rest body)
  "when the library is available, do things with it."
  `(condition-case nil
       (progn (require ',symbol) ,@body)
     (error (message (format "package unavailable: %s" ',symbol))
            nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; advices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; preserve undo-region selection
(defadvice undo-tree-undo (around keep-region activate)
  (if (use-region-p)
      (let ((m (set-marker (make-marker) (mark)))
            (p (set-marker (make-marker) (point))))
        ad-do-it
        (goto-char p)
        (set-mark m)
        (set-marker p nil)
        (set-marker m nil))
    ad-do-it))

;; don't disable the window on pressing C-z
(defadvice iconify-or-deiconify-frame (around disable-xframe-suspending))
(ad-activate 'iconify-or-deiconify-frame)

;; when exiting isearch, register the search term as regexp-highlight
(defadvice isearch-done (after ysph-hl-search activate compile)
           "highlight the search term after isearch has quit"
           (unhighlight-regexp t)
           (highlight-regexp (car (if isearch-regexp
                                    regexp-search-ring
                                    search-ring)) 'lazy-highlight))

;; git-gutter+ doesn't properly handle tramp-connections
;; https://github.com/nonsequitur/git-gutter-plus/issues/42
(with-eval-after-load 'git-gutter+
   (defun git-gutter+-remote-default-directory (dir file)
     (let* ((vec (tramp-dissect-file-name file))
            (method (tramp-file-name-method vec))
            (user (tramp-file-name-user vec))
            (domain (tramp-file-name-domain vec))
            (host (tramp-file-name-host vec))
            (port (tramp-file-name-port vec)))
       (tramp-make-tramp-file-name method user domain host port dir)))

   (defun git-gutter+-remote-file-path (dir file)
     (let ((file (tramp-file-name-localname (tramp-dissect-file-name file))))
       (replace-regexp-in-string (concat "\\`" dir) "" file))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart tabs, mix tabs and spaces (fak yea)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro no-tabs-mode-advice (function)
  `(unless (ad-find-advice ',function 'around 'smart-tabs)
     (defadvice ,function (around smart-tabs activate)
       (if smart-tabs-mode
           (let ((indent-tabs-mode nil)) ad-do-it)
         ad-do-it))))


(define-minor-mode smart-tabs-mode
  "Indent with tabs, align with spaces!
   So cool, so good, so beautiful."

  :init-value nil

  (progn
    (no-tabs-mode-advice align)
    (no-tabs-mode-advice align-regexp)
    (no-tabs-mode-advice indent-relative)
    (no-tabs-mode-advice comment-dwim)
    (no-tabs-mode-advice comment-box)
    (no-tabs-mode-advice comment-indent)

    (unless
        (ad-find-advice 'indent-according-to-mode 'around 'smart-tabs)
      (defadvice indent-according-to-mode (around smart-tabs activate)
        (if smart-tabs-mode
            (let ((indent-tabs-mode indent-tabs-mode))
              (if (memq indent-line-function
                        '(indent-relative
                          indent-relative-maybe))
                  (setq indent-tabs-mode nil))
              ad-do-it)
          ad-do-it)))
    ))

(defmacro smart-tabs-advice (function offset)
  `(progn
     (defadvice ,function (around smart-tabs activate)
                (cond
                  ((and smart-tabs-mode indent-tabs-mode)
                    ;remove spaces before or in between tabs
                    (save-excursion
                      (beginning-of-line)
                      (while (looking-at "\t*\\( +\\)\t+")
                             (replace-match "" nil nil nil 1)))
                    (setq tab-width tab-width)
                    (let
                        (;;set tabwidth to really high value (fill-column)
                         (tab-width fill-column)
                         (,offset fill-column)
                         ;;(wstart (window-start))
                         )
                      (unwind-protect
                        (progn ad-do-it)
                        ;;(set-window-start (selected-window) wstart)
                        )))
                  (t ad-do-it)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; setup (un)funny modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jj/modes ()
  (column-number-mode t)
  (cua-selection-mode t)
  (delete-selection-mode t)
  (display-battery-mode t)
  (xterm-mouse-mode t)
  (icomplete-mode t)
  (editorconfig-mode t)

  (put 'scroll-left 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  )


(defun jj/defaults ()

  (message "setting up 'sane' defaults")

  ;; push the mouse out of the way when the cursor approaches.
  (mouse-avoidance-mode 'cat-and-mouse)

  (setq-default visible-bell nil            ; disable window flashing
                ring-bell-function 'ignore) ; and also disable the sound

  (setq indicate-empty-lines t
        transient-mark-mode t
        gud-tooltip-mode t
        lazy-highlight t                 ; highlight occurrences
        lazy-highlight-cleanup nil       ; keep search term highlighted
        lazy-highlight-max-at-a-time nil ; all occurences in file
        isearch-allow-scroll t           ; continue the search even though we're scrolling
        font-lock-maximum-decoration t   ; decoration level: maximum
        auto-compression-mode t          ; deal with compressed files
        blink-cursor-mode nil            ; don't blink the cursor
        mouse-yank-at-point t            ; paste as cursor instead of mouse position
        inhibit-startup-screen t
        python-fill-docstring-style 'symmetric
        scrollbar-mode 'right
        backward-delete-char-untabify-method nil
        cua-auto-tabify-rectangles nil
        cua-enable-cua-keys nil
        ranger-show-literal t            ; colored ranger previews
        python-shell-prompt-detect-failure-warning nil
        python-shell-interpreter "ipython3" ; tramp on remote-hosts needs ipython3 and python3-setuptools
        ;python-shell-interpreter-interactive-arg ""
        compilation-environment (quote ("TERM=xterm-256color"))
        lsp-enable-on-type-formatting nil  ; using t funnily changes screen content whenever lsp thinks it can do "formatting"
        lsp-enable-file-watchers nil       ; lsp server can do inotify itself, but that may slow emacs down (https://github.com/MaskRay/ccls/issues/354)
        tramp-ssh-controlmaster-options    ; synced with .ssh/config ControlMaster settings
          (concat "-o ControlPath=/tmp/ssh_mux_%%u@%%l_%%r@%%h:%%p "
                  "-o ControlMaster=auto -o ControlPersist=10")
        lsp-diagnostic-package :none     ; disable lsp diagnostics for performance reasons (flycheck/flymake)
        helm-ff-file-name-history-use-recentf t
        history-delete-duplicates t      ; helm history duplicate removal
        )

  ;; default mode for new buffers
  (setq-default major-mode 'text-mode)
  (setq initial-major-mode 'text-mode)

  ;; don't enable auto-newline mode (c-toggle-auto-newline)
  (remove-hook 'c-mode-common-hook 'spacemacs//c-toggle-auto-newline )

  ;; indentation defaults
  ;;(setq-default indent-tabs-mode t)
  ;;(setq-default indent-line-function 'insert-tab)
  ;;(setq-default tab-width 4)
  (setq-default whitespace-line-column 400)

  ;; hide modeline indicators
  (spacemacs|diminish anaconda-mode)
  (spacemacs|diminish auto-revert-mode)
  (spacemacs|diminish evil-mc-mode)
  (spacemacs|diminish helm-gtags-mode)
  (spacemacs|diminish hybrid-mode)
  (spacemacs|diminish which-key-mode)
  (spacemacs|diminish yas-minor-mode)
  (spacemacs|diminish global-whitespace-mode)
  (spacemacs|diminish company-mode)


  ;; whitespace crimes
  (jj/whitespace-highlight)


  ;; backup files
  (setq make-backup-files nil
        backup-by-copying-when-mismatch t
        backup-by-copying-when-linked t
        ;;backup-by-copying t
        ;;backup-directory-alist
        ;;'(("." . "~/.saves"))
        ;;delete-old-versions t
        ;;kept-new-versions 6
        ;;kept-old-versions 2
        ;;version-control t
        ;;make-backup-files nil ; backup~ files
        ;;auto-save-default nil ; #autosave# files
        )


  ;; utf-8 ftw
  (prefer-coding-system 'utf-8)

  ;; link to X primary clipboard
  (setq x-select-enable-clipboard t)

  ;; require a ending newline
  (setq require-final-newline t) ; 'query will ask

  ;; display various non-editing buffers in their own frames
  (setq special-display-buffer-names
        (nconc '("*VC-log*" "*compilation*" "*grep*")
               special-display-buffer-names))

  ;; no tool bar for these buffers
  (add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))

  ;; don't echo passwords when using interactive terminal programs
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

  ;; man pages with clickable links
  (add-hook 'Man-mode-hook 'goto-address)

  ;; no shell path warning
  (setq exec-path-from-shell-check-startup-file nil)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fight the whitespace crimes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj/whitespace-highlight ()
  ;; see whitespace.el
  (interactive)

  ;; whitespace config
  (setq whitespace-display-mappings
        '(
          ;;(space-mark   ?\     [?\u00B7]     [?.])      ; space - centered dot
          (space-mark   ?\xA0  [?\u00A4]   [?_])          ; hard space - currency
          ;;(newline-mark ?\n    [?¬ ?\n]    [?$ ?\n])      ; eol - ¬ symbol
          (tab-mark     ?\t    [?∘ ?\t]    [?> ?\t]))     ; tab - ∘ symbol
        whitespace-style '(face tabs trailing
                                newline indentation
                                space-before-tab space-after-tab
                                space-mark tab-mark newline-mark lines-tail))
  (global-whitespace-mode t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; better mouse scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj/mousescroll ()
  ;;mouse-wheel scrolling
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                      ((control)))            ; one line at a time
        mouse-wheel-progressive-speed t                       ; accelerate scrolling
        mouse-wheel-follow-mouse t)                           ; scroll- window under mouse

  (setq scroll-preserve-screen-position t                     ; keep relative column position when scrolling
        scroll-margin 4                                       ; start scrolling n lines before window borders
        scroll-conservatively 10                              ; scroll up to n lines to bring pointer back on screen
        scroll-step 0                                         ; try scrolling n lines when pointer moves out
        auto-window-vscroll nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code symbol navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj/codenav-keybinds ()
  (interactive)
  (local-set-key [C-mouse-1] 'xref-find-definitions)
  (local-set-key (kbd "M-g d") 'xref-find-definitions)
  (local-set-key (kbd "M-g D") 'xref-find-definitions-other-frame)
  (local-set-key (kbd "M-g f") 'xref-find-references)
  ;; lsp-ui-peek-mode does not highlight the relevant line,
  ;; so it's currently rather useless, but may be cool in the future.
  ;(local-set-key (kbd "M-g D") 'lsp-ui-peek-find-definitions)
  ;(local-set-key (kbd "M-g F") 'lsp-ui-peek-find-references)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; funny functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name) (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defvar watch-buffer-command nil "Command to be executed upon buffer save.")
(make-variable-buffer-local 'watch-buffer-command)

(defun watch-buffer-run-command ()
  "execute the background shell for the command."
  (when watch-buffer-command
    (async-shell-command watch-buffer-command "*Watch-Process*")))

(defun watch-buffer (command)
  "Run command when saving this buffer."
  (interactive "sCommand: ")
  (setq watch-buffer-command command)
  (add-hook 'after-save-hook 'watch-buffer-run-command t t))

(defun unwatch-buffer ()
  "Disable previously defined watch-command."
  (interactive)
  (setq watch-buffer-command nil)
  (remove-hook 'after-save-hook 'watch-buffer-run-command t))

(defun insert-tab ()
  (interactive)
  (insert-char ?\t))

(defun tabs-disable ()
  (interactive)
  (setq indent-tabs-mode nil))

(defun tabs-enable  ()
  (interactive)
  (setq indent-tabs-mode t))

(defun nowrap-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun shift-text (distance)
  "Move a block of text to the right or left"
  (if (use-region-p)
    (let ((mark (mark)))
      (save-excursion
        (indent-rigidly (region-beginning)
                        (region-end)
                        distance)
        (push-mark mark t t)
        (setq deactivate-mark nil)))
    (indent-rigidly (line-beginning-position)
                    (line-end-position)
                    distance)))

(defun shift-right (count)
  (interactive "p")
  (shift-text count))

(defun shift-left (count)
  (interactive "p")
  (shift-text (- count)))


(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
    (minibuffer-complete)
    (if (check-expansion)
      (company-complete-common)
      (indent-for-tab-command))))


(defun sort-words (reverse beg end)
  "Sort words in region alphabetically, in REVERSE if negative.
  Prefixed with negative \\[universal-argument], sorts in reverse.

  The variable `sort-fold-case' determines whether alphabetic case
  affects the sort order.

  See `sort-regexp-fields'."
  (interactive "*P\nr")
  (sort-regexp-fields reverse "[a-zA-Z0-9_-]+" "\\&" beg end))

(defun sort-words-nocase (reverse beg end)
  "Sort words in region alphabetically, case insensitively"
  (interactive "*P\nr")
  (let ((sort-fold-case t))
    (call-interactively 'sort-words)))

(defun sort-lines-nocase ()
  "Sort lines in region alphabetically, case insensitively"
  (interactive)
  (let ((sort-fold-case t))
    (call-interactively 'sort-lines)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set kaschtomaisd key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj/delete-word (arg)
  "forward delete until end of word.
  argument specifies repetitions.
  does not yank to kill-ring."
  (interactive "p")
  (delete-region
   (point)
   (progn
     (forward-word arg)
     (point))))

(defun jj/delete-word-backward (arg)
  "backward delete until end of word.
  argument specifies repetitions.
  does not yank to kill-ring."
  (interactive "p")
  (jj/delete-word (- arg)))

(defun jj/delete-line ()
  "delete until end of line.
  if point is at line-end, delete the whole line.
  does not yank to kill-ring."
  (interactive)
  (save-excursion
    (let ((orig-point (point)))
      (move-end-of-line 1)
      (if (= orig-point (point))
        (jj/delete-whole-line)
        (delete-region
          orig-point
          (line-end-position))))))

(defun jj/delete-line-backward ()
  "delete from beginning of line to point.
  does not yank to kill-ring."
  (interactive)
  (save-excursion
    (let ((orig-point (point)))
      (beginning-of-line 1)
      (delete-region
        (point)
        orig-point))))

(defun jj/delete-whole-line ()
  "delete the current line, from beginning to end of line.
  does not yank to kill-ring."
  (interactive)
  (save-excursion
    (delete-region
      (line-beginning-position)
      (line-end-position))
    (delete-char 1) ; the newline
    ))


(defun jj/keybindings ()
  (interactive)

  ;; arrow key stuff
  (global-set-key (kbd "M-<left>")  'windmove-left)
  (global-set-key (kbd "M-<right>") 'windmove-right)
  (global-set-key (kbd "M-<up>")    'windmove-up)
  (global-set-key (kbd "M-<down>")  'windmove-down)

  (global-set-key (kbd "C-<left>")  'backward-word)
  (global-set-key (kbd "C-<right>") 'forward-word)
  (global-set-key (kbd "C-<up>")    'backward-paragraph)
  (global-set-key (kbd "C-<down>")  'forward-paragraph)

  ;; line nativation/deleteion
  (global-set-key (kbd "C-k") 'jj/delete-line)
  (global-set-key (kbd "C-S-k") 'jj/delete-line-backward)
  (global-set-key (kbd "C-l") 'recenter-top-bottom)
  (global-set-key (kbd "C-j") 'nowrap-newline-and-indent)
  (global-set-key (kbd "C-S-<backspace>") 'jj/delete-whole-line)

  ;; word deletion
  (global-set-key (kbd "C-<delete>")    'jj/delete-word)
  (global-set-key (kbd "C-<backspace>") 'jj/delete-word-backward)

  ;; terminal fu
  (global-set-key (kbd "M-[ d") 'left-word)  ;backward-word
  (global-set-key (kbd "M-[ c") 'right-word) ;forward-word
  (global-set-key (kbd "M-[ a") 'backward-paragraph)
  (global-set-key (kbd "M-[ b") 'forward-paragraph)

  ;; newline magic
  (global-set-key (kbd "<C-return>") 'newline)
  (global-set-key (kbd "C-c C-a") 'mark-whole-buffer)

  ;; the best™ file chooser!!11
  (use-package helm-for-files
    :defer t
    :init (progn
            (global-set-key (kbd "C-x C-S-f") 'helm-for-files)
            (spacemacs||set-helm-key "fF" helm-for-files)))

  (global-set-key (kbd "C-x b") 'helm-mini)

  (global-set-key (kbd "C-x B") 'bury-buffer)
  (global-set-key (kbd "C-x E") 'apply-macro-to-region-lines)
  (global-set-key (kbd "C-x I") 'insert-buffer)
  (global-set-key (kbd "C-c g") 'goto-line)
  (global-set-key (kbd "C-c G") 'goto-char)
  (global-set-key (kbd "C-c w") 'delete-region) ; ala C-w and M-C-w
  (global-set-key (kbd "C-c c") 'comment-region)
  (global-set-key (kbd "C-c u") 'uncomment-region)
  (global-set-key (kbd "C-c n") 'next-error)
  (global-set-key (kbd "C-c p") 'previous-error)

  (global-set-key (kbd "C-x C-b") 'bs-show) ; buffer selector
  (global-set-key (kbd "C-x M-b") 'speedbar)

  ;; align the current region to = or whatever
  (global-set-key (kbd "M-A") 'align-current)

  (global-set-key (kbd "C-c g") 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)

  ;;(global-set-key (kbd "C-v") 'cua-set-rectangle-mark) ;rectangle-select
  (global-set-key (kbd "M-SPC") 'just-one-space) ;fold space to 1

  (global-set-key (kbd "M-p") (lambda ()
                                (interactive)
                                (join-line -1)))

  ;; vim-like shifting
  (global-set-key (kbd "M->") (lambda ()
                                (interactive)
                                (shift-right 4)))
  (global-set-key (kbd "M-<") (lambda ()
                                (interactive)
                                (shift-left 4)))

  ;; really insert a fucking tab
  (global-set-key (kbd "C-<tab>") 'insert-tab)

  ;; force company completion:
  (global-set-key (kbd "S-<tab>") 'tab-indent-or-complete)

  ;; TODO: wrap the xref--marker-ring so we can go back and forward with M-, and M-.
  ;; also replace the evil-normal-state-map binding so we don't need to enter insert mode then...

  ;;unset unneeded keys
  ;;(global-unset-key (kbd "C-t")) ; annoying character swapping

  (fset 'yes-or-no-p 'y-or-n-p) ; yes/no answering without <RET>
  )


(defun jj/cstyle-keybinds ()
  (interactive)
  (local-set-key (kbd "C-c C-c") 'helm-make-projectile))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coding style definitions
;; ------------------------
;;
;; see all the possible variables at [emacsshare]/lisp/progmodes/cc-vars.el
;; c-set-stylevar-fallback 'c-offsets-alist
;;
;; get syntax/indent info by C-c C-s
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jj/create-codestyles ()
  ;; codestyle definitions

  ;; linux kernel indentation style
  (c-add-style
   "linux-kernel"
   '("linux" ;; based on the builtin linux style
     (c-offsets-alist . (
                         (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist)
                         (arglist-close . 0)
                         ))
     ))

  ;; closing template <> should line up.
  (defun c++-template-args-cont (langelem)
    "Indentation of template params for a closing '>'.
    return values:
    0   : If the first non-whitespace char is '>'. Line it up under 'template'.
    nil : Otherwise, return nil and run next lineup function."
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "^[\t ]*>" (line-end-position) t)
        0)))

  ;; sft coding style
  (defconst sft-c-style
    '("linux"  ;; base it on linux code style
      (c-doc-comment-style        . javadoc)
      (c-block-comment-prefix     . " * ")
      (indent-tabs-mode           . t)
      (c-basic-offset             . 4)
      (c-tab-always-indent        . t)
      (c-comment-only-line-offset . 4)
      (c-hanging-colons-alist     . ((access-label after)
                                     (case-label after)
                                     (inher-intro)
                                     (label after)
                                     (member-init-intro before)))
      (c-cleanup-list             . (brace-else-brace
                                     brace-elseif-brace
                                     brace-catch-brace
                                     empty-defun-braces
                                     defun-close-semi
                                     list-close-comma
                                     scope-operator))
      (c-comment-only-line-offset . 0)
      (c-hanging-braces-alist . ((arglist-cont-nonempty)
                                 (block-close . c-snug-do-while)
                                 (brace-entry-open)
                                 (brace-list-open)
                                 (substatement-open after)
                                 (defun-open after)
                                 (defun-close before after)
                                 (class-open after)
                                 (class-close before after)
                                 (inexpr-class-open after)
                                 (inexpr-class-close before)
                                 (namespace-open after)
                                 (inline-open after)
                                 (inline-close before after)
                                 (block-open after)
                                 (extern-lang-open after)
                                 (extern-lang-close after)
                                 (statement-case-open after)))
      (c-offsets-alist . (
                          ; documentation:
                          ; https://www.gnu.org/software/emacs/manual/html_node/ccmode/c_002doffsets_002dalist.html
                          ; symbols:
                          ; https://www.gnu.org/software/emacs/manual/html_node/ccmode/Syntactic-Symbols.html
                          ;
                          ; arg indent helper funcs: c-lineup-*
                          ; arglist = indent to matching (|here, stuff
                          ; argcont = indent to (stuff, |here
                          ; casecaded calls = ->lol\n->stuff
                          ; absolute offset: [0]
                          (access-label          . -)   ; public: or private:
                          (arglist-intro         . +)   ; first arg in newline
                          (arglist-cont          . 0)   ; wrapped function args: func(\nthisone
                                                        ; wrapped function args after func(arg,\nthisone:
                          (arglist-cont-nonempty . (max c-lineup-arglist
                                                        c-lineup-argcont
                                                        c-lineup-string-cont
                                                        c-lineup-cascaded-calls))
                          (arglist-close         . 0)   ; intentation of ) which closes tabbed args
                          (block-open            . 0)   ; { to open a block
                          (block-close           . 0)   ; } after a block
                          (brace-list-intro      . +)   ; first element in {\nthisone
                                                        ;; this will be improved through emacs commit aa1a4cceca2d93d83c721ce83950230739073727
                          (brace-list-entry      . 0)   ; other elements in {\nelem\nthisone
                          (case-label            . 0)   ; case 1337:
                          (statement-case-open   . 0)   ; { after case 1337:
                          (statement-case-intro  . +)   ; code after case 1337:
                          (cpp-macro             . [0])   ; #define, etcetc
                          (defun-block-intro     . +)   ; beginning of keyword (...) { stuff  }
                          (inclass               . +)   ; members of struct or class
                          (inexpr-class          . 0)   ; class declaration within expression
                          (inexpr-statement      . 0)   ; statement block within expression
                          (inher-intro           . +)   ; beginning of inheritance def
                          (inher-cont            . c-lineup-multi-inher)   ; inheritance continuation
                          (inlambda              . 0)   ; function body of a lambda
                          (inline-open           . +)
                          (innamespace           . 0)   ; namespace lol {\nthisstatement
                          (knr-argdecl-intro     . 0)
                          (label                 . 0)   ; gotolabel:
                          (member-init-intro     . +)   ; member initializing for class lol : var(val)
                          (member-init-cont      . c-lineup-multi-inher)   ; further members
                          (statement             . 0)
                          (statement-block-intro . +)   ; line in if () {\nthisline
                                                        ; int a =\nthisone or return B{\nthisone
                                                        ; or B{asdf +\nthisone
                          (statement-cont        . (max c-lineup-assignments
                                                        c-lineup-cascaded-calls
                                                        c-lineup-string-cont))
                          (stream-op             . c-lineup-streamop)
                          (substatement          . +)
                          (substatement-label    . 0)
                          (substatement-open     . 0)
                          (template-args-cont    . (c++-template-args-cont
                                                    c-lineup-template-args +))
                          (topmost-intro         . 0)   ; indentation of file start
                          (topmost-intro-cont    . c-lineup-topmost-intro-cont)
                          (comment-intro         . c-lineup-comment)   ; start of comment
                          (c                     . c-lineup-C-comments)  ; multiline comment continuation. what a name.
                          ))

      ;; information about indent parsing on TAB
      ;; this is also triggered by C-c C-s
      (c-echo-syntactic-information-p . nil))
    "The SFT C++ programming style"
    )
  (c-add-style "sftstyle"     sft-c-style)

  ;; https://github.com/llvm-mirror/llvm/blob/master/utils/emacs/emacs.el
  (defun llvm-lineup-statement (langelem)
    (let ((in-assign (c-lineup-assignments langelem)))
      (if (not in-assign)
          '++
        (aset in-assign 0
              (+ (aref in-assign 0)
                 (* 2 c-basic-offset)))
        in-assign)))

  ;; ciip style based on llvm
  ;; https://gitlab.lrz.de/IP/elsa/-/blob/master/.clang-format
  (c-add-style "ciipstyle"
               '("gnu"
                 (fill-column . 100)
                 (c++-indent-level . 4)
                 (c-basic-offset . 4)
                 (indent-tabs-mode . nil)
                 (c-offsets-alist . ((arglist-intro . +)
                                     (arglist-close         . 0)
                                     (innamespace . +)  ;; ohh whyyyy, pls change to 0
                                     (member-init-intro . ++)
                                     (inlambda . 0)   ; lambda function body
                                     (statement-cont . llvm-lineup-statement)))))

  ;; google c++ style
  ;; https://github.com/google/styleguide/blob/gh-pages/google-c-style.el
  ;; installed as melpa package
  ;(eval-when-compile (require 'google-c-style))
  (require 'google-c-style)
  (c-add-style "Google" google-c-style t)
  )
;;; end coding style definitions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj/mode-hooks ()
  ;; main coding configuration function
  (defun jj/coding-hook ()
    (auto-revert-mode t)
    (idle-highlight-mode t)  ;; idle-highlight word under cursor
    (font-lock-add-keywords nil '(("\\<\\(TODO\\|todo\\|ASDF\\|asdf\\|TMP\\|FIXME\\|fixme\\)" 1 font-lock-warning-face t)))
    (jj/lsp-enable)
    )

  (defun jj/lsp-enable ()
    ;; for c++, use ccls-lsp
    (setq lsp-mode t
          lsp-prefer-flymake nil
          lsp-enable-indentation nil       ; don't ask the language server for indentations
          lsp-enable-imenu nil
          lsp-enable-xref t
          lsp-headerline-breadcrumb-enable-diagnostics nil
          company-lsp-async t
          company-lsp-cache-candidates 'auto
          company-minimum-prefix-length 1  ;; lsp does the lookup :)
          company-idle-delay 0.0) ;; default is 0.2

    (jj/codenav-keybinds)
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; special language-specific hooks
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; example .dir-locals.el file for linux repos:
  ;; (
  ;;  (nil . ((indent-tabs-mode . t)
  ;;          (tab-width . 8)))
  ;;  (c-mode . ((c-file-style . "linux-kernel")))
  ;;  (c++-mode . ((c-file-style . "linux-kernel")))
  ;;  )

  ;; c-like-language setup
  (defun jj/cstyle-hook ()
    ;; create codestyle
    (jj/create-codestyles)

    ;; magic region formatting
    (with-library
     clang-format
     (global-set-key (kbd "C-M-<tab>") 'clang-format-region))

    ;; default to sft style
    (c-set-style "sftstyle")

    (setq tab-width 4
          indent-tabs-mode t)

    (c-toggle-auto-newline nil) ; no automatic
    (c-toggle-auto-state nil)   ; newlines

    ;; keybindings for clike languages
    (jj/cstyle-keybinds)

    ;; smart tabs: mix tabs and spaces the right way
    (smart-tabs-mode)
    (smart-tabs-advice c-indent-line c-basic-offset)
    (smart-tabs-advice c-indent-region c-basic-offset)
    )


  ;; hook for all c-like languages
  (defun jj/c-base-hook ()
    (setq flycheck-gcc-language-standard "c++20")

    ;; c-codingstyle
    (jj/cstyle-hook)
    )


  ;; c++ special stuff
  (defun jj/c++-coding-hook ()
    ;; comment parsing and word highlighting

    ;; placing regexes into `c-mode-common-hook' may work but their
    ;; evaluation order matters.
    (font-lock-add-keywords
      nil '(
            ;; missing C++ keywords
            ("\\<\\(static_assert\\|concept\\|requires\\|consteval\\|co_await\\|co_yield\\|co_return\\|export\\|import\\|module\\)\\>" . font-lock-keyword-face)

            ;; custom defined types
            ("\\<[A-Za-z_]+[A-Za-z_0-9]*_t\\>" . font-lock-type-face)
            ))
    )

  ;; py
  (defun jj/python-coding-hook ()
    (setq python-indent 4
          indent-tabs-mode nil
          tab-width 4
          whitespace-line-column 79)

    (setq flycheck-checker 'python-pylint
          flycheck-checker-error-threshold 300)

    ;; don't show anaconda mode error popup gaaarrhhgh
    (remove-hook 'anaconda-mode-response-read-fail-hook
                 'anaconda-mode-show-unreadable-response)

    ;; limit docstring line count
    (setq lsp-signature-doc-lines 1
          lsp-signature-render-documentation t)

    ;; smart tabs
    (smart-tabs-mode)
    (smart-tabs-advice py-indent-line py-indent-offset)
    (smart-tabs-advice py-newline-and-indent py-indent-offset)
    (smart-tabs-advice py-indent-region py-indent-offset))

  ;; elisp
  (defun jj/lisp-coding-hook ()
    (jj/codenav-keybinds)
    (setq indent-tabs-mode nil)
    (setq tab-width 8)
    (prettify-symbols-mode))

  ;; javascript / ecmascript
  (defun jj/javascript-coding-hook ()
    (setq js-indent-level 2)
    (setq tab-width 2)
    (setq indent-tabs-mode nil))

  ;; TeX
  (defun jj/latex-coding-hook ()

    (message "custom latex config loading...")

    ;; set latex indent offset so it doesn't fuck up
    ;; (i.e. use values != n*tab-width)
    (setq TeX-engine 'default    ;; or xetex
          tab-width 4
          fill-column 76
          LaTeX-indent-level 4
          LaTeX-item-indent 0
          indent-tabs-mode nil
          TeX-parse-self t  ;; enable parse on load
          TeX-auto-save t   ;; enable parse on save
          TeX-PDF-mode t
          reftex-plug-into-AUCTeX t
          company-minimum-prefix-length 2) ;; so completes start with 2 chars already

    ;; don't highlight long lines
    ;; whitespace-highlight may not be initialized yet, thus handle both cases..
    (let ((no-lines-tail (lambda ()
                           (setq whitespace-style (delete 'lines-tail whitespace-style)))))
      (if (boundp 'whitespace-style)
        (funcall no-lines-tail)
        (progn
          (add-hook 'global-whitespace-mode-hook no-lines-tail))))

    (setq-default TeX-master nil) ; query for master file
    (visual-line-mode t)
    (LaTeX-math-mode t)
    (turn-on-reftex)

    ;; don't do non-company completions
    ;; otherwise a funny new buffer appears with "useful" completions
    (remove-hook 'completion-at-point-functions 'TeX--completion-at-point t)

    ;; minted codehighlighting needs shell execution for pygments
    (add-to-list 'TeX-command-list
                 '("LaTeX-shellescape" "%`%l -shell-escape %(mode) %(extraopts) %' %t" TeX-run-TeX nil
                   (latex-mode doctex-mode) :help "Run LaTeX -shell-escape") t)
    (add-to-list 'TeX-command-list
                 '("XeLaTeX" "%`xelatex %(mode) %(extraopts) %' %t" TeX-run-TeX nil
                   (latex-mode doctex-mode) :help "Run XeLaTeX") t)
    (add-to-list 'TeX-command-list
                 '("XeLaTeXMk" "latexmk -xelatex %(-PDF)%S%(mode) %(file-line-error) %(extraopts) %t" TeX-run-latexmk nil
                   (plain-tex-mode latex-mode doctex-mode) :help "Run LaTeXMk with XeLaTeX") t))

  ;; BibTeX
  (defun jj/bibtex-coding-hook ()
    (setq tab-width 2
          indent-tabs-mode nil
          bibtex-comma-after-last-field t
          bibtex-align-at-equal-sign t))


  ;; html
  (defun jj/html-coding-hook ()
    (setq sgml-basic-offset 4)
    (setq indent-tabs-mode t))

  ;; haskell
  (defun jj/haskell-coding-hook ()
    ;; haskell interpreter: C-c C-z or C-c C-l
    ;;(haskell-indentation-mode)
    (setq indent-tabs-mode nil))

  ;; vhdl
  (defun jj/vhdl-coding-hook ()
    (setq indent-tabs-mode nil)
    (smart-tabs-mode)
    (smart-tabs-advice vhdl-indent-line vhdl-basic-offset)
    (setq vhdl-indent-tabs-mode t))

  ;; org-mode
  (defun jj/org-mode-hook ()
    (setq org-log-done nil
          indent-tabs-mode nil))

  ;; markdown-mode
  (defun jj/markdown-mode-hook ()
    (setq indent-tabs-mode nil
          whitespace-line-column 400))

  (defun jj/cmake-mode-hook ()
    (setq indent-tabs-mode t)
    (setq cmake-tab-width 4))

  (defun jj/compilation-mode-hook ()
    ;; colorized compilation
    (require 'ansi-color)
    (defun colorize-compilation-buffer ()
      (when (eq major-mode 'compilation-mode)
        (ansi-color-apply-on-region compilation-filter-start (point-max))))
    (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

  (defun jj/sql-mode-hook ()
    (setq indent-tabs-mode t
          tab-width 4
          sqlind-basic-offset 4)

    (smart-tabs-mode)
    (smart-tabs-advice sqlind-indent-line sqlind-basic-offset))

  (defun jj/sql-interactive-mode-hook ()
    (let ((lval 'sql-input-ring-file-name)
          (rval 'sql-product))
      (if (symbol-value rval)
        (let ((filename
                (concat "~/.emacs.d/sql/"
                        (symbol-name (symbol-value rval))
                        "-history.sql")))
          (set (make-local-variable lval) filename))
        (error
          (format "SQL history will not be saved because %s is nil"
                  (symbol-name rval))))))

  ;; hooks to be inherited:
  ;;(add-hook 'text-mode-hook       'something)
  (add-hook 'prog-mode-hook       'jj/coding-hook)


  ;; language-specific hooks:
  (add-hook 'python-mode-hook            'jj/python-coding-hook)
  (add-hook 'lisp-mode-hook              'jj/lisp-coding-hook)
  (add-hook 'emacs-lisp-mode-hook        'jj/lisp-coding-hook)
  (add-hook 'js-mode-hook                'jj/javascript-coding-hook)
  (add-hook 'html-mode-hook              'jj/html-coding-hook)
  (add-hook 'haskell-mode-hook           'jj/haskell-coding-hook)
  (add-hook 'c-mode-common-hook          'jj/c-base-hook)
  (add-hook 'c++-mode-hook               'jj/c++-coding-hook)
  (add-hook 'LaTeX-mode-hook             'jj/latex-coding-hook)
  (add-hook 'bibtex-mode-hook            'jj/bibtex-coding-hook)
  (add-hook 'vhdl-mode-hook              'jj/vhdl-coding-hook)
  (add-hook 'org-mode-hook               'jj/org-mode-hook)
  (add-hook 'markdown-mode-hook          'jj/markdown-mode-hook)
  (add-hook 'cmake-mode-hook             'jj/cmake-mode-hook)
  (add-hook 'compilation-mode-hook       'jj/compilation-mode-hook)
  (add-hook 'sql-mode-hook               'jj/sql-mode-hook)

  (add-hook 'doc-view-mode-hook          'auto-revert-mode)
  (add-hook 'server-visit-hook (lambda ()
                                 (prefer-coding-system 'utf-8)
                                 (setq locale-coding-system 'utf-8)
                                 (set-terminal-coding-system 'utf-8)
                                 (set-keyboard-coding-system 'utf-8)
                                 (set-selection-coding-system 'utf-8)))

  ;; correct zsh coloring in shell:
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

  ;; add a function to multiple hooks
  (defun multi-hook-add (function hooks)
    (mapc (lambda (hook)
            (add-hook hook function))
          hooks))

  ;; some modes don't inherit from prog-mode...
  (multi-hook-add
   (lambda ()
     ;; TODO: (put 'something-mode 'derived-mode-parent 'prog-mode)
     (run-hooks 'prog-mode-hook))
   '(python-mode-hook
     haskell-mode-hook))

  ;(with-eval-after-load 'company
  ;  (company-posframe-mode))

  ;; file suffix assignments to automatic mode loading
  ;(add-to-list 'auto-mode-alist '("\\.nyan\\'" . nyan-mode))
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
  )

;; we have a graphical window
(defun jj/window-setup ()
  (message "spawned new frame in windowed mode")
  (setq confirm-kill-emacs 'y-or-n-p))

;; we're running on tty
(defun jj/terminal-setup ()
  (message "spawned new frame in terminal mode")
  (setq confirm-kill-emacs nil)
  ;; TODO: disable hl-line-mode
  )

(defun jj/new-frame-setup (frame)
  (select-frame frame)
  (if (window-system frame)
    (jj/window-setup)
    (jj/terminal-setup)))

;; change colors according to display
(defun jj/display-setup ()

  ;; called after a new frame was created
  ;;(add-hook 'after-make-frame-functions 'jj/new-frame-setup)

  (custom-set-faces
    ;; in a terminal, set the background to black!
   '(default (
              (((type tty) (min-colors 256))
               (:background "black"))
              (t
               (:background "#181a26")))
      )
   ;; brighter tab circle
   '(whitespace-tab ((t (:foreground "#206090"))))
   ;; for alignment, space-after-tab is not evil
   '(whitespace-space-after-tab ((t (:foreground "#103050"))))
   ;; set the idle-highlight face to only underline
   '(idle-highlight ((t (:underline t))))
   )

  (jj/new-frame-setup (selected-frame)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spacemacs init hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotspacemacs/user-init ()
  "Initialization for user code:
This function is called immediately after `dotspacemacs/init', before layer
configuration.
It is mostly for variables that should be set before packages are loaded.
If you are unsure, try setting them in `dotspacemacs/user-config' first."

  (message "initializing user code...")
  ;; for debugging this file and emacs itself
  (setq debug-on-error t)
  ;; to watch variable writes, there's M-x debug-watch
  ;(debug-watch 'indent-line-function)

  ;; store customizations in extra file
  (setq custom-file "~/.spacemacs.d/custom.el")

  ;; mode hooks need to be here since user-config is executed
  ;; after command-line-provided files' modes are initialized.
  (jj/mode-hooks)
  )

(defun dotspacemacs/user-load ()
  "Library to load while dumping.
This function is called only while dumping Spacemacs configuration. You can
`require' or `load' the libraries of your choice that will be included in the
dump."
  )

(defun dotspacemacs/user-config ()
  "Configuration for user code:
This function is called at the very end of Spacemacs startup, after layer
configuration.
Put your configuration code here, except for variables that should be set
before packages are loaded."

  (message "loading user config...")
  (jj/modes)
  (jj/defaults)
  (jj/display-setup)
  (jj/mousescroll)
  (jj/keybindings)

  ;; load customization file if it exists.
  (when (file-exists-p custom-file)
    (load-file custom-file))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
