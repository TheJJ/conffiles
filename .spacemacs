;; -*- mode: emacs-lisp; lexical-binding: t -*-
;; vim: ft=lisp

;; jj's spacemacs configuration
;; Copyright (c) 2011-2022 Jonas Jelten <jj@sft.lol>
;; Licensed GPLv3 or later
;;
;; ====
;; dir-locals: for project-specific configs, create a .dir-locals.el file:
;; example .dir-locals.el file for a linux repo:
;; (
;;  (nil . ((indent-tabs-mode . t)
;;          (tab-width . 8)))
;;  (c-mode . ((c-file-style . "linux-kernel")))
;;  (c++-mode . ((c-file-style . "linux-kernel")))
;;  )
;;
;;
;; ====
;; optional dependencies to make things better:
;;
;; * C++: clangd
;;   for semantic indexing etc.
;;
;; ====
;; useful customizations, stored in ~/.emacs.d/custom.el
;;
;; * org:
;;   - org-agenda-files: directories and files to auto-load.
;;                       can be added with `org-agenda-file-to-front` C-c [
;;                       and removed with `org-remove-file` C-c ]
;; * org-roam:
;;   - org-roam-directory: path where the org-roam notes are
;;
;; * bibtex paths
;;   - bibtex-completion-bibliography  (where are bib files)
;;   - bibtex-completion-library-path  (where are pdfs)
;;   => to edit notes of bibtex entries, use M-x helm-bibtex <search> C-z F8
;;
;; ====
;; configuration todos:
;; * helm-M-x should display documentation of functions/variables, ivy does that


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

     ansible
     asm
     (auto-completion :variables
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-help-tooltip nil
                      ;; tui/gui switch + color fix needed:
                      auto-completion-use-company-box nil)
     better-defaults
     (bibtex :variables
             bibtex-enable-ebib-support t)
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-backend 'lsp-clangd)
     (cmake :variables
            cmake-enable-cmake-ide-support nil)
     csv
     dap
     debug
     emacs-lisp
     emoji
     major-modes  ;; qml-mode, openscad
     meson
     git
     gpu
     (go :variables
         go-tab-width 4)
     (gtags :variables
            gtags-enable-by-default nil)
     (haskell :variables
              haskell-completion-backend 'intero)
     (helm :variables
           helm-enable-auto-resize nil)
     helpful
     html
     java
     javascript
     (latex :variables
            latex-view-with-pdf-tools nil
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
     (multiple-cursors :variables
                       multiple-cursors-backend 'evil-mc)
     nginx
     (org :variables
          org-enable-appear-support t
          org-enable-roam-support t
          org-enable-github-support t
          ;; org-roam-directory is customized!
          org-roam-v2-ack t)
     pdf
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
     (spacemacs-editing :variables
                        vim-style-enable-undo-region t)
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     spell-checking-enable-auto-dictionary t
                     enable-flyspell-auto-completion nil)
     sql
     systemd
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips t)
     theming
     (treemacs :variables
               treemacs-use-follow-mode nil
               treemacs-use-filewatch-mode t
               treemacs-collapse-dirs 3)
     typescript
     version-control
     xclipboard
     yaml
     )

   ;; List of additional packages that will be installed without being wrapped
   ;; in a layer (generally the packages are installed only and should still be
   ;; loaded using load/require/use-package in the user-config section below in
   ;; this file). If you need some configuration for these packages, then
   ;; consider creating a layer. You can also put the configuration in
   ;; `dotspacemacs/user-config'. To use a local version of a package, use the
   ;; `:location' property: '(your-package :location "~/path/to/your-package/")
   ;; Also include the dependencies as they will not be resolved automatically.
   dotspacemacs-additional-packages
   '(
     ag
     bison-mode
     citar
     crdt
     deadgrep
     google-c-style
     idle-highlight-mode
     org-roam-bibtex
     org-super-agenda
     pdf-tools
     rainbow-mode
     ripgrep
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()

   ;; A list of packages that will not be installed and loaded.
   dotspacemacs-excluded-packages
   '(
     auto-complete
     auto-highlight-symbol
     helm-company  ;; so C-/ is not mapped to it when completing...
     importmagic
     ox-pandoc
     sqlup-mode
     wolfram-mode
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
   ;; If non-nil then enable support for the portable dumper. You'll need to
   ;; compile Emacs 27 from source following the instructions in file
   ;; EXPERIMENTAL.org at to root of the git repository.
   ;;
   ;; WARNING: pdumper does not work with Native Compilation, so it's disabled
   ;; regardless of the following setting when native compilation is in effect.
   ;;
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

   ;; Show numbers before the startup list lines. (default t)
   dotspacemacs-show-startup-list-numbers t

   ;; The minimum delay in seconds between number key presses. (default 0.4)
   dotspacemacs-startup-buffer-multi-digit-delay 0.4

   ;; If non-nil, show file icons for entries and headings on Spacemacs home buffer.
   ;; This has no effect in terminal or if "all-the-icons" package or the font
   ;; is not installed. (default nil)
   dotspacemacs-startup-buffer-show-icons nil

   ;; Default major mode for a new empty buffer. Possible values are mode
   ;; names such as `text-mode'; and `nil' to use Fundamental mode.
   ;; (default `text-mode')
   dotspacemacs-new-empty-buffer-major-mode 'text-mode

   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'lisp-interaction-mode

   ;; If non-nil, *scratch* buffer will be persistent. Things you write down in
   ;; *scratch* buffer will be saved and restored automatically.
   dotspacemacs-scratch-buffer-persistent t

   ;; If non-nil, `kill-buffer' on *scratch* buffer
   ;; will bury it instead of killing.
   dotspacemacs-scratch-buffer-unkillable nil

   ;; Initial message in the scratch buffer, such as "Welcome to Spacemacs!"
   ;; (default nil)
   dotspacemacs-initial-scratch-message nil

   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press `SPC T n' to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(starlit
                         spacemacs-dark
                         monokai)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `doom', `vim-powerline' and `vanilla'. The
   ;; first three are spaceline themes. `doom' is the doom-emacs mode-line.
   ;; `vanilla' is default Emacs mode-line. `custom' is a user defined themes,
   ;; refer to the DOCUMENTATION.org for more info on how to create your own
   ;; spaceline theme. Value can be a symbol or list with additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs
                                  :separator nil
                                  :separator-scale 1.3)

   ;; If non-nil the cursor color matches the state color in GUI Emacs.
   ;; (default t)
   dotspacemacs-colorize-cursor-according-to-state t

   ;; Default font or prioritized list of fonts. The `:size' can be specified as
   ;; a non-negative integer (pixel size), or a floating-point (point size).
   ;; Point size is recommended, because it's device independent. (default 10.0)
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 10.0
                               :weight normal
                               :width normal)

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

   ;; Show the scroll bar while scrolling. The auto hide time can be configured
   ;; by setting this variable to a number. (default t)
   dotspacemacs-scroll-bar-while-scrolling t

   ;; Control line numbers activation.
   ;; If set to `t', `relative' or `visual' then line numbers are enabled in all
   ;; `prog-mode' and `text-mode' derivatives. If set to `relative', line
   ;; numbers are relative. If set to `visual', line numbers are also relative,
   ;; but only visual lines are counted. For example, folded lines will not be
   ;; counted and wrapped lines are counted as multiple lines.
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

   ;; If non-nil and `dotspacemacs-activate-smartparens-mode' is also non-nil,
   ;; `smartparens-strict-mode' will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil

   ;; If non-nil smartparens-mode will be enabled in programming modes.
   ;; (default t)
   dotspacemacs-activate-smartparens-mode nil

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
   dotspacemacs-enable-server t

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
   ;; If nil then Spacemacs uses default `frame-title-format' to avoid
   ;; performance issues, instead of calculating the frame title by
   ;; `spacemacs/title-prepare' all the time.
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

   ;; If non-nil activate `clean-aindent-mode' which tries to correct
   ;; virtual indentation of simple modes. This can interfere with mode specific
   ;; indent handling like has been reported for `go-mode'.
   ;; If it does deactivate it here.
   ;; (default t)
   dotspacemacs-use-clean-aindent-mode t

   ;; Accept SPC as y for prompts if non-nil. (default nil)
   dotspacemacs-use-SPC-as-y nil

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
   ;; and todos. If non-nil only the file name is shown.
   dotspacemacs-home-shorten-agenda-source nil

   ;; If non-nil then byte-compile some of Spacemacs files.
   dotspacemacs-byte-compile t))

(defun dotspacemacs/user-env ()
  "Environment variables setup.
This function defines the environment variables for your Emacs session. By
default it calls `spacemacs/load-spacemacs-env' which loads the environment
variables declared in `~/.spacemacs.env' or `~/.spacemacs.d/.spacemacs.env'.
See the header of this file for more information."
  (setq spacemacs-env-vars-file (locate-user-emacs-file ".cache/spacemacs.env"))
  (setq spacemacs-ignored-environment-variables nil)
  ;;(spacemacs/load-spacemacs-env t)

  ;; execute the default shell twice and
  ;; fetch the environment variables with `env'.
  ;; this is a customized version of the spacemacs default
  ;; which caches the environment vars in a file, and filters out
  ;; things like ssh_auth_sock.
  ;; in order to get them on each emacs launch, this variant exists.
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


;;####################################################
;; jj's addons to spacemacs start here
;;####################################################


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; load non-spacemacsed packages
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj/load-packages ()
  "load and setup packages that are not yet nicely integrated in spacemacs"

  (use-package org-roam-bibtex
    :after org-roam
    :config
    (require 'org-ref)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; helper functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-library (symbol &rest body)
  "when the library is available, do things with it."
  `(condition-case nil
       (progn (require ',symbol) ,@body)
     (error (message (format "package unavailable: %s" ',symbol))
            nil)))

(defun sync-variable (destinationvar sourcevar)
  "synchronize the state of a given variable to another one.
the value is copied when setting up the sync."

  (let ((updatefunc (lambda (symbol newval operation where)
                      (set destinationvar newval))))
    ;; install update-hook
    (add-variable-watcher sourcevar updatefunc)
    ;; call for initial update
    (funcall updatefunc nil (symbol-value sourcevar) nil nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; advices
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; search-replace the whole buffer if there's no active region
(defun jj/goto-top-if-no-region (orig-func &rest args)
  "if region is not active, go to the buffer start"
  (save-excursion
    (when (not (use-region-p))
      (goto-char (point-min)))
    (apply orig-func args)))
(advice-add 'query-replace :around #'jj/goto-top-if-no-region)
(advice-add 'query-replace-regexp :around #'jj/goto-top-if-no-region)

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
                         ;; set the offset-variable to the same high thing
                         (,offset fill-column)
                         ;;(wstart (window-start))
                         )
                      (unwind-protect
                        (progn ad-do-it)
                        ;;(set-window-start (selected-window) wstart)
                        )))
                  (t ad-do-it)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode setup and default config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jj/defaults ()
  "mode and default configuration"

  (delete-selection-mode t)  ;; replace selection with typed text

  ;; push the mouse out of the way when the cursor approaches.
  (mouse-avoidance-mode 'cat-and-mouse)

  (setq-default visible-bell nil            ; disable window flashing
                ring-bell-function 'ignore) ; and also disable the sound

  (setq indicate-empty-lines t
        transient-mark-mode t
        package-native-compile nil       ; don't compile on install, instead on demand
        native-comp-async-report-warnings-errors t
        warning-minimum-level :error
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
        tab-always-indent t
        python-fill-docstring-style 'symmetric
        backward-delete-char-untabify-method nil
        cua-auto-tabify-rectangles nil
        cua-enable-cua-keys nil
        ranger-show-literal t            ; colored ranger previews
        python-shell-prompt-detect-failure-warning nil
        python-shell-interpreter "ipython3" ; tramp on remote-hosts needs ipython3 and python3-setuptools
        ;python-shell-interpreter-interactive-arg ""
        compilation-environment (quote ("TERM=xterm-256color"))
        recentf-max-saved-items 1000
        idle-highlight-idle-time 0.2
        confirm-kill-emacs 'y-or-n-p     ; always ask when exiting
        password-cache-expiry nil        ; tramp password cache
        inhibit-compacting-font-caches t ; trade more memory with less lagging due to font compactions

        desktop-restore-eager 3          ; other buffers are restored lazily

        ido-use-virtual-buffers t        ; use recentf-buffers as virtually "open"
        ido-enable-flex-matching t
        ido-case-fold t
        ido-file-extensions-order '(".c" ".cpp" ".h" ".py" ".tex" ".bib" ".hs")

        history-delete-duplicates t      ; helm history duplicate removal
        helm-adaptive-history-file (locate-user-emacs-file ".cache/helm-adaptive-history")
        helm-adaptive-history-length 200
        helm-ff-file-name-history-use-recentf nil  ; don't use recentf for helm find files

        custom-unlispify-tag-names nil   ; view variable names in custom-mode

        mime-edit-split-message nil      ; don't split large messages

        ;; bibtex settings
        bibtex-completion-pdf-open-function 'helm-open-file-with-default-tool  ; open with system viewer
        bibtex-completion-pdf-field "file" ; field in bibtex file for pdf name
        ebib-bibtex-dialect 'biblatex
        ebib-index-default-sort '("Year" . descend)
        ebib-file-associations '()         ; so find-file handles the opening

        ;; org settings
        org-hide-emphasis-markers t  ; hide syntax elements
        org-startup-with-inline-images t
        org-startup-with-latex-preview t
        org-appear-autolinks nil      ; let links be invisible because they expand to long lines
        org-appear-autoentities t
        org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil
        org-reverse-note-order t         ; new items at top
        org-refile-targets '((org-agenda-files :maxlevel . 2))
        org-enforce-todo-dependencies t
        org-src-window-setup 'current-window ; edit in current window
        org-src-preserve-indentation t
        org-edit-src-content-indentation 0
        org-latex-pdf-process '("xelatex -interaction nonstopmode %f" "xelatex -interaction nonstopmode %f")
        org-confirm-babel-evaluate nil   ; sure, just execute org code snippets, what can go wrong
        org-babel-default-header-args:cpp '((:flags . "-std=c++20 -Wall -Wextra"))
        org-log-done nil

        ;; LaTeX settings
        TeX-engine 'default  ;; or xetex, but conflicts with inputenc package
        TeX-PDF-mode t
        TeX-save-query nil
        TeX-parse-self t  ;; enable parse on load
        TeX-auto-save t   ;; enable parse on save
        reftex-plug-into-AUCTeX t

        ;; lsp settings
        lsp-enable-indentation nil       ; don't ask the language server for indentations
        lsp-enable-imenu nil
        lsp-enable-xref t
        lsp-headerline-breadcrumb-enable-diagnostics nil
        lsp-diagnostic-package :none     ; disable lsp diagnostics for performance reasons (flycheck/flymake)
        lsp-enable-on-type-formatting nil  ; using t funnily changes screen content whenever lsp thinks it can do "formatting"
        lsp-enable-file-watchers nil       ; lsp server can do inotify itself, but that may slow emacs down (https://github.com/MaskRay/ccls/issues/354)
        lsp-eldoc-enable-hover t           ; display info about thing at cursor in minibuffer
        lsp-eldoc-render-all nil
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-signature-doc-lines 1
        lsp-enable-snippet t

        company-minimum-prefix-length 1  ;; lsp does the lookup :)
        company-idle-delay 0.1)

  (when (<= emacs-major-version 28)
    ;; on emacs 29, the scaling seems correct :)
    (setq org-format-latex-options (plist-put org-format-latex-options :scale 2.0)))

  ;; don't persist clipboard accross sessions
  (delete 'kill-ring savehist-additional-variables)

  ;; adjust lsp-mode internals
  (with-eval-after-load 'lsp-mode
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "clangd")
                      :major-modes '(c++-mode)
                      :remote? t
                      :server-id 'clangd-remote))
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "pylsp")
                      :major-modes '(python-mode)
                      :remote? t
                      :server-id 'pylsp-remote)))

  ;; undo-tree with region-specific undos
  (with-eval-after-load 'undo-tree
    (setq undo-tree-auto-save-history nil
          ;; very buggy, hangs up emacs often with many -1 items in the buffer-undo-tree
          ;; but spacemacs seems to mitigate it in spacemacs-editing layer
          undo-tree-enable-undo-in-region t))

  (with-eval-after-load 'mmm-mode
    ;; automatic sub-mode parsing
    (setq mmm-parse-when-idle t))

  ;; tame org-open-file, which uses org-file-apps, and finally mailcap.el
  ;; to determine how to open pdf files
  ;; if we do not set this in mailcap-user-mime-data, it returns pdf-view-mode
  ;; test with:
  ;; (mailcap-mime-info (mailcap-extension-to-mime ".pdf"))
  (with-eval-after-load 'org
    (setcdr (assoc "\\.pdf\\'" org-file-apps) 'default))

  (with-eval-after-load 'mailcap
    (add-to-list 'mailcap-user-mime-data
                 '("pdf"
                   (viewer . "xdg-open %s")
                   (type . "application/pdf")
                   (test . window-system)))
    (add-to-list 'mailcap-user-mime-data
                 '("html"
                   (viewer . "xdg-open %s")
                   (type . "text/html")
                   (test . window-system)))
    )

  ;; create hooks to redirect bibtex notes handling into org-roam
  (with-eval-after-load 'org-roam
    (org-roam-bibtex-mode))

  ;; synchronize bibliography customization settings to other packages
  (with-eval-after-load 'ebib
    (sync-variable 'ebib-file-search-dirs 'bibtex-completion-library-path)
    (sync-variable 'ebib-preload-bib-files 'bibtex-completion-bibliography))
  (with-eval-after-load 'citar
    (sync-variable 'citar-bibliography 'bibtex-completion-bibliography))

  (with-eval-after-load 'ispell
    (setq ispell-program-name "hunspell"))

  (use-package org-super-agenda
    :after org-agenda
    :init
    (setq org-super-agenda-groups
          '((:name "Due today"
             :deadline today)
            (:name "Overdue"
             :deadline past)
            (:name "Upcoming Deadline"
             :deadline future)
            (:name "Scheduled Today"
             :scheduled today)
            (:name "Scheduled Past"
             :scheduled past)
            (:name "Future Tasks"
             :scheduled future)
            (:discard (:anything t))))
    :config
    (org-super-agenda-mode))

  ;; wanderlust email \o/
  ;; per-device config is in ~/.wl/config and folders
  (use-package wl
    :defer t
    :init
    (progn
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
            wl-address-file "~/.wl/addresses")))


  ;; default mode for new buffers
  (setq-default major-mode 'text-mode)
  (setq initial-major-mode 'text-mode)

  ;; don't enable auto-newline mode (c-toggle-auto-newline)
  (remove-hook 'c-mode-common-hook 'spacemacs//c-toggle-auto-newline)

  ;; indentation defaults
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
  (spacemacs|diminish org-roam-bibtex-mode)

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

  ;; file suffix assignments to automatic mode loading
  ;(add-to-list 'auto-mode-alist '("\\.nyan\\'" . nyan-mode))
  (add-to-list 'auto-mode-alist '("\\.tex\\'" . LaTeX-mode))
  (add-to-list 'auto-mode-alist '("\\.tikz\\'" . LaTeX-mode))
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

(defun jj/scrolling ()
  ;;mouse-wheel scrolling
  (setq mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                      ((control)))            ; one line at a time
        mouse-wheel-progressive-speed t                       ; accelerate scrolling
        mouse-wheel-follow-mouse t)                           ; scroll- window under mouse

  (setq scroll-preserve-screen-position t                     ; keep relative column position when scrolling
        scroll-margin 4                                       ; start scrolling n lines before window borders
        scroll-conservatively 25                              ; scroll up to n lines to bring pointer back on screen
        scroll-step 0                                         ; try scrolling n lines when pointer moves out
        scrollbar-mode 'right
        auto-window-vscroll nil)

  (when (>= emacs-major-version 29)
    (pixel-scroll-precision-mode t)
    ))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; code symbol navigation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; xref--marker-ring rotation
;; only needed for emacs <= 28
;; so you can navigate back and forward over xref findings.
;; use-case: follow multiple definitions, go back to the beginning,
;;           and then easily go forward to each definition.

;; in emacs 29 we directly have `xref-go-back` and `xref-go-forward` :)

(defvar jj/xref-marker-slide 0
  "seek position tracking in the xref marker ring.
 a new xref finding resets the slide.
 slide: 0 = current position
        1 = previous position, i.e. last jump to xref mark
        2 = xref mark position before that")

(defvar jj/xref-marker-ring-has-backjump nil
  "t if the last point position was pushed to xref marker ring.
 this is done when moving backward the first time after doing
 xref jumps.")

(defun jj/xref-push-marker-stack-slide-reset (orig &rest args)
  "forget the future xref jumps before storing the new xref jump position."
  (dotimes (i (min jj/xref-marker-slide
                   (ring-length xref--marker-ring)))
    ;; remove all walked-backward marker ring entries
    (ring-remove xref--marker-ring 0))
  ;; reset the slides
  (setq jj/xref-marker-slide 0)
  (setq jj/xref-marker-ring-has-backjump nil)
  (apply orig args))

;; whenever we push, we drop the 'history'
(when (<= emacs-major-version 28)
  (advice-add 'xref-push-marker-stack :around 'jj/xref-push-marker-stack-slide-reset))


(defun jj/xref-goto-marker-slide (slide)
  (let* ((ring xref--marker-ring)
         (marker (ring-ref ring slide)))
    (switch-to-buffer (or (marker-buffer marker)
                          (user-error "The marked buffer has been deleted")))
    (goto-char (marker-position marker))
    ;(set-marker marker nil nil)
    (run-hooks 'xref-after-return-hook)))


(defun jj/xref-jump-marker-ring-forward ()
  (interactive)
  (when (= jj/xref-marker-slide 0)
    (user-error "Can't go xref-forward at front positon."))
  (let ((ring xref--marker-ring))
    (when (ring-empty-p ring)
      (user-error "Marker stack is empty"))

    (let ((newslide (- jj/xref-marker-slide 1)))
      (jj/xref-goto-marker-slide newslide)
      (setq jj/xref-marker-slide newslide))))


(defun jj/xref-jump-marker-ring-backward ()
  "Move one step backward in the xref marker ring, i.e. go to
 where \\[xref-find-definitions] was last invoked."
  (interactive)
  (let ((ring xref--marker-ring))
    (when (ring-empty-p ring)
      (user-error "Marker stack is empty"))

    (when (>= (+ jj/xref-marker-slide
                 (if jj/xref-marker-ring-has-backjump 1 0))
              (ring-length ring))
      ;; when we want to slide back, but slidepos+1 > ring-length
      (user-error "Can't go xref-backward beyond marker stack."))

    (when (not jj/xref-marker-ring-has-backjump)
      ;; when we're at the latest jump point (which resets has-backjump to nil)
      ;; push the current point marker to the ring so we can go back to it
      (ring-insert xref--marker-ring (point-marker))
      (setq jj/xref-marker-ring-has-backjump t))

    (let ((newslide (+ jj/xref-marker-slide 1)))
      (jj/xref-goto-marker-slide newslide)
      (setq jj/xref-marker-slide newslide))))


(defun jj/codenav-keybinds ()
  (interactive)
  (local-set-key [C-mouse-1] 'xref-find-definitions)
  (local-set-key (kbd "M-g d") 'xref-find-definitions)
  (local-set-key (kbd "M-g f") 'xref-find-references)
  (local-set-key (kbd "M-g G") 'xref-find-definitions-other-frame)

  (with-library
    lsp-mode
    (with-library
      treemacs
      (local-set-key (kbd "M-g e") 'lsp-treemacs-errors-list))

    ;; open inline popup with search results
    (local-set-key (kbd "M-g D") 'lsp-ui-peek-find-definitions)
    (local-set-key (kbd "M-g F") 'lsp-ui-peek-find-references))

  ;; defaults:
  ;; M-, pop-tagmark in slime-nav-mode-map ->xref-pop-marker-stack
  ;; M-. evil-repeat-pop-next in evil-normal-state-map
  ;; M-. evil-slime-nav-find-elisp-thing-at-point
  ;; M-. xref-find-definitions in global-map

  ;; navigation on marker ring to go back and forward by xrefs
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "M-.") nil)
    (define-key evil-normal-state-map (kbd "M-,") nil))

  (if (<= emacs-major-version 28)
      (progn
        (local-set-key (kbd "M-,") 'jj/xref-jump-marker-ring-backward)
        (local-set-key (kbd "M-.") 'jj/xref-jump-marker-ring-forward))
    ;; emacs 29 has these features built-in in xref \o/
    (progn
      (local-set-key (kbd "M-.") 'xref-go-forward)
      (local-set-key (kbd "M-,") 'xref-go-back))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; funny functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun batch-indent ()
  "Run `indent-region' on the files remaining on the command line.
Use this from the command line, with `-batch';
it won't work in an interactive Emacs.

Probably called with:
emacs -batch -l ~/.emacs.d/init.el --eval '(batch-indent)' file file file...'

Each of them will be indented as if it was opened in the editor."

  (when (not noninteractive)
    (error "`batch-indent` only works in -batch mode"))

  ;; need to init some parts of emacs
  (run-hooks 'emacs-startup-hook)

  ;; command-line-args-left is what is left of the command line, from startup.el
  (message "starting batch indentation...")
  (defvar command-line-args-left)
  (let ((error nil))
    (while command-line-args-left
      (let ((filename (car command-line-args-left)))
        (if (file-directory-p (expand-file-name filename))
            (message "ignoring directory %s" filename)
          (if (null (indent-file filename))
              (setq error t))))
      (setq command-line-args-left (cdr command-line-args-left)))
    (kill-emacs (if error 1 0))))


(defun indent-file (&optional fpath)
  "indent file stored at FPATH. default: use current buffer"
  (interactive)
  ;; TODO: inhibit most other modes, e.g. lsp-mode

  (let (openbuffer
        buffer-already-open)
    (if (not fpath)
        (setq buffer-already-open t)
      (setq openbuffer (get-file-buffer fpath))
      (if openbuffer
          (setq buffer-already-open t)
        (setq openbuffer (find-file-noselect fpath))
        (switch-to-buffer openbuffer)))

    (message "indenting %s..." (buffer-file-name))
    (indent-region (point-min) (point-max) nil)

    (when (not buffer-already-open)
      (save-buffer)
      (kill-buffer openbuffer))))


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

(defun reload-dir-locals-current-buffer ()
  "reload the .dir-locals.el for the current buffer"
  (interactive)
  (message (format "reloading dir-locals for %s (%s)..." (current-buffer) buffer-file-name))
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun reload-dir-locals-projectile ()
  "For every buffer in the current projectile project,
reload dir-locals"
  (interactive)
  (let ((pdir (projectile-project-root)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal (projectile-project-root) pdir)
          (reload-dir-locals-current-buffer))))))

(defun reload-dir-locals-below-default-dir ()
  "Reload dir-locals for all buffers that are potentially affected
from a change in by prefix-matching the current buffer's `default-directory`"
  (interactive)
  (let ((cdir default-directory))
    (message (format "reloading dir-locals below %s..." cdir))
    (dolist (buffer (buffer-list))
      (when (buffer-file-name buffer)
        (with-current-buffer buffer
          ;; todo: symlink resolving...
          (when (string-prefix-p cdir buffer-file-name)
            (reload-dir-locals-current-buffer)))))))


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
      (end-of-line)
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
    (delete-char 1))) ; the newline

(defun jj/shift-text (beg end shift-block-fun shift-line-fun)
  "shift text in region or line using evil like S-v with < and > do in Vim.
  It takes special care of preserving or even extending the region to the moved text lines."
  (if (use-region-p)
      (progn
        (let ((point-at-end (< (mark) (point))))

          ;; fix up current region end to grab the whole line
          (if point-at-end
              (end-of-line)
            (beginning-of-line))

          ;; then fix up the other region end
          (exchange-point-and-mark)
          (if point-at-end
              (beginning-of-line)
            (end-of-line))

          ;; restore mark-point order
          (exchange-point-and-mark)

          (let ((linebeg (if point-at-end (mark) (point)))
                (lineend (if point-at-end (point) (mark))))
            ;; shift the text
            (save-mark-and-excursion
              (funcall shift-block-fun linebeg lineend)
              ;; "In Transient Mark mode, every buffer-modifying primitive sets deactivate-mark"
              ;; but we wanna keep it active :)
              (setq deactivate-mark nil)))))

    (funcall shift-line-fun 1)))

(defun jj/shift-left (beg end)
  (interactive "r")
  (jj/shift-text beg end #'evil-shift-left #'evil-shift-left-line))

(defun jj/shift-right (beg end)
  (interactive "r")
  (jj/shift-text beg end #'evil-shift-right #'evil-shift-right-line))

(defun jj/keybindings ()
  (interactive)

  ;; arrow key stuff
  (global-set-key (kbd "M-<left>")  #'windmove-left)
  (global-set-key (kbd "M-<right>") #'windmove-right)
  (global-set-key (kbd "M-<up>")    #'windmove-up)
  (global-set-key (kbd "M-<down>")  #'windmove-down)

  (global-set-key (kbd "C-<left>")  #'backward-word)
  (global-set-key (kbd "C-<right>") #'forward-word)
  (global-set-key (kbd "C-<up>")    #'backward-paragraph)
  (global-set-key (kbd "C-<down>")  #'forward-paragraph)

  ;; line+region movement (like in org :)
  (global-set-key (kbd "M-S-<left>")  #'drag-stuff-left)
  (global-set-key (kbd "M-S-<right>") #'drag-stuff-right)
  (global-set-key (kbd "M-S-<up>")    #'drag-stuff-up)
  (global-set-key (kbd "M-S-<down>")  #'drag-stuff-down)

  ;; line nativation/deleteion
  (global-set-key (kbd "C-k") #'jj/delete-line)
  (global-set-key (kbd "C-S-k") #'jj/delete-line-backward)
  (global-set-key (kbd "C-l") #'recenter-top-bottom)
  (global-set-key (kbd "C-j") #'nowrap-newline-and-indent)
  (global-set-key (kbd "C-S-<backspace>") #'jj/delete-whole-line)

  ;; word deletion
  (global-set-key (kbd "C-<delete>")    #'jj/delete-word)
  (global-set-key (kbd "C-<backspace>") #'jj/delete-word-backward)

  ;; terminal fu
  (global-set-key (kbd "M-[ d") #'left-word)  ;backward-word
  (global-set-key (kbd "M-[ c") #'right-word) ;forward-word
  (global-set-key (kbd "M-[ a") #'backward-paragraph)
  (global-set-key (kbd "M-[ b") #'forward-paragraph)

  ;; newline magic
  (global-set-key (kbd "<C-return>") #'newline)
  (global-set-key (kbd "C-c C-a") #'mark-whole-buffer)

  ;; the best™ file chooser!!11
  (use-package helm-for-files
    :defer t
    :init (progn
            (global-set-key (kbd "C-x C-S-f") #'helm-for-files)
            (spacemacs||set-helm-key "fF" helm-for-files)))

  ;; open project/file/vc things
  (global-set-key (kbd "C-x b") #'helm-mini)
  (global-set-key (kbd "C-x p") #'helm-projectile)
  (global-set-key (kbd "C-x S-p") #'helm-projectile-switch-project)
  (global-set-key (kbd "C-x C-p") #'projectile-ag)

  (global-set-key (kbd "C-x C-b") #'bs-show) ; buffer selector
  (global-set-key (kbd "C-x M-b") #'speedbar)
  (global-set-key (kbd "C-c g") #'magit-status)
  (global-set-key (kbd "C-x g") #'magit-status)

  (global-set-key (kbd "C-x j b") #'helm-bibtex)
  (global-set-key (kbd "C-S-s") #'helm-occur)

  (global-set-key (kbd "C-x B") #'bury-buffer)
  (global-set-key (kbd "C-x E") #'apply-macro-to-region-lines)
  (global-set-key (kbd "C-x I") #'insert-buffer)
  (global-set-key (kbd "C-c w") #'delete-region) ; ala C-w and M-C-w
  (global-set-key (kbd "C-c c") #'comment-region)
  (global-set-key (kbd "C-c u") #'uncomment-region)
  (global-set-key (kbd "C-c n") #'next-error)
  (global-set-key (kbd "C-c p") #'previous-error)


  ;; align the current region to = or whatever
  (global-set-key (kbd "M-A") #'align-current)

  (global-set-key (kbd "M-SPC") #'just-one-space) ;fold space to 1

  (global-set-key (kbd "M-p") (lambda ()
                                (interactive)
                                (join-line -1)))

  ;; text shifting. evil-normal-state-map has these anyway.
  (define-key evil-hybrid-state-map (kbd "M-<") #'jj/shift-left)
  (define-key evil-hybrid-state-map (kbd "M->") #'jj/shift-right)

  ;; really insert a fucking tab
  (global-set-key (kbd "C-<tab>") 'insert-tab)

  ;; disable annoying character swapping
  (global-unset-key (kbd "C-t"))

  (use-package org
    :defer t
    :config
    (progn
      ;; org's default C-M-arrow bindings are kinda useless,
      ;; so we use them so M-arrow is free for window movements!
      (define-key org-mode-map (kbd "C-M-<right>") #'org-metaright)
      (define-key org-mode-map (kbd "C-M-<left>") #'org-metaleft)
      (define-key org-mode-map (kbd "C-M-<up>") #'org-metaup)
      (define-key org-mode-map (kbd "C-M-<down>") #'org-metadown)
      ;; unset those in orgmodemap so the global window mappings are used
      (define-key org-mode-map (kbd "M-<right>") nil)
      (define-key org-mode-map (kbd "M-<left>") nil)
      (define-key org-mode-map (kbd "M-<up>") nil)
      (define-key org-mode-map (kbd "M-<down>") nil)))

  (use-package markdown-mode
    :defer t
    :config
    (progn
      ;; markdown mode default C-M-arrow can also be overridden for window movements.
      (define-key markdown-mode-map (kbd "C-M-<right>") #'markdown-demote)
      (define-key markdown-mode-map (kbd "C-M-<left>") #'markdown-promote)
      (define-key markdown-mode-map (kbd "C-M-<up>") #'markdown-move-up)
      (define-key markdown-mode-map (kbd "C-M-<down>") #'markdown-move-down)
      (define-key markdown-mode-map (kbd "M-<right>") nil)
      (define-key markdown-mode-map (kbd "M-<left>") nil)
      (define-key markdown-mode-map (kbd "M-<up>") nil)
      (define-key markdown-mode-map (kbd "M-<down>") nil)))

  (use-package treemacs
    :defer t
    :config
    (define-key treemacs-mode-map (kbd "C-c C-p e") #'treemacs-edit-workspaces))

  ;; fix button klicking etc in various modes due to evil
  (evil-set-initial-state 'Custom-mode 'emacs)
  (evil-set-initial-state 'custom-new-theme-mode 'emacs)
  (evil-set-initial-state 'calendar-mode 'emacs)
  (evil-set-initial-state 'org-agenda-mode 'emacs)
  (evil-set-initial-state 'ebib-entry-mode 'emacs)

  (fset 'yes-or-no-p 'y-or-n-p) ; yes/no answering without <RET>

  ;; todo: for M-S-: and others
  ;; alternative to previous-line-or-history-element
  ;; implemented like zsh's history-beginning-search-backward
  ;; or readline's history-search-backward
  )


(defun jj/cstyle-keybinds ()
  (interactive)
  (local-set-key (kbd "C-c C-c") #'helm-make-projectile))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coding style definitions
;; ------------------------
;;
;; see all the possible variables at [emacsshare]/lisp/progmodes/cc-vars.el
;; c-set-stylevar-fallback 'c-offsets-alist
;;
;; get syntax/indent info by C-c C-s
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; closing template <> should line up.
(defun c-lineup-c++-template-args-cont (langelem)
  "Indentation of template params for a closing '>'.
return values:
0   : If the first non-whitespace char is '>'. Line it up under 'template'.
nil : Otherwise, return nil and run next lineup function."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "^[\t ]*>" (line-end-position) t)
        0)))


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

  (c-add-style
   "sft-java"
   '("java" ;; based on the builtin java style
     (c-offsets-alist . (
                         (arglist-intro . +)
                         (arglist-close . 0)
                         (statement-cont        . (first c-lineup-string-cont
                                                         c-lineup-cascaded-calls
                                                         c-lineup-assignments))
                         ))
     ))

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
                          ; defaults are defined in cc-vars.el
                          (access-label          . -)   ; public: or private:
                          (arglist-intro         . +)   ; first arg in newline
                          (arglist-cont          . 0)   ; wrapped function args: func(\nthisone
                                                        ; wrapped function args after func(arg,\nthisone:
                          (arglist-cont-nonempty . (first c-lineup-string-cont
                                                          c-lineup-cascaded-calls
                                                          ;c-lineup-argcont
                                                          c-lineup-arglist))
                          (arglist-close         . 0)   ; intentation of ) which closes tabbed args
                          (block-open            . 0)   ; { to open a block
                          (block-close           . 0)   ; } after a block
                          (brace-list-open       . 0)
                                                        ; first element in {\nthisone:
                          (brace-list-intro      . (first c-lineup-2nd-brace-entry-in-arglist
                                                          c-lineup-class-decl-init-+
                                                          +))
                                                        ;; much improved through emacs commit aa1a4cceca2d93d83c721ce83950230739073727
                                                        ; other elements in {\nelem\nthisone
                          (brace-list-entry      . 0)
                          (brace-list-close      . 0)   ; } of brace list
                          (brace-entry-open      . 0)   ; {...,\n{thisone
                          (case-label            . 0)   ; case 1337:
                          (statement-case-open   . 0)   ; { after case 1337:
                          (statement-case-intro  . +)   ; code after case 1337:
                          (cpp-macro             . [0])   ; #define, etcetc
                          (defun-block-intro     . +)   ; code in block: keyword (...) {\nthisone
                          (inclass               . +)   ; members of struct or class
                          (inexpr-class          . 0)   ; class declaration within expression
                          (inexpr-statement      . 0)   ; statement block within expression
                          (inher-intro           . +)   ; beginning of inheritance def
                          (inher-cont            . c-lineup-multi-inher)   ; inheritance continuation
                          (inlambda              . 0)   ; function body of a lambda
                          (inline-open           . 0)   ; brace opening in-class method definition a rolf()\n{<-
                          (innamespace           . 0)   ; namespace lol {\nthisstatement
                          (knr-argdecl-intro     . 0)
                          (label                 . 0)   ; gotolabel:
                          (member-init-intro     . +)   ; member initializing for class lol : var(val)
                          (member-init-cont      . c-lineup-multi-inher)   ; further members
                          (statement             . 0)
                          (statement-block-intro . +)   ; line in if () {\nthisline
                                                        ; int a =\nthisone or return B{\nthisone
                                                        ; or B{bla +\nthisone
                                                        ; TODO: return bla+\nthis should be indented to bla
                          (statement-cont        . (first c-lineup-string-cont
                                                          c-lineup-cascaded-calls
                                                          c-lineup-assignments))
                          (stream-op             . c-lineup-streamop)
                          (substatement          . +)
                          (substatement-label    . 0)
                          (substatement-open     . 0)
                          (template-args-cont    . (first c-lineup-c++-template-args-cont
                                                          c-lineup-template-args
                                                          +))
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
  (c-add-style "sft-cpp" sft-c-style)

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
  (use-package google-c-style
      :defer t
      :config
      (c-add-style "Google" google-c-style t))
  )
;;; end coding style definitions

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; loading stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jj/loadpath-discover ()
  "here manual elisp load paths can be defined.
directories in themes/ and lisp/ are automatically added to the respective load-paths."
  (let ((load-specs `((,(locate-user-emacs-file "themes/") . "custom-theme-load-path")
                      (,(locate-user-emacs-file "lisp/") . "load-path"))))
    (dolist (load-spec load-specs)
      (let ((load-dir (car load-spec))
            (load-var (cdr load-spec)))

        (when (file-directory-p load-dir)
          (dolist (load-dir-file (directory-files load-dir))
            (let ((load-dir-path (expand-file-name (concat load-dir load-dir-file))))

              (when (and (not (or (equal load-dir-file ".")
                                  (equal load-dir-file "..")))
                         (file-directory-p load-dir-path))

                (add-to-list (intern load-var) load-dir-path)))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; theming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jj/theming ()
  "theme customizations"
  (interactive)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html
  ;; https://develop.spacemacs.org/layers/+themes/theming/README.html

  ;;'((theme1 (face1 attributes...)
  ;;          (face2 attributes...)
  ;;          ...))
  (setq theming-modifications '())

  ;; apply theming modifications from 'user' theme to current theme
  ;;(spacemacs/update-theme)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; add a function to multiple hooks
(defun multi-hook-add (function hooks)
  (mapc (lambda (hook)
          (add-hook hook function))
        hooks))

(defconst jj/snippets
  '((python-mode
     . ("\
# -*- mode: snippet -*-
# name: mainargparse
# key: mainp
# group: argparser
# --
\"\"\"
${1:cool script}
\"\"\"

import argparse


def main():
    \"\"\"
    ${2:entry point}
    \"\"\"
    cli = argparse.ArgumentParser(description='$3')
    cli.add_argument($4)
    args = cli.parse_args()
    $0

if __name__ == \"__main__\":
    main()
"
        ;; -----
"\
# -*- mode: snippet -*-
# name: mainfunction
# key: mainf
# --
def main():
    $0

if __name__ == \"__main__\":
    main()
"
))
    (latex-mode . ("\
# -*- mode: snippet -*-
# name: two_columns
# key: columns
# --
\\begin{columns}[T]
    \\begin{column}{0.48\\textwidth}
        $0
    \\end{column}
    \\begin{column}{0.48\\textwidth}
    \\end{column}
\\end{columns}
")
    )))

(defun jj/yas-hook ()
  "so we don't need to carry around snippet files"
  (cl-loop for (snippet-mode . snippets) in jj/snippets
           collect
           (let* ((snippet-defs nil))
             (dolist (snippet snippets)
               (with-temp-buffer
                 (insert snippet)
                 (push (yas--parse-template) snippet-defs)
                 ))
             (when snippet-defs
               (yas-define-snippets snippet-mode snippet-defs)))))

;; config for all prog-modes
(defun jj/coding-hook ()
  ;; highlight important words
  (font-lock-add-keywords nil '(("\\<\\(TODO\\|todo\\|ASDF\\|asdf\\)" 1 font-lock-warning-face t)))

  ;; highlight word under cursor
  (when (fboundp 'idle-highlight-mode)
    (idle-highlight-mode t))

  (jj/codenav-keybinds))

;; c-like-language setup
(defun jj/cstyle-hook ()

  ;; standalone cc cc-mode doesn't run prog-mode-hook
  ;; since it doesn't derive prog-mode
  (when (not (provided-mode-derived-p 'c-mode '(prog-mode)))
    (spacemacs/run-prog-mode-hooks))

  ;; magic region formatting
  (with-library
    clang-format
    (global-set-key (kbd "C-M-<tab>") #'clang-format-region))

  ;; restore so we indent line or region.
  ;; (does no completion since we set tab-always-indent)
  ;; cc-mode replaces this the other way round.
  (substitute-key-definition 'c-indent-command
                             'indent-for-tab-command
                             c-mode-base-map)

  (add-to-list 'lsp-before-initialize-hook
               (lambda ()
                 (add-to-list 'lsp-clients-clangd-args "--header-insertion=never")))  ; don't auto-insert #includes

  ;; keybindings for clike languages
  (jj/cstyle-keybinds)

  ;; create codestyles
  (jj/create-codestyles)

  ;; default to sft style
  (c-set-style "sft-cpp")

  ;; default to smart-tab indentation
  (setq-local
    tab-width 4
    indent-tabs-mode t)

  ;; smart tabs: mix tabs and spaces the right way
  (smart-tabs-mode)
  (smart-tabs-advice c-indent-line c-basic-offset)
  (smart-tabs-advice c-indent-region c-basic-offset)

  (c-toggle-auto-newline nil) ; no automatic
  (c-toggle-auto-state nil)   ; newlines
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

;; java
(defun jj/java-coding-hook ()
  (c-set-style "sft-java")

  (setq-local
   indent-tabs-mode t
   tab-width 4
   c-basic-offset 4))

;; py
(defun jj/python-coding-hook ()
  (setq-local
    python-indent 4
    indent-tabs-mode nil
    tab-width 4
    whitespace-line-column 79)

  (setq flycheck-checker 'python-pylint
        flycheck-checker-error-threshold 300)

  ;; don't show anaconda mode error popup gaaarrhhgh
  (remove-hook 'anaconda-mode-response-read-fail-hook
                'anaconda-mode-show-unreadable-response)

  ;; https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/
  ;; disable docstring view
  (setq-local
    lsp-signature-doc-lines 0
    lsp-eldoc-enable-hover nil
    lsp-signature-auto-activate nil
    lsp-signature-render-documentation nil)

  ;; smart tabs
  (smart-tabs-mode)
  (smart-tabs-advice py-indent-line py-indent-offset)
  (smart-tabs-advice py-newline-and-indent py-indent-offset)
  (smart-tabs-advice py-indent-region py-indent-offset))

;; elisp
(defun jj/lisp-coding-hook ()
  (jj/codenav-keybinds)
  (setq-local
    indent-tabs-mode nil
    tab-width 8)

  ;; so plists and other things are properly indented
  (setq lisp-indent-function 'common-lisp-indent-function)
  (put 'cl-flet 'common-lisp-indent-function
       (get 'flet 'common-lisp-indent-function))
  (put 'cl-labels 'common-lisp-indent-function
       (get 'labels 'common-lisp-indent-function))
  (put 'if 'common-lisp-indent-function 2)
  (put 'dotimes-protect 'common-lisp-indent-function
       (get 'when 'common-lisp-indent-function))

  (prettify-symbols-mode)
  (smart-tabs-advice lisp-indent-line lisp-indent-offset)

  ;; when editing dir-locals, reload all affected files
  (when (and (buffer-file-name)
              (equal dir-locals-file  ;; doesn't support .dir-locals-2.el :)
                    (file-name-nondirectory (buffer-file-name))))
    (message (format "installing auto-refresh hook when saving %s" (current-buffer)))
    (add-hook 'after-save-hook
              'reload-dir-locals-below-default-dir
              nil  ;; no insert offset
              t ;; only for this buffer
              ))
  )

;; javascript / ecmascript
(defun jj/javascript-coding-hook ()
  (setq-local
    js-indent-level 2
    tab-width 2
    indent-tabs-mode nil))

;; TeX
(defun jj/latex-coding-hook ()
  ;; set latex indent offset so it doesn't fuck up
  ;; (i.e. use values != n*tab-width)
  (setq-local
    tab-width 4
    fill-column 76
    LaTeX-indent-level 4
    LaTeX-item-indent 0
    indent-tabs-mode nil

    company-minimum-prefix-length 2) ;; so completes start with 2 chars already

  ;; don't highlight long lines
  ;; whitespace-highlight may not be initialized yet, thus handle both cases..
  (let ((no-lines-tail (lambda ()
                          (setq whitespace-style (delete 'lines-tail whitespace-style)))))
    (if (boundp 'whitespace-style)
      (funcall no-lines-tail)
      (progn
        (add-hook 'global-whitespace-mode-hook no-lines-tail))))

  ;; other settings are in jj/defaults
  (setq-default TeX-master nil) ; query for master file
  (visual-line-mode t)
  (LaTeX-math-mode t)
  (turn-on-reftex)

  ;; so pdf-tools buffer gets reverted
  (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

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
  (setq-local
    tab-width 2
    indent-tabs-mode nil
    bibtex-comma-after-last-field t
    bibtex-align-at-equal-sign t))

;; html
(defun jj/html-coding-hook ()
  (setq-local
    sgml-basic-offset 4
    indent-tabs-mode t))

;; haskell
(defun jj/haskell-coding-hook ()
  ;; haskell interpreter: C-c C-z or C-c C-l
  ;;(haskell-indentation-mode)
  (setq-local
    indent-tabs-mode nil))

;; vhdl
(defun jj/vhdl-coding-hook ()
  (setq-local
    indent-tabs-mode nil)
  (smart-tabs-mode)
  (smart-tabs-advice vhdl-indent-line vhdl-basic-offset))

;; org-mode
(defun jj/org-mode-hook ()
  (visual-line-mode t)
  (setq-local
    indent-tabs-mode nil))

;; markdown-mode
(defun jj/markdown-mode-hook ()
  (setq-local
    indent-tabs-mode nil
    markdown-toc-indentation-space 2
    markdown-toc-header-toc-start "<!-- markdown-toc start -->"
    whitespace-line-column 400))

(defun jj/cmake-mode-hook ()
  (setq-local
    indent-tabs-mode t
    tab-width 4
    cmake-tab-width 4))

(defun jj/compilation-mode-hook ()
  ;; colorized compilation
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))


(defun jj/sql-indent-begin-block (context offset)
  "depending on the block kind, either do an indent, or not.
  no indent for defun, all other block kinds are indented."
  (let ((syntax (sqlind-syntax context))
        (anchor (sqlind-anchor-point context))
        (syntax-symbol (sqlind-syntax-symbol context)))

    (when (not (eq syntax-symbol 'in-begin-block))
      (error "indent-begin-block used for wrong syntax symbol '%s'"
             syntax-symbol))
    (let ((block-kind (nth 1 syntax))
          (block-label (nth 2 syntax)))
      (cond
        ;; defun block content should not be indented
        ((eq block-kind 'defun)
         offset)
        (t
         ;; indent all other types by basic offset
         (+ offset sqlind-basic-offset))
        ))))

(defun jj/sql-mode-hook ()
  (setq-local
    indent-tabs-mode nil
    tab-width 4
    sqlind-basic-offset 4
    sql-product 'postgres)

  ;; customize indentation - very simple basic-offset based indents
  (setq
   sqlind-indentation-offsets-alist
   `((select-clause 0)
     (in-select-clause +)
     (delete-clause 0)
     (in-delete-clause +)
     (update-clause 0)
     (in-update-clause +)
     (insert-clause 0)
     (in-insert-clause +)
     (in-begin-block jj/sql-indent-begin-block)
     ,@sqlind-default-indentation-offsets-alist))

  ;; TODO: detect psql prompt, or disable custom prompt
  ;;(sql-set-product-feature 'postgres)

  (smart-tabs-mode)
  (smart-tabs-advice sqlind-indent-line sqlind-basic-offset))

(defun jj/sql-interactive-mode-hook ()
  (let ((lval 'sql-input-ring-file-name)
        (rval 'sql-product))
    (if (symbol-value rval)
      (let ((filename
             (locate-user-emacs-file
              (concat "sql/"
                      (symbol-name (symbol-value rval))
                      "-history.sql"))))
        (set (make-local-variable lval) filename))
      (error
        (format "SQL history will not be saved because %s is nil"
                (symbol-name rval))))))

(defun jj/emacs-server-visit-hook ()
  (prefer-coding-system 'utf-8)
  (setq locale-coding-system 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (set-selection-coding-system 'utf-8))

(defun jj/shell-mode-hook ()
  ;; correct zsh coloring in shell:
  (ansi-color-for-comint-mode-on))

(defun jj/eshell-mode-hook ()
  (add-hook 'eshell-preoutput-filter-functions
            'ansi-color-filter-apply))


(defun jj/mode-hooks ()
  "hooks are registered here"

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
  (add-hook 'java-mode-hook              'jj/java-coding-hook)
  (add-hook 'LaTeX-mode-hook             'jj/latex-coding-hook)
  (add-hook 'bibtex-mode-hook            'jj/bibtex-coding-hook)
  (add-hook 'vhdl-mode-hook              'jj/vhdl-coding-hook)
  (add-hook 'org-mode-hook               'jj/org-mode-hook)
  (add-hook 'markdown-mode-hook          'jj/markdown-mode-hook)
  (add-hook 'cmake-mode-hook             'jj/cmake-mode-hook)
  (add-hook 'compilation-mode-hook       'jj/compilation-mode-hook)
  (add-hook 'sql-mode-hook               'jj/sql-mode-hook)
  (add-hook 'yas-global-mode-hook        'jj/yas-hook)
  (add-hook 'doc-view-mode-hook          'auto-revert-mode)
  (add-hook 'server-visit-hook           'jj/emacs-server-visit-hook)
  (add-hook 'shell-mode-hook             'jj/shell-mode-hook)
  (add-hook 'eshell-mode-hook            'jj/eshell-mode-hook)
  (add-hook 'compilation-filter-hook
            (lambda () (ansi-color-apply-on-region (point-min) (point-max))))

  ;; some modes don't inherit from prog-mode...
  (multi-hook-add
   (lambda ()
     ;; TODO: (put 'something-mode 'derived-mode-parent 'prog-mode)
     (spacemacs/run-prog-mode-hooks))
   '(python-mode-hook
     haskell-mode-hook))
  )

(defun jj/load-device-settings ()
  "include device-specific configuration"
  (interactive)

  (let ((device-config (locate-user-emacs-file "device.el")))
    (when (file-exists-p device-config)
      (load-file device-config))))

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
  ;; for debugging whatever error occurs in this file and emacs itself
  ;(setq debug-on-error t)
  ;; to watch variable writes, there's M-x debug-watch
  ;(debug-watch 'indent-line-function)
  ;; breakpoints
  ;(debug-on-entry 'enable-theme)
  ;; debug-on-event: sigusr2 also triggers debugger!
  ;; to debug slowness somewhere, use M-x profiler-start, and profiler-report!
  ;; To debug on C-g, use M-x toggle-debug-on-quit

  ;; store customizations in extra file
  (setq custom-file (locate-user-emacs-file "custom.el"))
  ;; load customization file if it exists.
  (when (file-exists-p custom-file)
    (load custom-file))

  ;; https://github.com/Somelauw/evil-org-mode/issues/93
  ;; https://github.com/syl20bnr/spacemacs/issues/15123
  (when (not (boundp 'evil-redirect-digit-argument))
  (defmacro evil-redirect-digit-argument (map keys target)
     `(define-key ,map ,keys ,target)))

  ;; load external packages
  (jj/loadpath-discover)

  ;; mode hooks need to be in user-init since user-config is executed
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

  (jj/load-packages)
  (jj/defaults)
  (jj/theming)
  (jj/scrolling)
  (jj/keybindings)

  (jj/load-device-settings)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
