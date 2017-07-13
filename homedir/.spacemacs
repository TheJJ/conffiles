;; -*- mode: emacs-lisp -*-
;; jj's spacemacs configuration
;; Copyright (c) 2016-2017 Jonas Jelten <jj@stusta.net>
;; Licensed GPLv3 or later


(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
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
   ;; If non-nil layers with lazy install support are lazy installed.
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load.
   dotspacemacs-configuration-layers
   '(
     ;; ----------------------------------------------------------------
     ;; active layers select which of the magic of spacemacs
     ;; shall be activated.

     (auto-completion :variables
                      auto-completion-enable-sort-by-usage t
                      auto-completion-enable-help-tooltip t)
     better-defaults
     bibtex
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode
            c-c++-enable-clang-support t)
     csv
     emacs-lisp
     extra-langs
     git
     gtags
     (haskell :variables
              haskell-completion-backend 'intero)
     helm
     html
     javascript
     latex
     lua
     (markdown :variables
               markdown-live-preview-engine 'vmd)
     nlinum
     org
     python
     ranger
     rust
     semantic
     shell
     shell-scripts
     smex
     ;;spell-checking
     sql
     systemd
     (syntax-checking :variables
                      syntax-checking-enable-by-default nil
                      syntax-checking-enable-tooltips t)
     version-control
     yaml
     (shell :variables
            shell-default-height 30
            shell-default-position 'bottom)
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     bison-mode
     ag
     )
   ;; A list of packages that cannot be updated.
   dotspacemacs-frozen-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages
   '(
     smartparens
     auto-highlight-symbol
     )

   ;; Defines the behaviour of Spacemacs when installing packages.
   ;; Possible values are `used-only', `used-but-keep-unused' and `all'.
   ;; `used-only' installs only explicitly used packages and uninstall any
   ;; unused packages as well as their unused dependencies.
   ;; `used-but-keep-unused' installs only the used packages but won't uninstall
   ;; them if they become unused. `all' installs *all* packages supported by
   ;; Spacemacs and never uninstall them. (default is `used-only')
   dotspacemacs-install-packages 'used-only))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. Note that checking for
   ;; new versions works via git commands, thus it calls GitHub services
   ;; whenever you start Emacs. (default nil)
   dotspacemacs-check-for-update nil
   ;; If non-nil, a form that evaluates to a package directory. For example, to
   ;; use different package directories for different Emacs versions, set this
   ;; to `emacs-version'.
   dotspacemacs-elpa-subdirectory nil
   ;; One of `vim', `emacs' or `hybrid'.
   ;; `hybrid' is like `vim' except that `insert state' is replaced by the
   ;; `hybrid state' with `emacs' key bindings. The value can also be a list
   ;; with `:variables' keyword (similar to layers). Check the editing styles
   ;; section of the documentation for details on available variables.
   ;; (default 'vim)
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
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
   ;; `recents' `bookmarks' `projects' `agenda' `todos'."
   ;; List sizes may be nil, in which case
   ;; `spacemacs-buffer-startup-lists-length' takes effect.
   dotspacemacs-startup-lists '((recents . 5)
                                (projects . 7))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(monokai
                         deeper-blue
                         spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         zenburn)
   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font, or prioritized list of fonts. `powerline-scale' allows to
   ;; quickly tweak the mode-line size to make separators look not too crappy.
   dotspacemacs-default-font '("DejaVu Sans Mono"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.2)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
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
   ;; (default "C-M-m")
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; If non nil `Y' is remapped to `y$' in Evil states. (default nil)
   dotspacemacs-remap-Y-to-y$ nil
   ;; If non-nil, the shift mappings `<' and `>' retain visual state if used
   ;; there. (default t)
   dotspacemacs-retain-visual-state-on-shift t
   ;; If non-nil, J and K move lines up and down when in visual mode.
   ;; (default nil)
   dotspacemacs-visual-line-move-text nil
   ;; If non nil, inverse the meaning of `g' in `:substitute' Evil ex-command.
   ;; (default nil)
   dotspacemacs-ex-substitute-global nil
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
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
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-transient-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.4
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 90
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 90
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters point
   ;; when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; Control line numbers activation.
   ;; If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
   ;; `text-mode' derivatives. If set to `relative', line numbers are relative.
   ;; This variable can also be set to a property list for finer control:
   ;; '(:relative nil
   ;;   :disabled-for-modes dired-mode
   ;;                       doc-view-mode
   ;;                       markdown-mode
   ;;                       org-mode
   ;;                       pdf-view-mode
   ;;                       text-mode
   ;;   :size-limit-kb 1000)
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil, advise quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup nil
   ))



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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; smart tabs, mix tabs and spaces (fak yea)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defadvice align (around smart-tabs activate)
           (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice align-regexp (around smart-tabs activate)
           (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-relative (around smart-tabs activate)
           (let ((indent-tabs-mode nil)) ad-do-it))

(defadvice indent-according-to-mode (around smart-tabs activate)
           (let ((indent-tabs-mode indent-tabs-mode))
             (if (memq indent-line-function
                       '(indent-relative indent-relative-maybe))
               (setq indent-tabs-mode nil))
             ad-do-it))

(defmacro smart-tabs-advice (function offset)
  `(progn
     ;(defvaralias ',offset 'tab-width)
     (defadvice ,function (around smart-tabs activate)
                (cond
                  (indent-tabs-mode
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
;; enable funny modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun jj/modes ()
  (column-number-mode t)
  (cua-selection-mode t)
  (delete-selection-mode t)
  (display-battery-mode t)
  (xterm-mouse-mode t)
  (global-whitespace-mode t)
  (icomplete-mode t)

  (put 'scroll-left 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  )


(defun jj/defaults ()
  ;; push the mouse out of the way when the cursor approaches.
  (mouse-avoidance-mode 'cat-and-mouse)

  (setq-default visible-bell nil            ; disable window flashing
                ring-bell-function 'ignore) ; and also disable the sound

  (setq ido-enable-flex-matching t
        ido-case-fold t
        ido-use-virtual-buffers t
        ;;completion-ignored-extensions '(".pdf" ".aux" ".toc" ".tex~")
        ;;ido-ignore-extensions t
        ido-file-extensions-order '(".c" ".cpp" ".h" ".py" ".tex" ".bib" ".hs"))

  (setq nlinum-relative-redisplay-delay 0.2) ; relative number redisplay

  (setq indicate-empty-lines t
        transient-mark-mode t
        gud-tooltip-mode t
        lazy-highlight t                 ; highlight occurrences
        lazy-highlight-cleanup nil       ; keep search term highlighted
        lazy-highlight-max-at-a-time nil ; all occurences in file
        font-lock-maximum-decoration t   ; decoration level: maximum
        auto-compression-mode t          ; deal with compressed files
        blink-cursor-mode nil            ; don't blink the cursor
        mouse-yank-at-point t            ; paste as cursor instead of mouse position
        )

  ;; default mode for new buffers
  (setq-default major-mode 'text-mode)
  (setq initial-major-mode 'text-mode)

  ;; indentation defaults
  ;;(setq-default indent-tabs-mode t)
  (setq-default indent-line-function 'insert-tab)
  (setq-default tab-width 4)
  (setq-default whitespace-line-column 400)

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
        (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
               special-display-buffer-names))

  ;; no tool bar for these buffers
  (add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))

  ;; don't echo passwords when using interactive terminal programs
  (add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

  ;; man pages with clickable links
  (add-hook 'Man-mode-hook 'goto-address)
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; fight the whitespace crimes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj/whitespace-highlight ()
  ;; see whitespace.el
  (interactive)

  (setq whitespace-display-mappings
        '(
          ;;(space-mark   ?\     [?\u00B7]     [?.])      ; space - centered dot
          (space-mark   ?\xA0  [?\u00A4]     [?_])        ; hard space - currency
          (newline-mark ?\n    [?¬ ?\n]  [?$ ?\n])        ; eol - ¬ symbol
          (tab-mark     ?\t    [?∘ ?\t] [?> ?\t])         ; tab - ∘ symbol
          )))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; better mouse scrolling
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj/mousescroll ()
  ;;mouse-wheel scrolling
  (setq
   mouse-wheel-scroll-amount '(1 ((shift) . 1)
                                 ((control)))            ; one line at a time
   mouse-wheel-progressive-speed t                       ; accelerate scrolling
   mouse-wheel-follow-mouse t)                           ; scroll- window under mouse

  (setq
   scroll-preserve-screen-position t                     ; keep relative column position when scrolling
   scroll-margin 4                                       ; start scrolling n lines before window borders
   scroll-conservatively 10                              ; scroll up to n lines to bring pointer back on screen
   scroll-step 0                                         ; try scrolling n lines when pointer moves out
   auto-window-vscroll nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; semantic symbol jumping
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj/semantic-completion-keybinds ()
  (interactive)
  (local-set-key [M-S-mouse-1] 'semantic-ia-fast-mouse-jump)
  (local-set-key (kbd "M-g d") 'semantic-ia-fast-jump)
  (local-set-key (kbd "M-g f") 'semantic-symref)
  (local-set-key (kbd "M-g i") 'semantic-decoration-include-visit)
  (local-set-key (kbd "M-g S") 'semantic-complete-jump)
  (local-set-key (kbd "M-g s") 'semantic-ia-show-doc)
  (local-set-key (kbd "M-g h") 'semantic-analyze-proto-impl-toggle)

  ;;(local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
  ;;(local-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol)
  ;;(local-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)
  ;;(local-set-key "\C-c=" 'semantic-decoration-include-visit)
  ;;(local-set-key "\C-cs" 'semantic-ia-show-summary)

  ;; menubar entry for detected symbols
  (add-hook 'semantic-init-hooks (lambda ()
                                   (imenu-add-to-menubar "Stuff"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; funny functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name) (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

;; argument lineup by tabs only
(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset))
         )
    (* (max steps 1)
       c-basic-offset)))

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

(defun nowrap-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun call-pop-kill-ring (func)
  (funcall func)
  ; emacs 24.3 needed that:
  ;(setq kill-ring (cdr kill-ring))
  )

(defun kill-word-no-kill-ring ()
  (interactive)
  (call-pop-kill-ring (lambda () (kill-word 1)))
  )

(defun backward-kill-word-no-kill-ring ()
  (interactive)
  (call-pop-kill-ring (lambda () (backward-kill-word 1)))
  )

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; set kaschtomaisd key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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

  ; jlk; stuff
  ;;(global-set-key (kbd "M-j") 'backward-char)
  ;;(global-set-key (kbd "M-;") 'forward-char)
  ;;(global-set-key (kbd "M-l") 'previous-line)
  ;;(global-set-key (kbd "M-k") 'next-line)

  ;;(global-set-key (kbd "C-M-j") 'backward-word)
  ;;(global-set-key (kbd "C-M-;") 'forward-word)
  ;;(global-set-key (kbd "C-M-l") 'backward-paragraph)
  ;;(global-set-key (kbd "C-M-k") 'forward-paragraph)

  (global-set-key (kbd "C-k") 'kill-line)
  (global-set-key (kbd "C-l") 'recenter-top-bottom)
  (global-set-key (kbd "C-j") 'nowrap-newline-and-indent)

  ;; word deletion
  (global-set-key (kbd "C-<delete>")    'kill-word-no-kill-ring)
  (global-set-key (kbd "C-<backspace>") 'backward-kill-word-no-kill-ring)

  ;; terminal fu
  (global-set-key (kbd "M-[ d") 'left-word)  ;backward-word
  (global-set-key (kbd "M-[ c") 'right-word) ;forward-word
  (global-set-key (kbd "M-[ a") 'backward-paragraph)
  (global-set-key (kbd "M-[ b") 'forward-paragraph)

  ;; newline magic
  ;(global-set-key (kbd "RET") 'electric-newline-and-maybe-indent)
  (global-set-key (kbd "<C-return>") 'newline)
  (global-set-key (kbd "C-c C-a") 'mark-whole-buffer)

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

  (global-set-key (kbd "C-c g") 'magit-status) ;git stuff

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

  ;; force company completion: TODO: spacemacs adaption
  (global-set-key (kbd "S-<tab>") 'indent-for-tab-command)

  ;; TODO: auto-highlight-symbol
  ;; (ahs-backward) M-<up>

  ;;unset unneeded keys
  ;;(global-unset-key (kbd "C-t")) ; annoying character swapping

  (fset 'yes-or-no-p 'y-or-n-p) ; yes/no answering without <RET>
  )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; coding style definitions
;; ------------------------
;;
;; see all the possible variables at [emacsshare]/lisp/progmodes/cc-vars.el
;; c-set-stylevar-fallback 'c-offsets-alist
;;
;; get syntax/indent info by C-c C-s
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun jj/c-codestyle ()
  ;; codestyle defaults, will be overwritten

  ;; linux kernel indentation style
  (defconst kernel-c-style
    '("linux" ;; based on the builtin linux style
      (c-offsets-alist . (
                          (arglist-cont-nonempty c-lineup-gcc-asm-reg c-lineup-arglist)
                          (arglist-close . 0)
                          ))
      ))

  ;; sft coding style
  (defconst sft-c-style
    '("linux"  ;; base it on linux code style
      (c-doc-comment-style        . javadoc)
      (indent-tabs-mode           . t)
      (c-basic-offset             . 4)
      (c-tab-always-indent        . t)
      (c-comment-only-line-offset . 4)
      (c-hanging-braces-alist     . (
                                     (brace-list-open)
                                     (substatement-open after)
                                     ))
      (c-hanging-colons-alist     . (
                                     (access-label after)
                                     (case-label after)
                                     (inher-intro)
                                     (label after)
                                     (member-init-intro before)
                                     ))
      (c-cleanup-list             . (
                                     scope-operator
                                     empty-defun-braces
                                     defun-close-semi
                                     ))
      (c-comment-only-line-offset . 0)
      (c-hanging-braces-alist . (
                                 (arglist-cont-nonempty)
                                 (block-close . c-snug-do-while)
                                 (brace-entry-open)
                                 (brace-list-open)
                                 (substatement-open before after)
                                 ))
      (c-cleanup-list . (brace-else-brace))
      (c-offsets-alist . (
                          ; arg indent helper funcs: c-lineup-*
                          ; arglist = indent to matching (|here, asdf
                          ; argcont = indent to (asdf, |here
                          ; casecaded calls = ->lol\n->stuff
                          ; absolute offset: [0]
                          (access-label          . -)   ; public: or private:
                          (arglist-intro         . +)   ; first arg in newline
                          (arglist-cont          . 0)   ; wrapped function args: func(\nthisone
                                                        ; wrapped function args after func(arg,\nthisone:
                          (arglist-cont-nonempty . (max c-lineup-arglist
                                                        c-lineup-string-cont
                                                        c-lineup-cascaded-calls))
                          (arglist-close         . 0)   ; intentation of ) which closes tabbed args
                          (block-open            . 0)   ; { to open a block
                          (block-close           . 0)   ; } after a block
                          (brace-list-intro      . +)   ; first element in {\nthisone
                          (brace-list-entry      . 0)   ; other elements in {\nelem\nthisone
                          (case-label            . 0)   ; case 1337:
                          (statement-case-open   . 0)   ; { after case 1337:
                          (statement-case-intro  . +)   ; code after case 1337:
                          (defun-block-intro     . +)   ; beginning of keyword (...) { stuff  }
                          (inclass               . +)   ; members of struct or class
                          (inher-intro           . +)   ; beginning of inheritance def
                          (inher-cont            . c-lineup-multi-inher)   ; inheritance continuation
                          (inline-open           . +)
                          (innamespace           . 0)   ; namespace lol {\nthisstatement
                          (knr-argdecl-intro     . -)
                          (knr-argdecl-intro     . 0)
                          (label                 . 0)   ; gotolabel:
                          (member-init-intro     . +)   ; member initializing for class lol : var(val)
                          (member-init-cont      . c-lineup-multi-inher)   ; further members
                          (statement             . 0)
                          (statement-block-intro . +)   ; line in if () {\nthisline
                          (statement-case-open   . +)
                          (statement-cont        . (max c-lineup-assignments c-lineup-cascaded-calls c-lineup-string-cont))
                          (substatement          . +)
                          (substatement-label    . 0)
                          (substatement-open     . 0)
                          (substatement-open     . 0)
                          (template-args-cont    . c-lineup-template-args)
                          (topmost-intro         . 0)   ; indentation of file start
                          (topmost-intro-cont    . c-lineup-topmost-intro-cont)
                          (cpp-macro             . [0])   ; #define, etcetc
                          ))

      ;; information about indent parsing on TAB
      ;; this is also triggered by C-c C-s
      (c-echo-syntactic-information-p . nil))
    "The SFT C programming style"
    ))
;;; end coding style definitions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj/mode-hooks ()
  (add-hook 'LaTeX-mode-hook (lambda ()
                               (setq TeX-PDF-mode t
                                     TeX-auto-save t
                                     TeX-parse-self t
                                     reftex-plug-into-AUCTeX t)
                               (setq-default TeX-master nil) ; query for master file
                               (visual-line-mode t)
                               (LaTeX-math-mode t)
                               (turn-on-reftex)
                               (turn-off-auto-fill)))

  (add-hook 'doc-view-mode-hook 'auto-revert-mode)

  (add-hook 'server-visit-hook (lambda ()
                                 (prefer-coding-system 'utf-8)
                                 (setq locale-coding-system 'utf-8)
                                 (set-terminal-coding-system 'utf-8)
                                 (set-keyboard-coding-system 'utf-8)
                                 (set-selection-coding-system 'utf-8)))

  ;; correct zsh coloring in shell:
  (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


  ;; press ~ for reaching home directly in ido-mode
  (add-hook 'ido-setup-hook
            (lambda ()
              (define-key ido-file-completion-map
                (kbd "~")
                (lambda ()
                  (interactive)
                  (if (looking-back "/")
                      (insert "~/")
                    (call-interactively 'self-insert-command))))))


  ;; comment parsing and word highlinging
  (add-hook 'c++-mode-hook
   '(lambda ()

      ;; javadoc-style comments in c++
      (add-to-list 'c-doc-comment-style '(c++-mode . javadoc))

      ;; placing regexes into `c-mode-common-hook' may work but their
      ;; evaluation order matters.
      (font-lock-add-keywords
       nil '(
             ;; missing C++11 keywords
             ("\\<\\(static_assert\\)\\>" . font-lock-keyword-face)

             ;; custom defined types
             ("\\<[A-Za-z_]+[A-Za-z_0-9]*_\\(t\\)\\>" . font-lock-type-face)
             ))
      ) t)

  ;; main coding configuration function
  (defun jj/coding-hook ()
    (jj/c-codestyle)
    (jj/semantic-completion-keybinds)
    (auto-revert-mode t)
    (font-lock-add-keywords nil '(("\\<\\(TODO\\|todo\\|ASDF\\|asdf\\|TMP\\|FIXME\\|fixme\\)" 1 font-lock-warning-face t)))
    (jj/whitespace-highlight)
    )

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; special language-specific hooks
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; hook for all c-like languages
  (defun jj/cstyle-hook ()

    ;; magic region formatting
    (with-library
     clang-format
     (global-set-key (kbd "C-M-<tab>") 'clang-format-region))

    (c-add-style "sftstyle"     sft-c-style)
    (c-add-style "linux-kernel" kernel-c-style)

    ;; default to sft style
    (c-set-style "sftstyle")

    (setq tab-width 4
          indent-tabs-mode t)

    (c-toggle-auto-newline nil) ; no automatic
    (c-toggle-auto-state nil)   ; newlines

    ;; kernel code style: tabwidth=8, kernelstyle
    (when
        (and buffer-file-name
             (string-match
              (expand-file-name "/usr/src/linux") buffer-file-name))
      (c-set-style "linux-kernel")

      (setq tab-width 8
            indent-tabs-mode t))

    ;; smart tabs
    (smart-tabs-advice c-indent-line c-basic-offset)
    (smart-tabs-advice c-indent-region c-basic-offset)
    )

  ;; c, c++
  (defun jj/c-coding-hook ()
    (setq flycheck-gcc-language-standard "c++14")
    (jj/cstyle-hook))

  ;; py
  (defun jj/python-coding-hook ()
    (setq python-indent 4)
    (setq indent-tabs-mode nil)
    (setq tab-width 4)
    (setq-default whitespace-line-column 79)

    (setq flycheck-checker 'python-pylint
          flycheck-checker-error-threshold 300)

    ;; don't show anaconda mode error popup gaaarrhhgh
    (remove-hook 'anaconda-mode-response-read-fail-hook
                 'anaconda-mode-show-unreadable-response)

    ;; smart tabs
    (smart-tabs-advice py-indent-line py-indent-offset)
    (smart-tabs-advice py-newline-and-indent py-indent-offset)
    (smart-tabs-advice py-indent-region py-indent-offset))

  ;; elisp
  (defun jj/lisp-coding-hook ()
    (setq indent-tabs-mode nil)
    (setq tab-width 8)
    (prettify-symbols-mode)
    ;;(setq lisp-indent-offset 4)
    ;;(setq lisp-body-indent 4)
    )

  ;; javascript / ecmascript
  (defun jj/javascript-coding-hook ()
    (setq js-indent-level 2)
    (setq tab-width 2)
    (setq indent-tabs-mode nil))

  ;; TeX
  (defun jj/latex-coding-hook ()
    ;; set latex indent offset so it doesn't fuck up
    ;; (i.e. use values != n*tab-width)
    (setq tab-width 4)
    (setq LaTeX-indent-level 4)
    (setq LaTeX-item-indent -4)
    (setq indent-tabs-mode nil)
    (setq TeX-parse-self t)
    (setq TeX-auto-save t)

    ;; minted codehighlighting needs shell execution for pygments
    (add-to-list 'TeX-command-list
                 '("LaTeX-shellescape" "%`%l -shell-escape %(mode) %(extraopts) %' %t" TeX-run-TeX nil
                   (latex-mode doctex-mode) :help "Run LaTeX -shell-escape") t))

  ;; BibTeX
  (defun jj/bibtex-coding-hook ()
    (setq tab-width 2)
    (setq indent-tabs-mode nil)
    (setq bibtex-comma-after-last-field t)
    (setq bibtex-align-at-equal-sign t))

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
    (smart-tabs-advice vhdl-indent-line vhdl-basic-offset)
    (setq vhdl-indent-tabs-mode t))

  ;; org-mode
  (defun jj/org-mode-hook ()
    (setq org-log-done nil
          indent-tabs-mode nil))

  ;; markdown-mode
  (defun jj/markdown-mode-hook ()
    (setq indent-tabs-mode nil))

  (defun jj/cmake-mode-hook ()
    (setq indent-tabs-mode t)
    (setq cmake-tab-width 4))

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
  (add-hook 'c-mode-common-hook          'jj/c-coding-hook)
  (add-hook 'LaTeX-mode-hook             'jj/latex-coding-hook)
  (add-hook 'bibtex-mode-hook            'jj/bibtex-coding-hook)
  (add-hook 'vhdl-mode-hook              'jj/vhdl-coding-hook)
  (add-hook 'org-mode-hook               'jj/org-mode-hook)
  (add-hook 'markdown-mode-hook          'jj/markdown-mode-hook)
  (add-hook 'cmake-mode-hook             'jj/cmake-mode-hook)


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
  )

;; we have a graphical window
(defun jj/window-setup ()
  (message "running in windowed mode")
  (setq confirm-kill-emacs 'y-or-n-p)
  )

;; we're running on tty
(defun jj/terminal-setup ()
  (message "running in terminal mode")
  (custom-set-faces
   '(default ((t (:background "#000000"))))
   '(semantic-highlight-func-current-tag-face ((t (:background "gray15")))))
  (setq confirm-kill-emacs nil)
  ;; TODO: disable hl-line-mode
  )

;; change colors according to display
(defun jj/display-setup ()
  (if window-system
      (jj/window-setup)
    (jj/terminal-setup)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; spacemacs init hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  ;; debugging n stuff
  (setq debug-on-error nil)
  (jj/mode-hooks)
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place your code here."

  (jj/modes)
  (jj/defaults)
  (jj/display-setup)
  (jj/mousescroll)
  (jj/keybindings)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
;;customized variables, set by `customize`
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(backward-delete-char-untabify-method nil)
 '(company-auto-complete nil)
 '(company-auto-complete-chars (quote (32 95 40 41 119 46 39)))
 '(company-clang-arguments (quote ("-std=c++14")))
 '(company-clang-executable "/usr/bin/clang++")
 '(company-ghc-show-info (quote oneline))
 '(company-idle-delay 0.15)
 '(company-minimum-prefix-length 2)
 '(cua-auto-tabify-rectangles nil)
 '(cua-enable-cua-keys nil)
 '(cua-mode t nil (cua-base))
 '(doc-view-continuous t)
 '(fill-column 76)
 '(inhibit-startup-screen t)
 '(jit-lock-defer-time 0.01)
 '(nxml-child-indent 1)
 '(python-fill-docstring-style (quote symmetric))
 '(scroll-bar-mode (quote right))
 '(semantic-python-dependency-system-include-path nil)
 '(whitespace-display-mappings
   (quote
    ((space-mark 160
                 [164]
                 [95])
     (newline-mark 10
                   [172 10]
                   [36 10])
     (tab-mark 9
               [8728 9]
               [62 9]))))
 '(whitespace-style
   (quote
    (face tabs trailing newline indentation space-before-tab space-after-tab space-mark tab-mark newline-mark lines-tail))))

;;customized font colors and sizes
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "#14151f" :foreground "#f5f5f5" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))) nil "main font and background")
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil))))
 '(evil-search-highlight-persist-highlight-face ((t (:inherit region :background "#4040d0"))))
 '(font-lock-comment-face ((t (:foreground "gray80"))))
 '(font-lock-doc-face ((t (:foreground "#cca522"))))
 '(font-lock-function-name-face ((t (:foreground "goldenrod"))))
 '(font-lock-keyword-face ((t (:foreground "deep sky blue" :weight normal))))
 '(font-lock-string-face ((t (:foreground "burlywood"))))
 '(font-lock-variable-name-face ((t (:foreground "SeaGreen2"))))
 '(hl-line ((t (:inherit highlight :background "midnight blue"))))
 '(isearch ((t (:inherit region :background "#A6E22E" :foreground "black"))))
 '(magit-item-highlight ((t (:inherit nil))))
 '(region ((t (:background "#3030d0"))))
 '(semantic-decoration-on-unknown-includes ((t (:background "#203030"))))
 '(semantic-highlight-func-current-tag-face ((t (:background "gray15"))))
 '(whitespace-indentation ((t (:foreground "#797979"))))
 '(whitespace-space ((t (:background "default"))))
 '(whitespace-space-after-tab ((t (:foreground "#101540"))))
 '(whitespace-tab ((t (:background "#292929" :foreground "#a9a9a9"))))
 '(widget-field ((t (:background "gray25")))))

