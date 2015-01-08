;; JJ's emacs configuration
;
; designed to be portable & epic
;
; (c) 2011-2014 Jonas Jelten
;
;
; released under GPLv3 or later


(setq debug-on-error nil)

;customized variables, set by `customize`
(custom-set-variables
  '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
  '(backward-delete-char-untabify-method nil)
  '(custom-enabled-themes (quote (deeper-blue)))
  '(doc-view-continuous t)
  '(fill-column 76)
  '(inhibit-startup-screen t)
  '(scroll-bar-mode (quote right))
  '(semantic-python-dependency-system-include-path (quote ("/usr/lib64/python3.4/")))
  '(send-mail-function (quote sendmail-send-it)))

;customized font colors and sizes
(custom-set-faces
  '(default ((t (:inherit nil :stipple nil :background "#14151f" :foreground "#f5f5f5" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 98 :width normal :foundry "unknown" :family "DejaVu Sans Mono"))) nil "main font and background")
  '(font-lock-comment-face ((t (:foreground "gray80"))))
  '(hl-line ((t (:inherit highlight :background "midnight blue"))))
  '(magit-item-highlight ((t (:inherit nil))))
  '(region ((t (:background "#3030d0"))))
  '(semantic-decoration-on-unknown-includes ((t (:background "#203030"))))
  '(semantic-highlight-func-current-tag-face ((t (:background "gray15"))))
  '(whitespace-indentation ((t (:foreground "#797979"))))
  '(whitespace-space ((t (:background "default"))))
  '(whitespace-space-after-tab ((t (:background "RoyalBlue4" :foreground "firebrick"))))
  '(whitespace-tab ((t (:background "#292929" :foreground "#a9a9a9")))))


(defvar senator-tag-ring (make-ring 20)
  "Ring of tags for use with cut and paste.")

;(condition-case err
;                (load-auctex)
;                (file-error (message "auctex not found, skipping load") nil))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; enable funny modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(global-ede-mode t)
(column-number-mode t)
(show-paren-mode t)
(cua-selection-mode t)
(size-indication-mode t)
(delete-selection-mode t)
(display-battery-mode t)
(xterm-mouse-mode t)

(ede-enable-generic-projects)

(setq global-auto-complete-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; enable auto completion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when (require 'auto-complete nil 'noerror)

  (defun ac-enable-popups ()
    "Enable auto-complete-mode"
    (interactive)

    ;(message "enabling auto-complete-mode")
    (require 'auto-complete-config)
    (ac-config-default)
    (auto-complete-mode t)
    (global-auto-complete-mode t)

    (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict.d")
    (setq ac-comphist-file (expand-file-name "~/.emacs.d/ac-comphist.dat"))

    (setq-default ac-auto-start 3) ;start completing after n chars

    ;(ac-set-trigger-key "TAB")

    (add-to-list 'ac-modes 'haskell-mode)
    (add-to-list 'ac-modes 'python-mode)
    (add-to-list 'ac-modes 'latex-mode)
    (add-to-list 'ac-modes 'c-mode)
    (add-to-list 'ac-modes 'c++-mode)
    (add-to-list 'ac-modes 'fundamental-mode)

    (setq ac-use-fuzzy t)
    (setq ac-ignore-case 'smart) ; ignore case except contains upper
    ;(setq ac-menu-height 6) ; number of lines in completion popup
    )



    ;; (defun ac-semantic-documentation (symbol)
    ;;   (let* ((tag (assoc symbol ac-semantic-candidates-cache))
    ;;          (doc (semantic-documentation-for-tag tag))
    ;;          (summary (funcall semantic-idle-summary-function tag nil t)))
    ;;     (and (stringp doc)
    ;;          (string-match "\n*\\(.*\\)$" doc)
    ;;          (setq doc (match-string 1 doc)))
    ;;     (concat (funcall semantic-idle-summary-function tag nil t)
    ;;             (when doc
    ;;               (if (< (+ (length doc) (length summary) 4) (window-width))
    ;;                 " -- "
    ;;                 "\n"))
    ;;             doc)))




  ;TODO: use jedi, ropemacs, pymacs?

  ; jj's attempt of epic autocompletion with cedet and auto-complete-mode
  ; todo: adapt semantic-ia-complete-symbol for this.

  ;with-no-warnings, ignore-errors

  ; creates ac-semantic-ng source for auto-complete-mode
  (defun ac-define-semantic-source ()

    (defvar semantic-ac-completion-candidates nil)
    ; prefix may be the past text for the current completion
    ; each source is evaluated on each keypress.
    ; most sources are only active when required > ac-start chars
    ; semantic is active at 0, but the prefix has to be . or ->
    ; semantic is not active any more if the prefix changes.
    ;
    ; the prefix c-dot-ref may be too restrictive,
    ; as we wanna keep going after the ->
    ;
    ; ac-point -> starting point
    ; ac-prefix -> prefix string of completion target
    ; ac-limit -> max number of candidates
    (defun ac-semantic-completion-candidates (prefix)
      (message (format "get candidates, prefix: %s" prefix))
      (let* ((ctx (semantic-analyze-current-context nil))
             (syms (semantic-analyze-possible-completions ctx))
             )

        (when syms
          (mapcar 'semantic-tag-name syms)
          )
        )
      )


    ; this function returns the start of the completion position
    ; nil means don't try semantic completion
    (defun ac-semantic-ng-prefix ()
      ; ?: is a non-capturing group
      (if (re-search-backward "\\(?:\\.\\|->\\)\\([_a-zA-Z0-9]*\\)\\=" nil t)
          (match-beginning 1) ; return first () position match
        )
      )


    (ac-define-source semantic-ng
      '((available . (require 'semantic/ia nil t))
        (init setq ac-semantic-ng-cache nil)
        (candidates . (ac-semantic-completion-candidates ac-prefix))
        (prefix . ac-semantic-ng-prefix)
        (requires . 0) ; number of chars required for completion
        ;(document . ac-semantic-documentation)
        (symbol . "n") ;maybe get this dynamically
        ;(action . ac-start) ;ran when hitting enter
        )))


  (defun ac-enable-semantic ()
    "Enable semantic auto completion for current mode"
    (interactive)

    (semantic-mode t)

    (require 'cedet)
    (require 'semantic)
    (require 'semantic/sb)
    (require 'semantic/ia)
    (require 'semantic/symref)
    (require 'semantic/bovine/gcc)
    (require 'semantic/bovine/c)


    (add-hook 'semantic-init-hooks (lambda ()
                                     (imenu-add-to-menubar "Stuff")
                                     ))


    ;auto-complete-auctex.el

    ;(global-semantic-idle-completions-mode t)
    ;(global-semantic-idle-summary-mode t)
    ;(global-semantic-idle-breadcrumbs-mode t)  ;show tag summary in header line
    (global-semantic-decoration-mode t)
    (global-semantic-highlight-func-mode t)
    (global-semantic-show-unmatched-syntax-mode t)
    ;(semantic-load-enable-code-helpers)

    ;(global-srecode-minor-mode 1)
    )

  (message "setting up completion")
  (ac-enable-semantic)        ; enable semantic mode
  (ac-enable-popups)          ; activate auto-complete-mode popups
  (ac-define-semantic-source) ; create semantic completion source


  (defun ac-semantic-completion ()
    (interactive)
    ;(message "enabling semantic auto completion source")
    ;(add-to-list 'ac-sources 'ac-source-semantic)    ; 'm'
    (add-to-list 'ac-sources 'ac-source-semantic-ng) ; 'n'
    (add-to-list 'ac-sources 'ac-source-yasnippet)
    (add-to-list 'ac-sources 'ac-source-imenu)

    ; TODO: preprocess because of preformance:
    ;(add-to-list 'ac-sources 'ac-source-semantic-raw)

    (global-set-key [M-S-mouse-1] 'semantic-ia-fast-mouse-jump)
    (global-set-key (kbd "M-g d") 'semantic-ia-fast-jump)
    (global-set-key (kbd "M-g f") 'semantic-symref)
    ;(global-set-key (kbd "M-g s") 'semantic-complete-jump)
    (global-set-key (kbd "M-g s") 'semantic-ia-show-doc)

    ;(global-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
    ;(global-set-key (kbd "C-c ?") 'semantic-ia-complete-symbol)
    ;(global-set-key (kbd "C-c >") 'semantic-complete-analyze-inline)
    ;(global-set-key "\C-c=" 'semantic-decoration-include-visit)
    ;
    ;(global-set-key "\C-cs" 'semantic-ia-show-summary)
    ;(global-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
    )

  (defun ac-semantic-c-completion ()
    (ac-semantic-completion)

    ; jump to header/impl
    (local-set-key (kbd "M-g h") 'semantic-analyze-proto-impl-toggle)

    (add-to-list 'ac-sources 'ac-source-gtags)
    )

  (add-hook 'auto-complete-mode-hook 'ac-semantic-completion)

  (add-hook 'lisp-mode-hook 'ac-semantic-completion)
  (add-hook 'scheme-mode-hook 'ac-semantic-completion)
  (add-hook 'emacs-lisp-mode-hook 'ac-semantic-completion)
  (add-hook 'erlang-mode-hook 'ac-semantic-completion)
  (add-hook 'haskell-mode-hook 'ac-semantic-completion)

  (add-hook 'c-mode-common-hook 'ac-semantic-c-completion))




;(when window-system
;  (speedbar t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; fight the whitespace crimes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'whitespace)
(global-whitespace-mode t)
; color-face for: spaces, tabs, lines-tail(too long):
(setq whitespace-style '(face tabs trailing newline indentation space-before-tab space-after-tab space-mark tab-mark newline-mark lines-tail))
; old options: empty

(setq whitespace-display-mappings
      '(
        ;;(space-mark   ?\     [?\u00B7]     [?.])      ; space - centered dot
        (space-mark   ?\xA0  [?\u00A4]     [?_])        ; hard space - currency
        (newline-mark ?\n    [?¬ ?\n]  [?$ ?\n])        ; eol - ¬ symbol
        (tab-mark     ?\t    [?∘ ?\t] [?> ?\t])         ; tab - ∘ symbol
        )
      )
; see whitespace.el

;;; fix whitespace display for autocomplete popups with whitespace-mode
(set-default 'whitespace-mode-prev-status nil)

(defadvice popup-draw (before ac-popup-draw-whitespace-mode-off)
           "Turn off whitespace mode before showing autocomplete box"
           (make-local-variable 'whitespace-mode-prev-status)
           (if whitespace-mode
             (progn
               (setq whitespace-mode-prev-status t)
               (whitespace-mode nil))
             ;else
             (setq whitespace-mode-prev-status nil)))

(defadvice popup-delete (after ac-popup-delete-whitespace-mode-on)
           "Restore previous whitespace mode when deleting autocomplete box"
           (make-local-variable 'whitespace-mode-prev-status)
           (if whitespace-mode-prev-status
             (whitespace-mode t)))

(ad-activate 'popup-draw)
(ad-activate 'popup-delete)
;;; done fixing popup

; don't disable the window on pressing C-z
(defadvice iconify-or-deiconify-frame (around disable-xframe-suspending))
(ad-activate 'iconify-or-deiconify-frame)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; function helpers
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun toggle-indent-mode ()
  "toggle tab/space indentation"
  (interactive)
  (if indent-tabs-mode
    (progn
      (message "using space indentation...")
      (setq indent-tabs-mode nil))
    ;else
    (progn
      (message "using tab indentation...")
      (setq indent-tabs-mode t))))

(defun run-command ()
  "Run a shell command."
  (interactive)
  (let ((command (read-string "Command: ")))
    (shell-command
      (concat command " &")
      (concat "*" command "*")
      )
    )
  )

(defun mpv ()
  "play media file"
  (interactive)
  (shell-command
    (concat
      "mpv "
      (shell-quote-argument
        (expand-file-name
          (read-file-name "Filename: ")))
      ;" & "
      )))


(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (point) 'read-face-name) (get-char-property (point) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun inc-number (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d") answer)))))))

(defun dec-number (&optional arg)
  (interactive "p*")
  (inc-number (if arg (- arg) -1)))

(defun inc-number-hex (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer hex-format)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789abcdefABCDEF")
        (when (re-search-forward "[0-9a-fA-F]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 16) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 16 field-width) answer)))
          (if (equal (match-string 0) (upcase (match-string 0)))
            (setq hex-format "X")
            (setq hex-format "x"))
          (replace-match
            (format (concat "%0" (int-to-string field-width)
                            hex-format)
                    answer)))))))

(defun my-format-bin (val width)
  "Convert a number to a binary string."
  (let (result)
    (while (> width 0)
           (if (equal (mod val 2) 1)
             (setq result (concat "1" result))
             (setq result (concat "0" result)))
           (setq val (/ val 2))
           (setq width (1- width)))
    result))

(defun inc-number-binary (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "01")
        (when (re-search-forward "[0-1]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 2) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 2 field-width) answer)))
          (replace-match (my-format-bin answer field-width)))))))


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



;remember last position in file
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file "~/.emacs.d/line-saved-places")

(defun insert-tab ()
  (interactive)
  (insert-char ?\t))

(defun nowrap-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))

(defun date (&optional insert)
  "Display the current date and time. With a prefix arg, INSERT it into the buffer."
  (interactive "P")
  (funcall (if insert 'insert 'message)
           ; %b = month as string
           (format-time-string "%a, %Y-%m-%d %T %Z" (current-time))
           )
  )

(defun call-pop-kill-ring (func)
  (funcall func)
  (setq kill-ring (cdr kill-ring))
  )

(defun kill-word-no-kill-ring ()
  (interactive)
  (call-pop-kill-ring (lambda () (kill-word 1)))
  )

(defun backward-kill-word-no-kill-ring ()
  (interactive)
  (call-pop-kill-ring (lambda () (backward-kill-word 1)))
  )

(defun smooth-scroll (number-lines linedelta &optional waittime)
  (interactive "nIteration count? \nnLine increment each time? ")
  (if (equal waittime nil)
    (message "waitttime undefined!"))
  (if (= 0 number-lines) t
    (progn
      (sit-for 0.02)
      (scroll-up linedelta)
      (smooth-scroll (- number-lines 1) linedelta))))

;(defun highlight-todos (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
;(add-hook 'prog-mode-hook 'highlight-todos)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; set kaschtomaisd key bindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun jj-keybindings ()

  ;arrow key stuff
  (global-set-key (kbd "M-<left>")  'windmove-left)
  (global-set-key (kbd "M-<right>") 'windmove-right)
  (global-set-key (kbd "M-<up>")    'windmove-up)
  (global-set-key (kbd "M-<down>")  'windmove-down)

  (global-set-key (kbd "C-<left>")  'backward-word)
  (global-set-key (kbd "C-<right>") 'forward-word)
  (global-set-key (kbd "C-<up>")    'backward-paragraph)
  (global-set-key (kbd "C-<down>")  'forward-paragraph)

  ;jlk; stuff
  (global-set-key (kbd "M-j") 'backward-char)
  (global-set-key (kbd "M-;") 'forward-char)
  (global-set-key (kbd "M-l") 'previous-line)
  (global-set-key (kbd "M-k") 'next-line)

  (global-set-key (kbd "C-M-j") 'backward-word)
  (global-set-key (kbd "C-M-;") 'forward-word)
  (global-set-key (kbd "C-M-l") 'backward-paragraph)
  (global-set-key (kbd "C-M-k") 'forward-paragraph)

  (global-set-key (kbd "C-k") 'kill-line)
  (global-set-key (kbd "C-l") 'recenter-top-bottom)
  (global-set-key (kbd "C-j") 'nowrap-newline-and-indent)

  ;word deletion
  (global-set-key (kbd "C-<delete>")    'kill-word-no-kill-ring)
  (global-set-key (kbd "C-<backspace>") 'backward-kill-word-no-kill-ring)

  ;terminal fu
  (global-set-key (kbd "M-[ d") 'left-word)  ;backward-word
  (global-set-key (kbd "M-[ c") 'right-word) ;forward-word
  (global-set-key (kbd "M-[ a") 'backward-paragraph)
  (global-set-key (kbd "M-[ b") 'forward-paragraph)

  ; insert fakking tab
  (global-set-key (kbd "C-<tab>") 'insert-tab)

  ;(global-set-key (kbd "RET") 'newline) ;newline-and-indent
  ;(global-set-key (kbd "<C-return>") 'newline)
  (global-set-key (kbd "M-a") 'beginning-of-line-text)
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
  (global-set-key (kbd "C-x M-b") 'speedbar) ; buffer selector

  ;align the current region to = or whatever
  (global-set-key (kbd "M-A") 'align-current)

  (global-set-key (kbd "C-c r") 'remember) ;remember-mode
  (global-set-key (kbd "C-c g") 'magit-status) ;git stuff

  (global-set-key (kbd "C-v") 'cua-set-rectangle-mark) ;rectangle-select
  (global-set-key (kbd "M-SPC") 'just-one-space) ;fold space to 1

  (global-set-key (kbd "M-p") (lambda ()
                                (interactive)
                                (join-line -1)))

  ;unset unneeded keys
  (global-unset-key (kbd "C-t")) ; annoying character swapping

  (fset 'yes-or-no-p 'y-or-n-p) ; yes/no answering without <RET>
  )


;; git amend without any prompt
(defun magit-just-amend ()
  (interactive)
  (save-window-excursion
    (magit-with-refresh
      (magit-need-refresh)
      (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))

; highlight current word with custom face
;(run-with-idle-timer secs repeat function)
;(symbol-at-point)
;unhighlight previous
;(highlight-phrase)

;mouse-wheel scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control)))) ; one line at a time
(setq mouse-wheel-progressive-speed 't)             ; accelerate scrolling
(setq mouse-wheel-follow-mouse 't)                  ; scroll window under mouse

;automatic scrolling
(setq scroll-margin 2)                               ; start scrolling n lines before window borders
(setq scroll-conservatively 10)                      ; scroll up to n lines to bring pointer back on screen
(setq scroll-step 0)                                 ; try scrolling n lines when pointer moves out
(setq auto-window-vscroll nil)

(setq scroll-preserve-screen-position t)             ; keep relative column position when scrolling

; push the mouse out of the way when the cursor approaches.
(mouse-avoidance-mode 'cat-and-mouse)

; custom smooth scrolling
;(global-set-key [(mouse-5)] '(lambda () (interactive) (smooth-scroll 6 1)))
;(global-set-key [(mouse-4)] '(lambda () (interactive) (smooth-scroll 6 -1)))

(setq-default visible-bell nil            ; disable window flashing
              ring-bell-function 'ignore) ; and also disable the sound


(setq indicate-empty-lines t
      transient-mark-mode t
      gud-tooltip-mode t
      lazy-highlight-cleanup nil       ; keep search term highlighted
      lazy-highlight-max-at-a-time nil ; all occurences in file
      global-font-lock-mode t ; font coloring
      auto-compression-mode t ; deal with compressed files
      blink-cursor-mode nil ; don't blink the cursor
      mouse-yank-at-point t) ; paste as cursor instead of mouse position

;backup files

(setq
  make-backup-files nil
  backup-by-copying-when-mismatch t
  backup-by-copying-when-linked t
  ;backup-by-copying t
  ;backup-directory-alist
  ;'(("." . "~/.saves"))
  ;delete-old-versions t
  ;kept-new-versions 6
  ;kept-old-versions 2
  ;version-control t
  ;make-backup-files nil ; backup~ files
  ;auto-save-default nil ; #autosave# files
  )


;utf-8 ftw
(prefer-coding-system 'utf-8)

;link to X primary clipboard
(setq x-select-enable-clipboard t)

;require a ending newline
(setq require-final-newline 't) ; 'query will ask

;display various non-editing buffers in their own frames
(setq special-display-buffer-names
      (nconc '("*Backtrace*" "*VC-log*" "*compilation*" "*grep*")
             special-display-buffer-names
             )
      )
;no tool bar for these buffers
(add-to-list 'special-display-frame-alist '(tool-bar-lines . 0))

;don't echo passwords when using interactive terminal programs
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

;man pages with clickable links
(add-hook 'Man-mode-hook 'goto-address)



(require 'ido)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-case-fold t)
(setq ido-use-virtual-buffers t)
;(setq completion-ignored-extensions '(".pdf" ".aux" ".toc" ".tex~"))
;(setq ido-ignore-extensions t)
(setq ido-file-extensions-order '(".c" ".cpp" ".h" ".py" ".tex" ".bib" ".hs"))
(add-hook 'ido-setup-hook (lambda () (
                                      define-key ido-completion-map [tab] 'ido-next-match
                                      )))

(ido-mode t)
(icomplete-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; spellchecking
;
; hunspell support for spell checking
; adds {english,german}-hunspell dictionaries
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'ispell)

(setq-default ispell-program-name "hunspell")
(add-to-list 'ispell-local-dictionary-alist '(
                                              "german-hunspell" "[[:alpha:]]" "[^[:alpha:]]" "[']"
                                              t
                                              ("-d" "de_DE")
                                              nil
                                              utf-8
                                              ))

(add-to-list 'ispell-local-dictionary-alist '(
                                              "english-hunspell" "[[:alpha:]]" "[^[:alpha:]]" "[']"
                                              t
                                              ("-d" "en_US")
                                              nil
                                              utf-8
                                              ))

(setq ispell-dictionary "english-hunspell")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; org-mode stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'org-install)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-return-follows-link t)
(setq org-log-done nil)
(setq org-server "~/org/")
(setq org-tag-alist '(("read" . ?r) ("work" . ?w) ("code" . ?c)))
(setq org-remember-templates '(
                               ("Todo" ?t "* TODO %^{Brief Description} %^g\n%?\nAdded: %U" (car org-agenda-files) "UNFILED")
                               ("Note" ?n "* %^{Brief Description} %^g\n%?\nAdded: %U" org-default-notes-file "UNFILED")
                               ))

(setq org-agenda-files (mapcar (lambda (x) (concat org-server x)) '("org/todo.org")))
(defun todo ()
  (interactive)
  (find-file-existing (car org-agenda-files)))

(setq wiki-entry-point (concat org-server "org/wiki.org"))
(setq org-default-notes-file wiki-entry-point)
(defun wiki ()
  (interactive)
  (find-file-existing wiki-entry-point))


; update org [9/10] markers when deleting lines
(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
           (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
           (myorg-update-parent-cookie))





; preserve undo-region selection
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


; after deleting a tag, indent properly
(defadvice sgml-delete-tag (after reindent activate)
           (indent-region (point-min) (point-max)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; smart tabs, mix tabs and spaces (fak yea)
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
                      (
                       ;set tabwidth to really high value (fill-column)
                       (tab-width fill-column)
                       (,offset fill-column)
                       ;(wstart (window-start))
                       )
                      (unwind-protect
                        (progn ad-do-it)
                        ;(set-window-start (selected-window) wstart)
                        )
                      )
                    )
                  (t ad-do-it)
                  )
                )
     )
  )


;cc-langs
(smart-tabs-advice c-indent-line c-basic-offset)
(smart-tabs-advice c-indent-region c-basic-offset)
;python-mode
(smart-tabs-advice py-indent-line py-indent-offset)
(smart-tabs-advice py-newline-and-indent py-indent-offset)
(smart-tabs-advice py-indent-region py-indent-offset)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; random stuff?
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;automatically newline on certain chars:
;(add-hook 'c-mode-common-hook '(lambda () (c-toggle-auto-state 1)))
;reindent line when typing {,},;,etc..
;(setq-default c-electric-flag nil)
;useful modes:
;auto-newline, hungry-delete, syntactic-indentation
;-> M-x c-toggle[-auto]-{hungry-state,syntactic-indentation}
;(electric-indent-mode t) ;auto-indent
;(electric-pair-mode 1)   ;auto-brackets

;custom key bindings:
;(defun jj-c-initialization-hook ()
;  (define-key c-mode-base-map "\C-m" 'c-context-line-break))
;(add-hook 'c-initialization-hook 'jj-c-initialization-hook)

;offset customizations not in the custom c-style
;-> precedence over any setting of the syntactic symbol made by a style
;(setq c-offsets-alist '((member-init-intro . ++)))

;(when (load "flymake" t)
; (defun flymake-pylint-init ()
;   (interactive)
;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;     'flymake-create-temp-inplace))
;     (local-file (file-relative-name
;       temp-file
;       (file-name-directory buffer-file-name))))
;     (list "epylint" (list local-file))))

;(add-to-list 'flymake-allowed-file-name-masks
; '("\\.py\\'" flymake-pylint-init)))

; reload the emacs config
(defun reload-config ()
  (interactive)
  (load-file "~/.emacs")
  )

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset))
         )
    (* (max steps 1)
       c-basic-offset
       )
    )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; coding style definitions
; ------------------------
;
; see all the possible variables at [emacsshare]/lisp/progmodes/cc-vars.el
; c-set-stylevar-fallback 'c-offsets-alist
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; codestyle defaults, will be overwritten
(setq-default indent-tabs-mode t)
(setq-default indent-line-function 'insert-tab)
(setq-default default-tab-width 4)
(setq-default whitespace-line-column 400)



; linux kernel indentation style
(defconst kernel-c-style
          '("linux" (c-offsets-alist (
                                      arglist-cont-nonempty
                                      c-lineup-gcc-asm-reg
                                      c-lineup-arglist-tabs-only
                                      )))
          )

; sft coding style
(defconst sft-c-style
          '("linux" ; base it on linux code style
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
                                       (substatement-open before after) ;
                                       ))
            (c-cleanup-list . (brace-else-brace))
            (c-offsets-alist . (
                                ; indent helper funcs: c-lineup-arglist{-tabs-only}
                                ; absolute offset: [0]
                                (access-label          . -)   ; public: or private:
                                (arglist-intro         . +)   ; first arg in newline
                                (arglist-cont          . 0)   ; wrapped function args after func(
                                (arglist-cont-nonempty . c-lineup-arglist)   ; wrapped function args after func(arg
                                (arglist-close         . 0)   ; intentation of ) which closes args
                                (block-open            . 0)   ; { to open a block
                                (block-close           . 0)   ; } after a block
                                (case-label            . 0)   ; case 1337:
                                (statement-case-open   . 0)   ; { after case 1337:
                                (statement-case-intro  . +)   ; code after case 1337:
                                (defun-block-intro     . +)   ; beginning of keyword (...) { stuff  }
                                (inclass               . +)   ; members of struct or class
                                (inline-open           . +)
                                (innamespace           . 0)   ; members of a namespace
                                (knr-argdecl-intro     . -)
                                (knr-argdecl-intro     . 0)
                                (label                 . 0)   ; gotolabel:
                                (statement             . 0)
                                (statement-block-intro . +)   ; first line of a block
                                (statement-case-open   . +)
                                (statement-cont        . 0)
                                (substatement          . +)
                                (substatement-label    . 0)
                                (substatement-open     . 0)
                                (substatement-open     . 0)
                                (topmost-intro         . 0)   ; indentation of file start
                                (topmost-intro-cont    . 0)
                                (cpp-macro             . 0)   ; #define, etcetc
                                (member-init-intro     . +)   ; member initializing for class lol : var(val)
                                (member-init-cont      . 0)   ; further members
                                ))

            ;information about indent parsing on TAB
            ;this is also triggered by C-c C-s
            (c-echo-syntactic-information-p . nil))
          "The SFT C programming style"
          )
;;; end coding style definitions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; hooks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-hook 'LaTeX-mode-hook (lambda ()
                             (setq TeX-PDF-mode t
                                   TeX-auto-save t
                                   TeX-parse-self t
                                   reftex-plug-into-AUCTeX t)
                             (setq-default TeX-master nil) ; query for master file
                             (visual-line-mode t)
                             (LaTeX-math-mode t)
                             (turn-on-reftex)
                             (turn-off-auto-fill)
                             ))

(add-hook 'server-visit-hook (lambda ()
                               (prefer-coding-system 'utf-8)
                               (setq locale-coding-system 'utf-8)
                               (set-terminal-coding-system 'utf-8)
                               (set-keyboard-coding-system 'utf-8)
                               (set-selection-coding-system 'utf-8)
                               ))

; correct zsh coloring in shell:
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


; press ~ for reaching home directly in ido-mode
(add-hook 'ido-setup-hook
          (lambda ()
            (define-key ido-file-completion-map
                        (kbd "~")
                        (lambda ()
                          (interactive)
                          (if (looking-back "/")
                            (insert "~/")
                            (call-interactively 'self-insert-command))))))

; amend a commit
(add-hook 'magit-mode-hook (lambda ()
                             (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)))




; default c-coding-style
(setq-default c-default-style "linux" c-basic-offset 4)

; hook for all c-like languages
(defun jj-cstyle-hook ()
  (c-add-style "sftstyle"     sft-c-style)
  (c-add-style "linux-kernel" kernel-c-style)

  (message "setting C coding style...")

  ; default to sft style
  (c-set-style "sftstyle")

  (when ; kernel code style
    (and buffer-file-name
         (string-match
           (expand-file-name "/usr/src/linux-git") buffer-file-name))
    (setq indent-tabs-mode t)
    (c-set-style "linux-kernel"))

  (setq tab-width 4
        indent-tabs-mode t)
  (c-toggle-auto-newline nil) ; no automatic
  (c-toggle-auto-state nil)   ; newlines
  )

; main coding configuration function
(defun jj-coding-hook ()
  (local-set-key (kbd "RET") 'newline-and-indent)
  (jj-keybindings)
  (ruler-mode t)
  (auto-revert-mode t)
  (eldoc-mode t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; special language-specific hooks

; c, c++
(defun jj-c-coding-hook ()
  (jj-cstyle-hook)
  (jj-coding-hook))

; py
(defun jj-python-coding-hook ()
  (setq python-indent 4)
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (setq-default whitespace-line-column 79)
  (jj-coding-hook))

; elisp
(defun jj-lisp-coding-hook ()
  (setq indent-tabs-mode nil)
  (setq tab-width 8)
  ;(setq lisp-indent-offset 4)
  ;(setq lisp-body-indent 4)
  (jj-coding-hook))

; javascript / ecmascript
(defun jj-javascript-coding-hook ()
  (jj-coding-hook))

; TeX
(defun jj-latex-coding-hook ()
  ; set latex indent offset so it doesn't fuck up
  ; (i.e. use values != n*tab-width)
  (setq tab-width 4)
  (setq LaTeX-indent-level 4)
  (setq LaTeX-item-indent -4)
  (setq indent-tabs-mode nil)
  (jj-coding-hook))

; html
(defun jj-html-coding-hook ()
  (setq sgml-basic-offset 4)
  (setq indent-tabs-mode t))

; haskell
(defun jj-haskell-coding-hook ()
  ; haskell interpreter: C-c C-z or C-c C-l
  (turn-on-haskell-indentation)
  (turn-on-haskell-doc-mode))


(add-hook 'python-mode-hook     'jj-python-coding-hook)
(add-hook 'lisp-mode-hook       'jj-lisp-coding-hook)
(add-hook 'javascript-mode-hook 'jj-javascript-coding-hook)
(add-hook 'html-mode-hook       'jj-html-coding-hook)
(add-hook 'haskell-mode-hook    'jj-haskell-coding-hook)
(add-hook 'c-mode-common-hook   'jj-c-coding-hook)

; enable commands that may "confuse" the user
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)

(defun window-setup ()
  (message "running in windowed mode")
  (global-linum-mode)
  (global-hl-line-mode))

(defun terminal-setup ()
  (message "running in terminal mode")
  (custom-set-faces
    '(default ((t (:background "#000000"))))
    '(semantic-highlight-func-current-tag-face ((t (:background "gray15"))))))

;switch for X and console window
(if window-system
  (window-setup)
  (terminal-setup))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; custom fileextension -> mode assignments
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; auctex
(autoload 'latex-mode "auctex" "Major mode for editing LaTeX files" t)
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

; markdown text mode
(autoload 'markdown-mode "markdown-mode" "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

; cmake config mode
(autoload 'cmake-mode "cmake-mode" "Major Mode for cmake configuration editing" t)
(add-to-list 'load-path (expand-file-name "/usr/share/cmake/editors/emacs/"))
(add-to-list 'auto-mode-alist '("CMakeLists\\.txt\\'" . cmake-mode))
(add-to-list 'auto-mode-alist '("\\.cmake\\'" . cmake-mode))

; use c++ for header files, does no harm to c-headers.
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

; opengl shaders
(add-to-list 'auto-mode-alist '("\\.glsl\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.vert\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.frag\\'" . c++-mode))

; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; random init stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun emacs-reloaded ()
  (animate-string
    (concat ";; Welcome to "
            (substring (emacs-version) 0 16)
            "!\n"
            ";;  ___  ______ _______ _______ ___ _______          _     \n"
            ";; |  _)/ _____|_______|_______|_  (_______)        | |    \n"
            ";; | | ( (____  _____      _     | |   _ _____  ____| |__  \n"
            ";; | |  \\____ \\|  ___)    | |    | |  | | ___ |/ ___)  _ \\ \n"
            ";; | |_ _____) ) |        | |   _| |  | | ____( (___| | | |\n"
            ";; |___|______/|_|        |_|  (___|  |_|_____)\\____)_| |_|\n"
            "\n\n"
            )
    5 nil
    )
  )

(defun try-kill-buffer (name)
  (interactive "BBuffer name? ")

  (setq buffer-names (
                      mapcar (function buffer-name) (buffer-list)
                      ))

  ;(message (format "buffer names: %s" buffer-names))

  (setq is-member (member name buffer-names))

  (if is-member
    (kill-buffer name)
    (message (format "Buffer %s not found!" name))))

;;TODO: the var name is wrong..
(defun print-variable (var)
  (interactive "vVariable name? ")

  (cond
    ; is the var a list?
    ((listp var)
     (format "List contents: len=%d" (length var))
     (let (value (i 0))
       (dolist (elt var)
         (format "%d: %s" i elt)
         (setq i (i+1))
         )
       )
     )
    (t
      (format "%s" var)
      )
    )
  )


(defun jj-emacs-init ()
  (try-kill-buffer "*scratch*")
  ;(try-kill-buffer "*CEDET Global*")

  ;(emacs-reloaded)
  )

(add-hook 'after-init-hook 'jj-emacs-init)
