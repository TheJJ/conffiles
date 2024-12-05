;;; settings.el -*- lexical-binding: t; -*-

(defun jj/defaults ()
  (delete-selection-mode t)  ;; replace selection with typed text
  (context-menu-mode t)      ;; rightclick menu instead of useless region selection
  (breadcrumb-mode t)        ;; display current navigation path in headerline

  ;; don't push the mouse away, instead just hide it when typing
  (setq mouse-avoidance-mode nil
        make-pointer-invisible t)

  (global-auto-revert-mode t)
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)

  (setq-default visible-bell nil            ; disable window flashing
                ring-bell-function 'ignore) ; and also disable the sound

  (setq
    auto-compression-mode t          ; deal with compressed files
    backward-delete-char-untabify-method nil
    custom-unlispify-tag-names nil   ; view variable names in custom-mode
    desktop-restore-eager 3          ; other buffers are restored lazily
    display-line-numbers-type t      ; line number format
    history-delete-duplicates t      ; helm history duplicate removal
    indicate-empty-lines t
    isearch-allow-scroll t           ; continue the search even though we're scrolling
    isearch-wrap-function '(lambda nil)   ; no overwrapping in search
    lazy-highlight t                 ; highlight occurrences
    lazy-highlight-cleanup nil       ; keep search term highlighted
    lazy-highlight-max-at-a-time nil ; all occurences in file
    load-prefer-newer t              ; don't load older .elc files than .el
    mime-edit-split-message nil      ; don't split large messages
    mouse-yank-at-point t            ; paste as cursor instead of mouse position
    native-comp-async-report-warnings-errors 'silent
    password-cache-expiry nil        ; tramp password cache
    python-fill-docstring-style 'symmetric
    python-shell-prompt-detect-failure-warning nil
    ranger-show-literal t            ; colored ranger previews
    recentf-max-saved-items 1000
    tab-always-indent t
    transient-mark-mode t
    ;; when buffers have the same filenames, uniquify them
    ;; by parts of the directory name instead of the annoying <2>,<3>,.. etc
    uniquify-buffer-name-style 'post-forward
    ;; don't rename special buffers
    uniquify-ignore-buffers-re "^ ?\\*"
    evil-want-fine-undo t            ; more undo steps, e.g. within one insert sequence
    evil-search-wrap nil             ; like vim nowrapscan
    evil-want-C-u-scroll nil         ; don't shadow prefix argument
    evil-want-C-u-delete nil         ; same

    +default-want-RET-continue-comments nil ;; no magic doom comment continuation
  ))

(after! savehist
  ;; don't persist clipboard accross sessions
  (delq! 'kill-ring savehist-additional-variables))

(after! mailcap
  (add-to-list 'mailcap-user-mime-data
               '("pdf"
                 (viewer . "xdg-open %s")
                 (type . "application/pdf")
                 (test . window-system)))
  (add-to-list 'mailcap-user-mime-data
               '("html"
                 (viewer . "xdg-open %s")
                 (type . "text/html")
                 (test . window-system))))

(after! helm
  (setq
   helm-adaptive-history-file (locate-user-emacs-file ".cache/helm-adaptive-history")
   helm-adaptive-history-length 200
   helm-ff-file-name-history-use-recentf nil  ; don't use recentf for helm find files
   helm-ff-initial-sort-method 'ext))         ; sort by extension, advised for priority and name sorting

(after! vterm
  (setq vterm-min-window-width 20))

(after! breadcrumb
  ;; default is breadcrumb--header-line which combines project+imenu
  ;; only show the in-file part from imenu.
  ;; breadcrumb-local-mode hardcodes this to set header-line-format on activation,
  ;; so we have to replace the hardcoded function...
  (defadvice! jj/breadcrumb-default-format ()
    :override #'breadcrumb--header-line
    (breadcrumb-imenu-crumbs))
  (setq breadcrumb-imenu-max-length 1.0))  ; path in file, 100% window width

(after! tramp
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1

  ;; for better performance
  (setq tramp-verbose 0)
  (setq tramp-histfile-override t)

  ;; control ssh muxing in .ssh/config instead
  (setq tramp-use-ssh-controlmaster-options nil))
