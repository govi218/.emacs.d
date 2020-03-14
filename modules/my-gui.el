;;; my-gui -- Code for gui configuration

;;; Commentary:

;;; Code:

;; Fullscreen by default, as early as possible.
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; UTF-8 everywhere, please.
(prefer-coding-system 'utf-8)

;; Disable otiose GUI settings: they just waste space.
;; fringe-mode is especially ruinous performance-wise.
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (tooltip-mode -1)
  (fringe-mode -1))

;; The Doom Emacs themes look really good.
(use-package doom-themes
  :config
  (load-theme 'doom-vibrant)
  (doom-themes-visual-bell-config)
  (doom-themes-org-config))

;; Ivy makes most minibuffer prompts sortable and filterable. I used
;; to use helm, but it was too slow. Unfortunately org-ref depends on
;; it, but I never load it, so we good.

(use-package ivy
  :ensure t
  :init
  (ivy-mode 1)
  (unbind-key "S-SPC" ivy-minibuffer-map)
  (setq ivy-height 15
        ivy-use-virtual-buffers t
        ivy-use-selectable-prompt t)
  (defun swiper-at-point ()
    (interactive)
    (swiper (thing-at-point 'word)))
  :bind (("C-x b"   . ivy-switch-buffer)
         ("C-c C-r" . ivy-resume)
         ("C-c s"   . swiper-at-point)
         ("C-s"     . swiper))
  :diminish)

;; ivy-rich makes Ivy look a little bit more like Helm.
(use-package ivy-rich
  :after counsel
  :custom
  (ivy-virtual-abbreviate 'full
   ivy-rich-switch-buffer-align-virtual-buffer t
   ivy-rich-path-style 'abbrev)
  :init
  (ivy-rich-mode))

;; Provides visual interface to hydra layouts. I don't really
;; use hydras anywhere yet but some packages do.
(use-package ivy-hydra)

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

;; fish is a good shell. You should try it.
(use-package fish-mode)

;; Counsel applies Ivy-like behavior to other builtin features of
;; emacs, e.g. search.
(use-package counsel
  :ensure t
  :after ivy
  :init
  (counsel-mode 1)

  :bind (("C-c ;" . counsel-M-x)
         ("C-c U" . counsel-unicode-char)
         ("C-c i" . counsel-imenu)
         ("C-x f" . counsel-find-file)
         ("C-c y" . counsel-yank-pop)
	 ("C-c r" . counsel-recentf)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history))
  :diminish)

;; Deadgrep is amazing.
(use-package deadgrep
  :bind (("C-c h" . deadgrep)))

;; projectile comes with Emacs these days, but we want to enable
;; caching, since I work on big projects.
(use-package projectile
  :bind (("C-c f" . projectile-find-file))
  :config
  (setq projectile-enable-caching t
        projectile-completion-system 'ivy)
  :diminish)

;; Counsel and projectile should work together.
(use-package counsel-projectile
  :bind (("C-c f" . counsel-projectile))
  :init
  (counsel-projectile-mode))

;; Sort commands by recency in ivy windows.
(use-package smex)

(defun open-init-file ()
  "Open this very file."
  (interactive)
  (find-file user-init-file))

(defun kill-all-buffers ()
  "Close all buffers."
  (interactive)
  (maybe-unset-buffer-modified)
  (save-some-buffers)
  (mapc 'kill-buffer-with-prejudice (buffer-list)))

(defun split-right-and-enter ()
  "Split the window to the right and enter it."
  (interactive)
  (split-window-right)
  (other-window 1))

;; I'm not made of time, I can't type "yes" for every choice
(defalias 'yes-or-no-p 'y-or-n-p)
(global-hl-line-mode t)              ; Always highlight the current line.
(show-paren-mode t)                  ; And point out matching parentheses.
(delete-selection-mode t)            ; Behave like any other sensible text editor would.
(global-display-line-numbers-mode)   ; Emacs has this builtin now, it's fast
(save-place-mode)                    ; Remember where I was

;; Make sure that ligatures from fonts that offer them are enabled.
;; This isn't present on GNU Emacs and requires a tremendous amount
;; of munging of 'prettify-symbols-alist'.
(ignore-errors (mac-auto-operator-composition-mode))

(setq
  compilation-always-kill t              ; Never prompt to kill a compilation session.
  compilation-scroll-output 'first-error ; Always scroll to the bottom.
  make-backup-files nil                  ; No backups, thanks.
  auto-save-default nil                  ; Or autosaves. What's the difference between autosaves and backups?
  create-lockfiles nil                   ; Emacs sure loves to put lockfiles everywhere.
  inhibit-startup-screen t               ; No need to see GNU agitprop.
  kill-whole-line t                      ; Lets C-k delete the whole line
  mac-command-modifier 'super            ; I'm not sure this is the right toggle, but whatever.
  require-final-newline t                ; Auto-insert trailing newlines.
  ring-bell-function 'ignore             ; Do not ding. Ever.
  use-dialog-box nil                     ; Dialogues always go in the modeline.
  frame-title-format "emacs – %b"        ; Put something useful in the status bar.
  initial-scratch-message nil            ; SHUT UP SHUT UP SHUT UP
  mac-option-modifier 'meta              ; why isn't this the default
  save-interprogram-paste-before-kill t  ; preserve paste to system ring
  enable-recursive-minibuffers t         ; don't fucking freak out if I use the minibuffer twice
  sentence-end-double-space nil          ; are you fucking kidding me with this shit
  confirm-kill-processes nil             ; don't whine at me when I'm quitting.
  mac-mouse-wheel-smooth-scroll nil      ; no smooth scrolling
  mac-drawing-use-gcd t                  ; and you can do it on other frames
  mark-even-if-inactive nil              ; prevent really unintuitive undo behavior
  electric-pair-mode 1                   ; match parens
  )

;; dired whines at you on macOS unless you do this.
(when (eq system-type 'darwin)
  (setq dired-use-ls-dired nil))

(setq-default
  cursor-type 'bar
  indent-tabs-mode nil
  cursor-in-non-selected-windows nil)

(set-fill-column 95)

;; Always trim trailing whitespace.
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defun transparency (value)
   "Set the transparency of the frame window given a VALUE, 0=transparent/100=opaque."
   (interactive "nTransparency value 0 - 100 opaque:")
   (set-frame-parameter (selected-frame) 'alpha value))
(transparency 92)

(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-startup-banner 'logo
        dashboard-center-content t
        dashboard-banner-logo-title "Be careful"
        dashboard-items '((recents  . 5)
                        (projects . 5))
        dashboard-set-footer nil))

(use-package all-the-icons
  :ensure t)

(use-package page-break-lines
  :ensure t
  :config
  (page-break-lines-mode))

(use-package drag-stuff
  :ensure t
  :bind* (("C-M-p" . drag-stuff-up)
          ("C-M-n" . drag-stuff-down)))

(use-package bind-key
  :ensure t
  :config
  (bind-key "M-j" (lambda () (interactive) (join-line -1))))

(use-package ace-window
  :ensure t
  :diminish ace-window-mode
  :bind
  ("M-o" . ace-window)
  :config
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :weight 'bold
                      :height 2.0)
  (set-face-attribute 'aw-mode-line-face nil
                      :inherit 'mode-line-buffer-id
                      :foreground "lawn green")
  (setq aw-scope 'frame)
  (setq aw-dispatch-always t)
  (setq aw-keys '(?q ?w ?e ?r ?a ?s ?d ?f))
  (setq aw-dispatch-alist '((?c aw-swap-window "Ace - Swap Window")
                            (?n aw-flip-window)))
  (ace-window-display-mode t))


;; ZOOM
(use-package zoom
  :ensure t
  :config
  (zoom-mode t))

;;; OS specific config
(defconst *is-a-mac* (eq system-type 'darwin))
(defconst *is-a-linux* (eq system-type 'gnu/linux))

(when *is-a-mac*
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'none)
  (setq default-input-method "MacOSX"))

(when *is-a-linux*
  (setq x-super-keysym 'meta))		; Set Super key as Meta

;; Enable a better look and feel when on mac os by adding natural
;; title bar
(when *is-a-mac*
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark)))


(setq create-lockfiles nil)

;; Magit is one of the best pieces of OSS I have ever used. It is truly esssential.
(defun maybe-unset-buffer-modified (&optional _)
  "Clear modified bit on all unmodified buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and buffer-file-name (buffer-modified-p) (current-buffer-matches-file-p))
        (set-buffer-modified-p nil)))))


(use-package magit
  :bind (("C-c g" . magit-status))
  :diminish magit-auto-revert-mode
  :diminish auto-revert-mode
  :custom
  (magit-remote-set-if-missing t)
  (magit-diff-refine-hunk t)
  :config
  (magit-auto-revert-mode t)

  ;; Magit, and Emacs in general, has a nasty habit of prompting to save buffers
  ;; that are identical to those on disk. This is an attempt at remedying that,
  ;; one that I should probably attach to other functions like save-buffers-kill-emacs.
  (advice-add 'magit-refresh :before #'maybe-unset-buffer-modified)
  (advice-add 'magit-commit  :before #'maybe-unset-buffer-modified)
  (setq magit-completing-read-function 'ivy-completing-read)
  (add-to-list 'magit-no-confirm 'stage-all-changes))

(use-package diff-hl
  :ensure t
  :demand t
  :config
    (diff-hl-flydiff-mode +1)
    (global-diff-hl-mode +1)
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode-unless-remote))


(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package smartparens
  :ensure t
  :diminish smartparens-mode
  :init
  (smartparens-global-mode t)
  (smartparens-strict-mode t)
  (show-smartparens-global-mode t)
  (require 'smartparens-config))


;; The beauty of undo-tree is that it means that, once you've typed something into a buffer,
;; you'll always be able to get it back. At least in theory. undo-tree has long-standing data
;; loss bugs that are unlikely to be fixed. But no other package provodes a comparable experience.

(use-package undo-tree
  :config
  (global-undo-tree-mode +1)
  :diminish)

;; Recentf comes with Emacs but it should always be enabled.
(use-package recentf
  :init (recentf-mode t)
  :config
  (add-to-list 'recentf-exclude "\\.emacs.d")
  (add-to-list 'recentf-exclude ".+tmp......\\.org"))

;; Keychain stuff. Note to self: if you keep having to enter your
;; keychain password on OS X, make sure that you have the following in .ssh/config:
;; Host *
;;    UseKeychain yes
(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; Ensure GNU ELPA has the GPG keys it needs
(use-package gnu-elpa-keyring-update)

;; Haven't figured out how to diminish eldoc-mode outside of
;; requiring this explicitly and doing it manually.
(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode))

;; Always prefer newer files.
(setq load-prefer-newer t)

(provide 'my-gui)
;;; my-gui.el ends here
