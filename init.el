;;; init.el -- My Emacs configuration

;;; Commentary:
;; As of right now, common configurations are in ./modules
;; The code begins with setting up Emacs with straight.el and use-package.
;; It then loads my custom modules, and ends with misc things.

;;; Code:

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Don't build built-in packages that come with Emacs 30
(setq straight-built-in-pseudo-packages
      (append straight-built-in-pseudo-packages
              '(project xref eldoc flymake jsonrpc eglot)))

;; Install use-package via straight
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t)

;; Allow navigation between use-package stanzas with imenu.
(defvar use-package-enable-imenu-support t)
(require 'use-package)
(setq use-package-verbose t)

;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Keychain stuff. Note to self: if you keep having to enter your
;; keychain password on OS X, make sure that you have the following in .ssh/config:
;; Host *
;;    UseKeychain yes
(use-package keychain-environment
  :config
  (keychain-refresh-environment))

;; Ensure GNU ELPA has the GPG keys it needs
(use-package gnu-elpa-keyring-update)

(ignore-errors
    (set-frame-font "Hack Nerd Font Mono 20"))

;; Make font a bit larger
(set-face-attribute 'default nil :height 150)

;; Loading before nearly anything so than any package is diminishable and the modeline doesn't get fucked
(use-package diminish
  :ensure t
  :config
  (diminish 'eldoc-mode))

;; Custom modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'my-configs)
(require 'my-evil)
(require 'my-gui)
(require 'my-editing)
(require 'my-python)
(require 'my-javascript)
(require 'my-haskell)
(require 'my-docker)
(require 'my-go)
(require 'my-cpp)
(require 'my-prolog)
(require 'my-rust)
(require 'my-functions)
(require 'my-sql)
(require 'my-copilot)

;;; MISC things
;; I do all of my writing in either org-mode or markdown-mode.
(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown -t html")))

(use-package dumb-jump)
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(setq xref-show-definitions-function #'xref-show-definitions-completing-read)
(use-package ivy-xref
  :ensure t
  :init
  ;; xref initialization is different in Emacs 27 - there are two different
  ;; variables which can be set rather than just one
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  ;; Necessary in Emacs <27. In Emacs 27 it will affect all xref-based
  ;; commands other than xref-find-definitions (e.g. project-find-regexp)
  ;; as well
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

(provide 'init)
;;; init.el ends here
