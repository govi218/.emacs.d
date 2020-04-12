;;; init.el -- My Emacs configuration


;;; Commentary:
;; As of right now, common configurations are in ./modules
;; The code begins with setting up Emacs with use package and other necessary
;; initialization settings. It then loads my custom modules, and ends with misc
;; things that I can't find a good enough name to group together as


;;; Code:
(require 'package)

(defmacro append-to-list (target suffix)
  "Append SUFFIX to TARGET in place."
  `(setq ,target (append ,target ,suffix)))

(append-to-list package-archives
                '(("melpa" . "http://melpa.org/packages/")
                  ("melpa-stable" . "http://stable.melpa.org/packages/")
                  ("org-elpa" . "https://orgmode.org/elpa/")))

(package-initialize)

(unless (package-installed-p 'quelpa)
    (with-temp-buffer
      (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/quelpa.el")
      (eval-buffer)
      (quelpa-self-upgrade)))

;; Ensure use-package is present. From here on out, all packages are loaded
;; with use-package.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Allow navigation between use-package stanzas with imenu.
;; This has to be set before loading use-package.
(defvar use-package-enable-imenu-support t)
(require 'use-package)
(setq
 use-package-always-ensure t
 use-package-verbose t)

;; Any Customize-based settings should live in custom.el, not here.
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; Custom modules
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))
(require 'my-gui)
(require 'my-evil)
(require 'my-editing)
(require 'my-python)
(require 'my-javascript)
(require 'my-haskell)
(require 'my-docker)
(require 'my-go)

;;; MISC things
;; I do all of my writing in either org-mode or markdown-mode.
(use-package markdown-mode
  :mode ("\\.md$" . gfm-mode)
  :config
  (when (executable-find "pandoc")
    (setq markdown-command "pandoc -f markdown -t html")))

(use-package yaml-mode)

(use-package sqlformat)

(defun copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(provide 'init)
;;; init.el ends here
