;;; my-evil -- Code for evil configuration

;;; Commentary:

;;; Code:
;; Evil configuration
(use-package evil
  :ensure t
  :init
  (setq evil-want-keybinding nil)
  (progn
    (setq evil-default-cursor t)
    (use-package evil-leader
		 :ensure t
		 :init (global-evil-leader-mode)
		 (evil-leader/set-leader "<SPC>")
                 (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
		 :config
		 (progn
		   (setq evil-leader/in-all-states t)
		   (evil-leader/set-key
		    "<SPC>" 'ace-window

		    ;; code commands
		    "c d" 'lsp-find-definition
		    ";" 'comment-line

		    ;; projectile commands
		    "p r" 'deadgrep
		    "p f" 'projectile-find-file
		    "p p" 'projectile-switch-project

		    ;; magit shortcuts
		    "g s" 'magit-status
		    "g c" 'magit-checkout
		    "g b" 'magit-blame

		    ;; file operations
		    "f w" 'save-buffer
		    "f f" 'ido-find-file
		    "f s" 'swiper
		    "f S" 'swiper-at-point
		    "f c" 'copy-file-name-to-clipboard

		    ;; buffer operations
		    "b" 'ivy-switch-buffer
		    "k" 'kill-buffer
		    "K" 'kill-this-buffer

		    ;; shell
		    "T" 'shell

                    ;; errors
                    "e f" 'flycheck-first-error
                    "e n" 'flycheck-next-error
                    "e p" 'flycheck-previous-error

		    ;; window commands
		    "w d" 'delete-window
		    "w n" 'split-right-and-enter
		    "w 1" 'delete-other-windows

		    ))
		    (use-package evil-magit
		    :ensure t)
		    (modify-syntax-entry ?_ "w")

                    (use-package evil-snipe
                      :after evil
                      :config
                      (setq evil-snipe-scope 'buffer)
                      (evil-snipe-mode +1))

		    (use-package evil-surround
		    :ensure t
		    :config
		    (global-evil-surround-mode 1))

		    (use-package evil-org
		    :ensure t)
		    ;; boot evil by default

                    (use-package evil-collection
                    :after evil
                    :ensure t
                    :config
                    (evil-collection-init))

		    (evil-mode 1)))
  :config
  (progn
    ;; escape key should always escacpe
    (define-key evil-normal-state-map [escape] 'keyboard-quit)
    (define-key evil-visual-state-map [escape] 'keyboard-quit)
    (define-key minibuffer-local-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-ns-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-completion-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-must-match-map [escape] 'abort-recursive-edit)
    (define-key minibuffer-local-isearch-map [escape] 'abort-recursive-edit)

    ;; modes to map to diifferent default states
    (dolist (mode-map '((comint-mode . emacs)
                        (dashboard-mode . emacs)
			(term-mode . emacs)
			(help-mode . emacs)
			(fundamental-mode . emacs)))
      (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))
    ))
(global-undo-tree-mode)
(evil-set-undo-system 'undo-tree)
(provide 'my-evil)
;;; my-evil.el ends here
