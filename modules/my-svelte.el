;;; my-svelte.el --- Svelte support with LSP
;;; Code:
;; Ensure lsp-mode is available
(require 'lsp-mode nil t)
;; Use web-mode for Svelte files
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . web-mode))
;; Configure web-mode for Svelte
(with-eval-after-load 'web-mode
  (add-to-list 'web-mode-engines-alist '("svelte" . "\\.svelte\\'")))
;; LSP for Svelte files
(add-hook 'web-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (string-equal "svelte" (file-name-extension buffer-file-name)))
              (lsp-deferred)
              (add-hook 'before-save-hook #'lsp-format-buffer nil t))))

;; Register Svelte language server
(straight-use-package 'evil-matchit)
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("svelteserver" "--stdio"))
    :activation-fn (lambda (filename &optional _)
                     (and filename (string-match-p "\\.svelte\\'" filename)))
    :major-modes '(web-mode)
    :priority 10
    :language-id "svelte"
    :server-id 'svelte-ls)))
;; evil-matchit: makes % jump between matching HTML tags
(with-eval-after-load 'evil
  (require 'evil-matchit nil t)
  (with-eval-after-load 'evil-matchit
    (global-evil-matchit-mode 1)))
(provide 'my-svelte)
;;; my-svelte.el ends here
