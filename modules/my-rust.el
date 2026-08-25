;;; my-rust.el -- Rust configuration

;;; Commentary:
;; Rust setup using rust-mode + lsp-mode

;;; Code:
(require 'lsp-rust)

(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
(add-hook 'rust-mode-hook #'lsp)

(setq rust-format-on-save t)

;; Cargo commands
(with-eval-after-load 'rust-mode
  (define-key rust-mode-map (kbd "C-c C-c") #'rust-run)
  (define-key rust-mode-map (kbd "C-c C-b") #'rust-build)
  (define-key rust-mode-map (kbd "C-c C-t") #'rust-test))

(provide 'my-rust)
;;; my-rust.el ends here
