;;; my-go.el -- Code for go configuration

;;; Commentary:

;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)

(setq package-selected-packages
  '(dart-mode lsp-mode lsp-dart lsp-treemacs flycheck company
    ;; Optional packages
    lsp-ui company hover))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(add-hook 'dart-mode-hook 'lsp)

(setq dart-format-on-save t)

(setq gc-cons-threshold 200000000
      ;; lsp-idle-delay 0.500
      read-process-output-max (* 1024 1024)
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-log-io nil
      lsp-signature-auto-activate nil)
(provide 'my-dart)
;;; my-dart.el ends here
