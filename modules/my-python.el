;;; my-python.el -- Code for python configuration
;;; Commentary:
;; Nothing special besides the use of django-test-runner
;; this package is not available on melpa so we need to configure it
;; locally.
;;; Code:

(use-package python
  :straight nil  ; built-in package
  :bind (:map python-mode-map
              ("<f2>" . py-isort-buffer)
              ("M-q" . blacken-buffer)))

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp))))

(use-package blacken)

(use-package py-isort)

(use-package pyvenv)

;; Add linting support
(use-package flycheck
  :hook (python-mode . flycheck-mode))

(provide 'my-python)
;;; my-python.el ends here
