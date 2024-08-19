;;; my-javascript -- Code for javascript configuration

;;; Commentary:
;; Includes Vue and Tide config

;;; Code:
(use-package tide :ensure t)
(use-package company :ensure t)
(use-package flycheck :ensure t)

;; json-mode
(use-package json-mode
  :ensure t)

;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.svelte\\'" . web-mode)
	 ("\\.html\\'" . web-mode)))

;; (add-hook 'after-init-hook #'global-prettier-mode)
(add-hook 'before-save-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (prettier-prettify))))
(add-hook 'before-save-hook
          (lambda ()
            (when (string-equal "ts" (file-name-extension buffer-file-name))
              (prettier-prettify))))
(add-hook 'before-save-hook
          (lambda ()
            (when (string-equal "js" (file-name-extension buffer-file-name))
              (prettier-prettify))))

(add-hook 'before-save-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (prettier-prettify))))

(add-hook 'web-mode-hook
          (lambda ()
            (when (or (string-equal "ts" (file-name-extension buffer-file-name))
                      (string-equal "tsx" (file-name-extension buffer-file-name))
                      (string-equal "js" (file-name-extension buffer-file-name))
                      (string-equal "jsx" (file-name-extension buffer-file-name)))
              (setup-tide-mode))))

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; set indent to 2
(setq-default typescript-indent-level 2)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)

;; enable typescript - tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
(flycheck-add-mode 'typescript-tide 'web-mode)

(use-package vue-mode
  :config
  (setq mmm-submode-decoration-level 0)
  (add-hook 'vue-mode-hook #'lsp))

(use-package emmet-mode
  :hook (css-mode sgml-mode vue-mode js-jsx-mode js-mode))

(use-package eslintd-fix
  :config
    (add-hook 'vue-mode-hook 'eslintd-fix-mode)
    (add-hook 'js-jsx-mode-hook 'eslintd-fix-mode)
    (add-hook 'js-mode-hook 'eslintd-fix-mode))

(add-hook 'js-mode-hook 'lsp-deferred)
(add-hook 'js-jsx-mode-hook 'lsp-deferred)

(use-package svelte-mode)

(use-package typescript-mode)

(quelpa '(npm :fetcher github :repo "shaneikennedy/npm.el"))
(require 'npm )

(defun sk/vue-base()
  "Snippet for base vue template."
  (interactive)
  (insert "<template>\n</template>
	    \n<script>\n export default {};\n</script>
	    \n\n<style scoped>\n</style>"))

(provide 'my-javascript)
;;; my-javascript.el ends here
