;;; my-javascript -- Code for javascript configuration

;;; Commentary:
;; Includes Vue, Svelte and Tide config

;;; Code:
(use-package tide :ensure t)
(use-package company :ensure t)
(use-package flycheck :ensure t)
(use-package eglot :ensure t)

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
	 ("\\.html\\'" . web-mode)))

;; Svelte mode
(use-package svelte-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.svelte\\'" . svelte-mode)))

(use-package typescript-mode
  :ensure t)

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

(add-hook 'before-save-hook
          (lambda ()
            (when (string-equal "svelte" (file-name-extension buffer-file-name))
              (prettier-prettify))))


(add-hook 'web-mode-hook
          (lambda ()
            (when (or (string-equal "ts" (file-name-extension buffer-file-name))
                      (string-equal "tsx" (file-name-extension buffer-file-name))
                      (string-equal "js" (file-name-extension buffer-file-name))
                      (string-equal "jsx" (file-name-extension buffer-file-name)))
              (setup-tide-mode))))

;; Setup Svelte language server for Svelte mode
(add-hook 'svelte-mode-hook #'eglot-ensure)

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
  :ensure t
  :config
  (setq mmm-submode-decoration-level 0)
  (add-hook 'vue-mode-hook #'lsp))

(use-package emmet-mode
  :ensure t
  :hook (css-mode sgml-mode vue-mode svelte-mode js-jsx-mode js-mode))

(use-package eslintd-fix
  :ensure t
  :config
    (add-hook 'vue-mode-hook 'eslintd-fix-mode)
    (add-hook 'svelte-mode-hook 'eslintd-fix-mode)
    (add-hook 'js-jsx-mode-hook 'eslintd-fix-mode)
    (add-hook 'js-mode-hook 'eslintd-fix-mode))

(add-hook 'js-mode-hook 'lsp-deferred)
(add-hook 'js-jsx-mode-hook 'lsp-deferred)

(setq tide-server-max-response-length 1024000)

(use-package svelte-mode)

(use-package typescript-mode)

;; Deno project detection
(defun is-deno-project-p ()
  "Check if current buffer is in a Deno project by looking for deno.json"
  (let ((dir (or (buffer-file-name) default-directory)))
    (when dir
      (or (file-exists-p (expand-file-name "deno.json" (file-name-directory dir)))
          (locate-dominating-file dir "deno.json")))))

;; Svelte project detection
(defun is-svelte-project-p ()
  "Check if current buffer is in a Svelte project by looking for svelte.config.js"
  (let ((dir (or (buffer-file-name) default-directory)))
    (when dir
      (or (file-exists-p (expand-file-name "svelte.config.js" (file-name-directory dir)))
          (file-exists-p (expand-file-name "svelte.config.cjs" (file-name-directory dir)))
          (locate-dominating-file dir "svelte.config.js")
          (locate-dominating-file dir "svelte.config.cjs")))))

;; Deno eglot class and configuration - wrapped in eval-after-load
(with-eval-after-load 'eglot
  (defclass eglot-deno (eglot-lsp-server) ()
    :documentation "A custom class for deno lsp.")

  (cl-defmethod eglot-initialization-options ((server eglot-deno))
    "Passes through required deno initialization options"
    (list
      :enable t
      :unstable t
      :typescript
        (:inlayHints
          (:variableTypes
            (:enabled t))
          (:parameterTypes
            (:enabled t)))))

  ;; Conditional server program setup
  (defun setup-typescript-lsp ()
    "Setup appropriate LSP server based on project type"
    (if (is-deno-project-p)
        (add-to-list 'eglot-server-programs '((js-mode typescript-mode web-mode) . (eglot-deno "deno" "lsp")))
      ;; Keep existing LSP setup for non-Deno projects
      (add-to-list 'eglot-server-programs '((js-mode typescript-mode web-mode) . ("typescript-language-server" "--stdio")))))

  ;; Setup LSP based on project type
  (setup-typescript-lsp))

;; Convert npm package to use straight.el instead of quelpa
(use-package npm
  :straight (:host github :repo "shaneikennedy/npm.el"))

(defun sk/vue-base()
  "Snippet for base vue template."
  (interactive)
  (insert "<template>\n</template>
	    \n<script>\n export default {};\n</script>
	    \n\n<style scoped>\n</style>"))

(defun sk/svelte-base()
  "Snippet for base Svelte component template."
  (interactive)
  (insert "<script>\n  // Component logic here\n</script>
	    \n\n<main>\n  <!-- Your markup here -->\n</main>
	    \n\n<style>\n  /* Component styles here */\n</style>"))

(provide 'my-javascript)
;;; my-javascript.el ends here
