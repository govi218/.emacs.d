;;; my-javascript -- Code for javascript configuration
;;; Commentary:
;; LSP for language features, Prettier for formatting
;;; Code:
(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-c l")
  ;; Disable LSP formatting - we use Prettier
  (setq lsp-enable-on-type-formatting nil)
  (setq lsp-enable-indentation nil))

(use-package company :ensure t)
(use-package flycheck :ensure t)
(use-package json-mode :ensure t)

;; Prettier for formatting
(use-package prettier
  :ensure t)

;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)

(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
         ("\\.jsx\\'" . web-mode)
         ("\\.ts\\'" . web-mode)
         ("\\.tsx\\'" . web-mode)
         ("\\.html\\'" . web-mode)))

;; LSP for TS/JS files
(add-hook 'web-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (member (file-name-extension buffer-file-name) '("ts" "tsx" "js" "jsx")))
              (lsp-deferred))))

;; Prettier format on save for TS/JS files
(add-hook 'web-mode-hook
          (lambda ()
            (when (and buffer-file-name
                       (member (file-name-extension buffer-file-name) '("tsx" "ts" "js" "jsx")))
              (add-hook 'before-save-hook #'prettier-prettify nil t))))

;; LSP for typescript-mode
(add-hook 'typescript-mode-hook #'lsp-deferred)

;; Company and flycheck
(add-hook 'web-mode-hook #'company-mode)
(add-hook 'web-mode-hook #'flycheck-mode)

(setq company-tooltip-align-annotations t)
(setq-default typescript-indent-level 2)

;; Vue
(use-package vue-mode
  :config
  (setq mmm-submode-decoration-level 0)
  (add-hook 'vue-mode-hook #'lsp))

(use-package emmet-mode
  :hook (css-mode sgml-mode vue-mode js-jsx-mode js-mode web-mode))

(use-package eslintd-fix
  :config
  (add-hook 'vue-mode-hook 'eslintd-fix-mode)
  (add-hook 'js-jsx-mode-hook 'eslintd-fix-mode)
  (add-hook 'js-mode-hook 'eslintd-fix-mode)
  (add-hook 'web-mode-hook 'eslintd-fix-mode))

(add-hook 'js-mode-hook 'lsp-deferred)
(add-hook 'js-jsx-mode-hook 'lsp-deferred)

;; Deno project detection
(defun is-deno-project-p ()
  "Check if current buffer is in a Deno project by looking for deno.json"
  (let ((dir (or (buffer-file-name) default-directory)))
    (when dir
      (or (file-exists-p (expand-file-name "deno.json" (file-name-directory dir)))
          (locate-dominating-file dir "deno.json")))))

;; LSP configuration for TypeScript/Deno
(with-eval-after-load 'lsp-mode
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("deno" "lsp"))
                    :activation-fn (lambda (&rest _) (is-deno-project-p))
                    :priority 1
                    :server-id 'deno-ls
                    :major-modes '(js-mode typescript-mode web-mode)))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("typescript-language-server" "--stdio"))
                    :activation-fn (lambda (&rest _) (not (is-deno-project-p)))
                    :priority 0
                    :server-id 'ts-ls
                    :major-modes '(js-mode typescript-mode web-mode))))

(use-package npm
  :straight (:host github :repo "shaneikennedy/npm.el"))

(defun sk/vue-base()
  "Snippet for base vue template."
  (interactive)
  (insert "<template>\n</template>
            \n<script>\n export default {};\n</script>
            \n\n<style scoped>\n</style>"))

(provide 'my-javascript)
;;; my-javascript.el ends here
