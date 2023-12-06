;;; my-sql.el -- Code for SQL configuration

;;; Commentary:
;;; The lsp setup here is derived from https://github.com/lighttiger2505/sqls

;;; Code:
(add-hook 'sql-mode-hook 'lsp)
(setq lsp-sqls-workspace-config-path nil)
(setq lsp-sqls-connections
    '(((driver . "postgresql") (dataSourceName . "postgresql://postgres:postgres@127.0.0.1:54322/postgres"))))

;; format on save with pgformatter, https://github.com/darold/pgFormatter
;; Function to run pg_format on the current buffer, courtesy Mr. GPT
(defun pgformat-before-save ()
  "Run pg_format on the current buffer."
  (when (eq major-mode 'sql-mode) ;; Adjust mode if needed
    (shell-command-on-region (point-min) (point-max) "pg_format" t t)))

;; Add pgformat-before-save to before-save-hook
(add-hook 'before-save-hook #'pgformat-before-save)

(provide 'my-sql)
;;; my-sql.el ends here
