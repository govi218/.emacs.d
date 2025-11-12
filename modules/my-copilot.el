;;; copilot.el --- Emacs Copilot

;;; Commentary:

;;; Code:
(require 'use-package)
(require 'quelpa-use-package)

(use-package copilot
  :quelpa (copilot :fetcher github
                   :repo "copilot-emacs/copilot.el"
                   :branch "main"
                   :files ("*")))

(define-key copilot-mode-map (kbd "C-c C-a") 'copilot-accept-completion)
(define-key copilot-mode-map (kbd "C-c C-c") 'copilot-next-completion)

(defun my-copilot-load-env-file (file)
  "Load environment variables from FILE in .env format."
  (when (file-exists-p file)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (when (looking-at "^\\([A-Z_]+\\)=\\(.*\\)$")
          (setenv (match-string 1) (match-string 2)))
        (forward-line 1)))))

;; Load environment variables from .env file
(my-copilot-load-env-file (expand-file-name ".env" user-emacs-directory))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  ; defun my-get-openrouter-api-key yourself elsewhere for security reasons
  ;; (setenv "OPENROUTER_API_KEY" (my-get-openrouter-api-key))
  :custom
  ; See the Configuration section below
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "openrouter/anthropic/claude-sonnet-4.5"))

;; this is for LLM at home
;; (use-package aidermacs
;;   :bind (("C-c a" . aidermacs-transient-menu))
;;   :config
;;   (setenv "OPENAI_API_BASE" "http://100.115.58.35:8088/v1")
;;   (setenv "OPENAI_API_KEY" "foo")
;;   :custom
;;   ; See the Configuration section below
;;   (aidermacs-use-architect-mode t)
;;   (aidermacs-default-model "openai/TheBloke/CodeLlama-7B-Instruct-GPTQ"))


(provide 'copilot)
;;; my-copilot.el ends here
