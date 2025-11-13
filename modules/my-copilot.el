;;; copilot.el --- Emacs Copilot

;;; Commentary:

;;; Code:
(require 'use-package)

;; Install copilot from GitHub using straight.el
(use-package copilot
  :straight (:host github
             :repo "copilot-emacs/copilot.el"
             :branch "main"
             :files ("*")))

(define-key copilot-mode-map (kbd "C-c C-a") 'copilot-accept-completion)
(define-key copilot-mode-map (kbd "C-c C-c") 'copilot-next-completion)

;; TODO: this should be somewhere more global
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

(use-package emigo
  :straight (:host github :repo "govi218/emigo" :files (:defaults "*.py" "*.el"))
  :config
  (emigo-enable) ;; Starts the background process automatically
  :custom
  ;; Encourage using OpenRouter with Deepseek
  (emigo-model "openrouter/deepseek/deepseek-chat-v3-0324")
  (emigo-base-url "https://openrouter.ai/api/v1")
  (emigo-api-key (getenv "OPENROUTER_API_KEY")))

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


(provide 'my-copilot)

;;; copilot.el ends here
