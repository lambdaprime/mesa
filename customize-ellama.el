;;
;; This source file is a part of mesa project.
;; Description for this project can be found in README.org.
;;
;; Mesa is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Mesa is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with mesa. If not, see <http://www.gnu.org/licenses/>.
;;
;;

;;; Description:

;; Customize ellama

;;; Code:

(use-package ellama
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  (setopt ellama-spinner-enabled t)
  (setopt ellama-language "English")
  (setopt ellama-auto-scroll t)
  (setopt ellama-tools-allow-all nil)
  (setopt ellama-chat-display-action-function #'display-buffer-full-frame)
  ;(setopt ellama-session-auto-save nil)
  (let ((home (file-name-directory load-file-name)))
    (setopt ellama-blueprints
     `((:act "Commit changes reviewer"
        :prompt "Review following commit changes. When making suggestions always include original lines.\12In the end of the review generate a commit message.\12"
        :for-devs t)
       (:act "duke"
        :prompt ,(read-file (concat home "/prompts/duke.txt"))
        :for-devs t)
       (:act "elisp"
        :prompt "You are ELISP software developer\12\12"
        :for-devs t))))
  (require 'llm-ollama)
  (require 'llm-openai)
  ;(setq default-llm (make-llm-ollama
  ;  :chat-model "mistral"
  ;  :embedding-model "mistral"))
  ;; Change default Ollama provider from local to remote
  ;; Default Ollama provider is used by (ellama-get-ollama-model-name) to
  ;; list available Ollama models
  ;(setopt ellama-provider (make-llm-ollama
  ;  :host "192.168.0.112"
  ;  :port 8080))
  (setopt ellama-providers
    '(("sl-mistral" . (make-llm-ollama
        :chat-model "mistral"
        :port 8080
        :embedding-model "mistral"))
      ("sl-devstral-small-2" . (make-llm-ollama
        :chat-model "devstral-small-2:latest"
        :port 8080
        :embedding-model "devstral-small-2:latest"))
      ("sl-qwen3-coder:30b" . (make-llm-ollama
        :chat-model "qwen3-coder:30b"
        :port 8080
        :embedding-model "qwen3-coder:30b"))
      ("sl-llama3.1:8b" . (make-llm-ollama
        :chat-model "llama3.1:8b"
        :port 8080
        :embedding-model "llama3.1:8b"))
      ("sl-ministral-3:8b" . (make-llm-ollama
        :chat-model "ministral-3:8b"
        :port 8080
        :embedding-model "ministral-3:8b"))
      ("sl-deepseek-r1:8b" . (make-llm-ollama
        :chat-model "deepseek-r1:8b"
        :default-chat-non-standard-params '(("think" . t))
        :port 8080))
      ("sl-nemotron-3-nano-30b-a3b" . (make-llm-ollama
        :chat-model "nvidia/nemotron-3-nano-30b-a3b"
        :port 8080
        :embedding-model "nvidia/nemotron-3-nano-30b-a3b"))
      ;("openai-model-example" . (make-llm-openai-compatible
      ;  :chat-model "example"
      ;  :key "xxxxxx"
      ;  :url "https://openai.com/example/v1"))
      )))

(defun ellama-sessions-directory ()
  "Set directory where sessions are stored"
  (interactive)
  (setopt ellama-sessions-directory (concat (ask-directory-path) "/ellama-sessions")))
