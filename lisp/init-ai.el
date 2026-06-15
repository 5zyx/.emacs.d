;; init-ai.el --- Initialize AI configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2026 Vincent Zhang

;; Author: Vincent Zhang <seagle0128@gmail.com>
;; URL: https://github.com/seagle0128/.emacs.d

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; AI configurations.
;;

;;; Code:

(eval-when-compile
  (require 'init-const))

;; Interact with ChatGPT or other LLMs
(use-package gptel
  :diminish
  :functions (gptel-make-openai gptel-make-anthropic)
  :bind (("C-<f12>"   . gptel)
         ("C-M-<f12>" . gptel-menu))
  :hook (gptel-mode . gptel-highlight-mode)
  :custom (gptel-use-curl nil)
  :config
  (setq gptel-model 'qwen3.7-max
        gptel-backend
        (gptel-make-anthropic "Bailian"
          :host "token-plan.cn-beijing.maas.aliyuncs.com"
          :endpoint "/apps/anthropic/v1/messages"
          :stream nil
          :key #'gptel-api-key-from-auth-source
          :request-params '(:thinking (:type "disabled"))
          :models '(qwen3.7-max qwen3.6-plus qwen3.6-flash
                    deepseek-v4-pro deepseek-v4-flash deepseek-v3.2
                    kimi-k2.6 kimi-k2.5
                    glm-5.1 glm-5
                    MiniMax-M2.5)))

  (gptel-make-openai "Github Models"
    :host "models.inference.ai.azure.com"
    :endpoint "/chat/completions?api-version=2024-05-01-preview"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '(gpt-4o))

  (gptel-make-openai "Nvidia"
    :host "integrate.api.nvidia.com"
    :endpoint "/v1/chat/completions"
    :stream t
    :key #'gptel-api-key-from-auth-source
    :models '(z-ai/glm4.7 minimaxai/minimax-m2.1 deepseek-ai/deepseek-v3.1-terminus))

  (gptel-make-anthropic "Claude"
    :stream t
    :key #'gptel-api-key-from-auth-source))

;; Generate commit messages for magit
(use-package gptel-magit
  :hook (magit-mode . gptel-magit-install))

;; A native shell experience to interact with ACP agents
(when emacs/>=29p
  (use-package agent-shell
    :diminish agent-shell-ui-mode
    :commands agent-shell-insert
    :defines magit-mode-map
    :functions (magit-staged-files magit-commit-p magit-thing-at-point)
    :custom (agent-shell-display-action '(display-buffer-reuse-window))
    :bind (("<f12>"      . agent-shell)
           ("<f13>"      . agent-shell)
           ("C-c a"      . agent-shell)
           ("C-c A"      . agent-shell-new-shell)
           :map agent-shell-mode-map
           ("C-h ?"      . agent-shell-help-menu)
           ("C-<return>" . agent-shell-help-menu)
           :map magit-mode-map
           ("C-c C-g"    . my/agent-shell-magit-generate-commit)
           ("C-c C-r"    . my/agent-shell-review-magit-commit))
    :config
    (with-eval-after-load 'magit
      (defun my/agent-shell-magit-generate-commit ()
        "Generate conventional message and commit stage changes in magit."
        (interactive)
        (if (magit-staged-files)
            (agent-shell-insert
             :submit t
             :text "Commit changes with conventional message")
          (user-error "No staged changes")))

      (defun my/agent-shell-review-magit-commit ()
        "Send the commit from magit to agent-shell for reviews."
        (interactive)
        (if-let ((commit (magit-commit-p (magit-thing-at-point 'git-revision t))))
            (agent-shell-insert
             :submit t
             :text (format "Review commit: %s" commit))
          (user-error "No magit commit at point"))))))

;; --- Bailian Image Generation ---
(defvar centaur-bailian-image-models
  '(("qwen-image-2.0-pro" . sync)
    ("qwen-image-2.0" . sync)
    ("wan2.7-image-pro" . async)
    ("wan2.7-image" . async))
  "Bailian image generation models and their API call types.")

(defvar centaur-bailian-image-dir
  (expand-file-name "bailian-images/" user-emacs-directory)
  "Directory to save generated images.")

(defun centaur-bailian--get-api-key ()
  "Get Bailian API key from auth-source."
  (let ((info (auth-source-search :host "token-plan.cn-beijing.maas.aliyuncs.com" :max 1)))
    (if info
        (let ((s (plist-get (car info) :secret)))
          (if (functionp s) (funcall s) s))
      (user-error "No API key for token-plan.cn-beijing.maas.aliyuncs.com in auth-source"))))

(defun centaur-bailian--download-image (url)
  "Download image from URL to centaur-bailian-image-dir, return file path."
  (unless (file-directory-p centaur-bailian-image-dir)
    (make-directory centaur-bailian-image-dir t))
  (let* ((ts (format-time-string "%Y-%m-%d-%H%M%S"))
         (fp (expand-file-name (format "bailian-%s.png" ts)
                               centaur-bailian-image-dir)))
    (url-copy-file url fp t)
    fp))

(defun centaur-bailian--extract-image-url (resp)
  "Extract image URL from API response RESP."
  (let* ((choices (map-nested-elt resp '(output choices)))
         (c0 (and choices (> (length choices) 0) (aref choices 0)))
         (content (and c0 (map-nested-elt c0 '(message content))))
         (item (and content (> (length content) 0) (aref content 0))))
    (when item (alist-get 'image item))))

(defun centaur-bailian--handle-image-response (resp)
  "Process image response RESP: extract URL, download, display."
  (if-let ((url (centaur-bailian--extract-image-url resp)))
      (let ((fp (centaur-bailian--download-image url)))
        (message "Image saved: %s" fp)
        (find-file fp)
        fp)
    (message "Image generation failed: %s" (json-encode resp))
    nil))

(defun centaur-bailian--build-qwen-image-body (prompt model &optional size negative-prompt)
  "Build request body for Qwen-image generation."
  (let ((params `((size . ,(or size "2048*2048"))
                  (prompt_extend . t)
                  (watermark . nil))))
    (when negative-prompt
      (push `(negative_prompt . ,negative-prompt) params))
    (list
     (cons 'model model)
     (cons 'input
           (list (cons 'messages
                       (vector (list (cons 'role "user")
                                     (cons 'content (vector (list (cons 'text prompt)))))))))
     (cons 'parameters params))))

(defun centaur-bailian--build-wan-image-body (prompt model &optional size)
  "Build request body for Wan image generation."
  (list
   (cons 'model model)
   (cons 'input
         (list (cons 'messages
                     (vector (list (cons 'role "user")
                                   (cons 'content (vector (list (cons 'text prompt)))))))))
   (cons 'parameters `((size . ,(or size "2K"))
                       (watermark . nil)
                       (thinking_mode . t)))))

(defun centaur-bailian--qwen-image-gen (prompt model &optional size negative-prompt callback)
  "Generate image with Qwen-image sync API, then CALLBACK with file path.
PROMPT is text, MODEL is model ID, optional SIZE and NEGATIVE-PROMPT."
  (let* ((data (centaur-bailian--build-qwen-image-body prompt model size negative-prompt))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (centaur-bailian--get-api-key)))
            ("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode data) 'utf-8)))
    (message "Generating with %s..." model)
    (url-retrieve
     "https://token-plan.cn-beijing.maas.aliyuncs.com/api/v1/services/aigc/multimodal-generation/generation"
     (lambda (status)
       (if (plist-get status :error)
           (message "HTTP error: %s" (plist-get status :error))
         (goto-char url-http-end-of-headers)
         (let ((resp (json-read)))
           (when callback
             (funcall callback (centaur-bailian--handle-image-response resp))))))
     nil t)))

(defun centaur-bailian--wan-image-gen (prompt model &optional size callback)
  "Generate image with Wan async API + polling, then CALLBACK with file path.
PROMPT is text, MODEL is model ID, optional SIZE."
  (let* ((data (centaur-bailian--build-wan-image-body prompt model size))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (centaur-bailian--get-api-key)))
            ("Content-Type" . "application/json")
            ("X-DashScope-Async" . "enable")))
         (url-request-data (encode-coding-string (json-encode data) 'utf-8)))
    (message "Submitting task to %s..." model)
    (url-retrieve
     "https://token-plan.cn-beijing.maas.aliyuncs.com/api/v1/services/aigc/image-generation/generation"
     (lambda (status)
       (if (plist-get status :error)
           (message "HTTP error: %s" (plist-get status :error))
         (goto-char url-http-end-of-headers)
         (let* ((resp (json-read))
                (task-id (map-nested-elt resp '(output task_id))))
           (if task-id
               (progn
                 (message "Task %s submitted, polling..." task-id)
                 (run-at-time 3 nil #'centaur-bailian--poll-task task-id callback))
             (message "Task submission failed: %s" (json-encode resp))))))
     nil t)))

(defun centaur-bailian--poll-task (task-id callback)
  "Poll task with TASK-ID until complete, then CALLBACK with file path."
  (let ((url-request-method "GET")
        (url-request-extra-headers
         `(("Authorization" . ,(concat "Bearer " (centaur-bailian--get-api-key))))))
    (url-retrieve
     (format "https://token-plan.cn-beijing.maas.aliyuncs.com/api/v1/tasks/%s" task-id)
     (lambda (status)
       (if (plist-get status :error)
           (message "Poll error: %s" (plist-get status :error))
         (goto-char url-http-end-of-headers)
         (let* ((resp (json-read))
                (task-status (map-nested-elt resp '(output task_status))))
           (cond
            ((string= task-status "SUCCEEDED")
             (when callback
               (funcall callback (centaur-bailian--handle-image-response resp))))
            ((string= task-status "FAILED")
             (message "Image generation failed: %s"
                      (alist-get 'message resp)))
            (t
             (message "Task %s: %s, polling..." task-id task-status)
             (run-at-time 3 nil #'centaur-bailian--poll-task task-id callback))))))
     nil t)))

(defun centaur-bailian-generate-image (prompt model &optional size)
  "Generate an image via Bailian API.
PROMPT is text description, MODEL is the model ID, optional SIZE."
  (interactive
   (list (read-string "Prompt: ")
         (completing-read "Model: "
                          (mapcar #'car centaur-bailian-image-models)
                          nil t "qwen-image-2.0-pro")
         (let ((s (read-string "Size (optional, e.g. 2048*2048 or 2K): ")))
           (when (> (length s) 0) s))))
  (let ((model-type (alist-get model centaur-bailian-image-models nil nil #'string=)))
    (cond
     ((eq model-type 'sync)
      (centaur-bailian--qwen-image-gen prompt model size nil nil))
     ((eq model-type 'async)
      (centaur-bailian--wan-image-gen prompt model size nil))
     (t (user-error "Unknown model: %s" model)))))

(defun org-babel-execute:bailian-image (body params)
  "Execute bailian-image src block.
BODY is the prompt text, PARAMS are header arguments."
  (let* ((model (or (cdr (assq :model params)) "qwen-image-2.0-pro"))
         (size (cdr (assq :size params)))
         (negative-prompt (cdr (assq :negative_prompt params)))
         (model-type (alist-get model centaur-bailian-image-models nil nil #'string=))
         (prompt (string-trim body)))
    (cond
     ((eq model-type 'sync)
      (centaur-bailian--generate-image-sync prompt model size negative-prompt))
     ((eq model-type 'async)
      (centaur-bailian--generate-image-sync-async prompt model size))
     (t (user-error "Unknown model: %s" model)))))

(defun centaur-bailian--generate-image-sync (prompt model &optional size negative-prompt)
  "Generate image synchronously with Qwen-image API, return org link."
  (let* ((data (centaur-bailian--build-qwen-image-body prompt model size negative-prompt))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (centaur-bailian--get-api-key)))
            ("Content-Type" . "application/json")))
         (url-request-data (encode-coding-string (json-encode data) 'utf-8)))
    (message "Generating with %s..." model)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://token-plan.cn-beijing.maas.aliyuncs.com/api/v1/services/aigc/multimodal-generation/generation")
      (goto-char url-http-end-of-headers)
      (let* ((resp (json-read))
             (fp (centaur-bailian--handle-image-response resp)))
        (when fp
          (format "[[file:%s]]" (file-relative-name fp)))))))

(defun centaur-bailian--generate-image-sync-async (prompt model &optional size)
  "Generate image synchronously with Wan async API, return org link."
  (let* ((data (centaur-bailian--build-wan-image-body prompt model size))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " (centaur-bailian--get-api-key)))
            ("Content-Type" . "application/json")
            ("X-DashScope-Async" . "enable")))
         (url-request-data (encode-coding-string (json-encode data) 'utf-8)))
    (message "Submitting task to %s..." model)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://token-plan.cn-beijing.maas.aliyuncs.com/api/v1/services/aigc/image-generation/generation")
      (goto-char url-http-end-of-headers)
      (let* ((resp (json-read))
             (task-id (map-nested-elt resp '(output task_id))))
        (if task-id
            (progn
              (message "Task %s submitted, waiting..." task-id)
              (let ((result (centaur-bailian--poll-task-sync task-id)))
                (when result
                  (let ((fp (centaur-bailian--handle-image-response result)))
                    (when fp
                      (format "[[file:%s]]" (file-relative-name fp)))))))
          (message "Task submission failed: %s" (json-encode resp))
          nil)))))

(defun centaur-bailian--poll-task-sync (task-id)
  "Poll task with TASK-ID synchronously until complete, return response."
  (let ((max-attempts 60)
        (attempt 0)
        result)
    (while (and (< attempt max-attempts) (not result))
      (cl-incf attempt)
      (message "Polling task %s (attempt %d)..." task-id attempt)
      (let* ((url-request-method "GET")
             (url-request-extra-headers
              `(("Authorization" . ,(concat "Bearer " (centaur-bailian--get-api-key))))))
        (with-current-buffer
            (url-retrieve-synchronously
             (format "https://token-plan.cn-beijing.maas.aliyuncs.com/api/v1/tasks/%s" task-id))
          (goto-char url-http-end-of-headers)
          (let* ((resp (json-read))
                 (task-status (map-nested-elt resp '(output task_status))))
            (cond
             ((string= task-status "SUCCEEDED")
              (setq result resp))
             ((string= task-status "FAILED")
              (user-error "Image generation failed: %s"
                          (alist-get 'message resp)))
             (t (sleep-for 3)))))))
    (or result (user-error "Task polling timed out after %d attempts" max-attempts))))

(provide 'init-ai)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ai.el ends here
