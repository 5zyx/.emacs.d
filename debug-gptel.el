;;; debug-gptel.el --- Debug gptel Bailian connection

(require 'gptel)
(require 'auth-source)

(defun debug-bailian-gptel ()
  "Debug Bailian gptel configuration."
  (interactive)
  
  ;; Step 1: Check if API key is accessible
  (message "=== Step 1: Checking API key ===")
  (let ((api-key (let ((info (auth-source-search :host "token-plan.cn-beijing.maas.aliyuncs.com" :max 1)))
                   (when info
                     (let ((s (plist-get (car info) :secret)))
                       (if (functionp s) (funcall s) s))))))
    (if api-key
        (message "✓ API key found (length: %d)" (length api-key))
      (message "✗ API key NOT found in authinfo!")
      (error "API key not configured")))
  
  ;; Step 2: Check if Bailian backend exists
  (message "\n=== Step 2: Checking Bailian backend ===")
  (let ((backend (gptel-get-backend "Bailian")))
    (if backend
        (progn
          (message "✓ Bailian backend found")
          (message "  Host: %s" (gptel-backend-host backend))
          (message "  Endpoint: %s" (gptel-backend-endpoint backend))
          (message "  Stream: %s" (gptel-backend-stream backend)))
      (message "✗ Bailian backend NOT found!")
      (message "Available backends: %s" (mapcar #'car gptel--known-backends))
      (error "Bailian backend not registered")))
  
  ;; Step 3: Test simple request
  (message "\n=== Step 3: Testing API request ===")
  (let* ((backend (gptel-get-backend "Bailian"))
         (api-key (funcall (gptel-backend-key backend)))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " api-key))
            ("Content-Type" . "application/json")
            ("anthropic-version" . "2023-06-01")))
         (url-request-data (encode-coding-string
                           (json-encode '((model . "qwen3.7-max")
                                         (messages . [((role . "user")
                                                       (content . "Say hello"))])
                                         (max_tokens . 20)
                                         (thinking . ((type . "disabled")))))
                           'utf-8)))
    (message "Sending test request...")
    (condition-case err
        (with-current-buffer
            (url-retrieve-synchronously
             (format "https://%s%s"
                     (gptel-backend-host backend)
                     (gptel-backend-endpoint backend)))
          (goto-char url-http-end-of-headers)
          (let ((response (json-read)))
            (message "✓ Response received!")
            (message "Response: %s" (json-encode response))
            (let ((content (map-nested-elt response '(content))))
              (when content
                (dotimes (i (length content))
                  (let ((item (aref content i)))
                    (when (string= (alist-get 'type item) "text")
                      (message "✓ Text response: %s" (alist-get 'text item)))))))))
      (error
       (message "✗ Request failed: %s" (error-message-string err))))))

(provide 'debug-gptel)
