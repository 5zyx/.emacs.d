;;; test-api-simple.el --- Simple API test without gptel

(require 'url)
(require 'json)
(require 'auth-source)

(defun test-bailian-simple ()
  "Test Bailian API directly without gptel."
  (interactive)
  
  ;; Get API key
  (let ((api-key (let ((info (auth-source-search :host "token-plan.cn-beijing.maas.aliyuncs.com" :max 1)))
                   (when info
                     (let ((s (plist-get (car info) :secret)))
                       (if (functionp s) (funcall s) s))))))
    
    (if (not api-key)
        (message "ERROR: No API key found!")
      
      (message "API key found, testing...")
      
      (let* ((url-request-method "POST")
             (url-request-extra-headers
              `(("Authorization" . ,(concat "Bearer " api-key))
                ("Content-Type" . "application/json")))
             (url-request-data (encode-coding-string
                               (json-encode '((model . "qwen3.7-max")
                                             (messages . [((role . "user")
                                                           (content . "Say hello in one word"))])
                                             (max_tokens . 10)
                                             (thinking . ((type . "disabled")))))
                               'utf-8)))
        
        (message "Sending request to Bailian API...")
        
        (condition-case err
            (with-current-buffer
                (url-retrieve-synchronously
                 "https://token-plan.cn-beijing.maas.aliyuncs.com/apps/anthropic/v1/messages")
              (goto-char url-http-end-of-headers)
              (let ((response (json-read)))
                (message "✓ API test successful!")
                (let ((content (map-nested-elt response '(content))))
                  (when content
                    (dotimes (i (length content))
                      (let ((item (aref content i)))
                        (when (string= (alist-get 'type item) "text")
                          (message "Response: %s" (alist-get 'text item)))))))))
          (error
           (message "✗ API test failed: %s" (error-message-string err))))))))
