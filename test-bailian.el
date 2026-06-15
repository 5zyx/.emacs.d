;;; test-bailian.el --- Test Bailian Token Plan connection

(require 'url)
(require 'json)

(defun test-bailian-api ()
  "Test Bailian Token Plan API connection."
  (interactive)
  (let* ((api-key (let ((info (auth-source-search :host "token-plan.cn-beijing.maas.aliyuncs.com" :max 1)))
                    (when info
                      (let ((s (plist-get (car info) :secret)))
                        (if (functionp s) (funcall s) s)))))
         (url-request-method "POST")
         (url-request-extra-headers
          `(("Authorization" . ,(concat "Bearer " api-key))
            ("Content-Type" . "application/json")
            ("anthropic-version" . "2023-06-01")))
         (url-request-data (encode-coding-string
                           (json-encode '((model . "qwen3.7-max")
                                         (messages . [((role . "user")
                                                       (content . "Hello"))])
                                         (max_tokens . 50)))
                           'utf-8)))
    (if (not api-key)
        (message "ERROR: No API key found in authinfo")
      (message "Testing Bailian API...")
      (url-retrieve
       "https://token-plan.cn-beijing.maas.aliyuncs.com/apps/anthropic/v1/messages"
       (lambda (status)
         (if (plist-get status :error)
             (message "HTTP Error: %s" (plist-get status :error))
           (goto-char url-http-end-of-headers)
           (let ((response (json-read)))
             (message "Response: %s" (json-encode response))
             (let ((content (map-nested-elt response '(content))))
               (when content
                 (dotimes (i (length content))
                   (let ((item (aref content i)))
                     (when (string= (alist-get 'type item) "text")
                       (message "Text: %s" (alist-get 'text item))))))))))
       nil t))))

(provide 'test-bailian)
