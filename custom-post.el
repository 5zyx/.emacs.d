(use-package evil
  :init
  (evil-mode 1)
  ;;(lsp-ui-doc-enable nil)
  )

;;; custom.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(go-test-verbose t)
 '(lsp-ui-doc-position (quote top)))

(add-hook 'go-mode-hook (lambda()
                          (local-set-key (kbd "C-c C-j") 'lsp-find-definition) ))
