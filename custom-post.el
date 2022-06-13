(use-package evil
  :init (evil-mode 1))
(use-package htmlize)
(use-package racket-mode)
(use-package esxml)
(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)



(add-hook 'org-insert-heading-hook
          (lambda () (org-set-property "CREATED" (format-time-string "%Y/%m/%d %H:%M"))))
