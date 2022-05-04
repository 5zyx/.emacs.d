(use-package evil
  :init (evil-mode 1))
(use-package htmlize)

(tool-bar-mode 1)
(menu-bar-mode 1)
(scroll-bar-mode 1)

(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((shell         . t)
   (js         . t)
   (emacs-lisp . t)
   (perl       . t)
   (clojure    . t)
   (python     . t)
   (ruby       . t)
   (dot        . t)
   (go        . t)
   (css        . t)
   (calc        . t)
   (sql        . t)
   (sqlite        . t)
   (plantuml   . t)))
