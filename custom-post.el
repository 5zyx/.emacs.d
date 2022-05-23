(use-package evil
  :init (evil-mode 1))

(use-package htmlize)

(use-package js-comint)

(use-package esxml)

(tool-bar-mode 1)
(menu-bar-mode 1)
(scroll-bar-mode 1)


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
   (makefile . t)
   (plantuml   . t)))



(add-hook 'org-insert-heading-hook
          (lambda () (org-set-property "CREATED" (format-time-string "%Y/%m/%d %H:%M"))))


(defun ob-js-insert-session-header-arg (session)
  "Insert ob-js `SESSION' header argument.
- `js-comint'
- `skewer-mode'
- `Indium'
"
  (interactive (list (completing-read "ob-js session: "
                                      '("js-comint" "skewer-mode" "indium"))))
  (org-babel-insert-header-arg
   "session"
   (pcase session
     ("js-comint" "\"*Javascript REPL*\"")
     ("skewer-mode" "\"*skewer-repl*\"")
     ("indium" "\"*JS REPL*\""))))

(add-to-list 'org-babel-default-header-args:plantuml
             '((:cmdline . "-charset utf-8")
               (:exports . "both")
               (:session . "do-something")
               (:dir . "./assets")))

(defun node-repl () (interactive)
       (setenv "NODE_NO_READLINE" "1") ;avoid fancy terminal codes
       (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))
