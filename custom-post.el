(use-package evil
  :init (evil-mode 1))
  (use-package htmlize)
(use-package racket-mode)
(use-package esxml)
(use-package ox-hugo
  :ensure t   ;Auto-install the package from Melpa
  :pin melpa  ;`package-archives' should already have ("melpa" . "https://melpa.org/packages/")
  :after ox)
(use-package easy-hugo)
(use-package leetcode)


(add-hook 'org-insert-heading-hook
          (lambda () (org-set-property "CREATED" (format-time-string "%Y/%m/%d %H:%M"))))

(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'endless/org-ispell)

(require 'baidu-dictionary)

;; Baidu Dictionary
(use-package baidu-dictionary
  :load-path "site-lisp/"
  :bind (("C-c y"   . my-baidu-dictionary-search-at-point))
  :init
  (setq url-automatic-caching t)
  :config
  (with-no-warnings
    (with-eval-after-load 'hydra
      (setq baidu-dictionary-use-chinese-word-segmentation t) ; 中文分词
      (defhydra baidu-dictionary-hydra (:color blue)
        ("q" quit-window "quit")
        ("C-g" nil nil)
        ("h" nil nil)
        ("?" nil nil))
      )

    (defun my-baidu-dictionary-search-at-point ()
      "Search word at point and display result with `posframe', `pos-tip' or buffer."
      (interactive)
      (if (posframe-workable-p)
          (baidu-dictionary-search-at-point-posframe)
        (baidu-dictionary-search-at-point)))

    (defun my-baidu-dictionary--posframe-tip (string)
      "Show STRING using `posframe-show'."
      (unless (posframe-workable-p)
        (error "Posframe not workable"))

      (if-let ((word (baidu-dictionary--region-or-word)))
          (progn
            (with-current-buffer (get-buffer-create baidu-dictionary-buffer-name)
              (let ((inhibit-read-only t))
                (erase-buffer)
                                        ;(baidu-dictionary-mode)
                (insert string)
                (set (make-local-variable 'baidu-dictionary-current-buffer-word) word)))
            (posframe-show
             baidu-dictionary-buffer-name
             :position (point)
             :left-fringe 8
             :right-fringe 8
             :max-width (/ (frame-width) 2)
             :max-height (/ (frame-height) 2)
             :background-color (face-background 'tooltip nil t)
             :internal-border-color (face-background 'posframe-border nil t)
             :internal-border-width 1)
            (unwind-protect
                (push (read-event) unread-command-events)
              (progn
                (posframe-hide baidu-dictionary-buffer-name)
                (other-frame 0)))
            (message "Nothing to look up"))))
    (advice-add #'baidu-dictionary--posframe-tip
                :override #'my-baidu-dictionary--posframe-tip)))
