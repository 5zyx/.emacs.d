;;; custom.el --- user customization file    -*- no-byte-compile: t -*-
;;; Commentary:
;;;       Add or change the configurations in custom.el, then restart Emacs.
;;;       Put your own configurations in custom-post.el to override default configurations.
;;; Code:

;; (setq centaur-logo nil)                        ; Logo file or nil (official logo)
;; (setq centaur-full-name "user name")           ; User full name
;; (setq centaur-mail-address "user@email.com")   ; Email address
;;(setq centaur-proxy "127.0.0.1:7890")          ; Network proxy
;; (setq centaur-server nil)                      ; Enable `server-mode' or not: t or nil
;; (setq centaur-icon nil)                        ; Display icons or not: t or nil
;; (setq centaur-package-archives 'netease)   ; Package repo: melpa, emacs-china, netease, ustc, tencent or tuna
;; (setq centaur-theme 'light)                    ; Color theme: auto, random, default, classic, colorful, dark, light, day or night
;; (setq centaur-dashboard nil)                   ; Use dashboard at startup or not: t or nil
;; (setq centaur-lsp 'eglot)                      ; Set LSP client: lsp-mode, eglot or nil
;; (setq centaur-lsp-format-on-save-ignore-modes '(c-mode c++-mode)) ; Ignore format on save for some languages
;; (setq centaur-chinese-calendar t)              ; Use Chinese calendar or not: t or nil
(setq centaur-prettify-symbols-alist nil)      ; Alist of symbol prettifications
(setq centaur-prettify-org-symbols-alist nil)  ; Alist of symbol prettifications for `org-mode'
;; (setq centaur-benchmark-init t)                ; Enable initialization benchmark or not: t or nil

;; For Emacs devel
;; (setq package-user-dir (locate-user-emacs-file (format "elpa-%s" emacs-major-version)))
;; (setq desktop-base-file-name (format ".emacs-%s.desktop" emacs-major-version))
;; (setq desktop-base-lock-name (format ".emacs-%s.desktop.lock" emacs-major-version))

;; Fonts
(defun centaur-setup-fonts ()
  "Setup fonts."
  (when (display-graphic-p)
    ;; Set default font
    (cl-loop for font in '("Cascadia Code" "Fira Code" "Jetbrains Mono"
                           "SF Mono" "Hack" "Source Code Pro" "Menlo"
                           "Monaco" "DejaVu Sans Mono" "Consolas")
             when (font-installed-p font)
             return (set-face-attribute 'default nil
                                        :family font
                                        :height (cond (sys/macp 130)
                                                      (sys/win32p 110)
                                                      (t 100))))

    ;; Set mode-line font
    ;; (cl-loop for font in '("Menlo" "SF Pro Display" "Helvetica")
    ;;          when (font-installed-p font)
    ;;          return (progn
    ;;                   (set-face-attribute 'mode-line nil :family font :height 120)
    ;;                   (when (facep 'mode-line-active)
    ;;                     (set-face-attribute 'mode-line-active nil :family font :height 120))
    ;;                   (set-face-attribute 'mode-line-inactive nil :family font :height 120)))

    ;; Specify font for all unicode characters
    (cl-loop for font in '("Segoe UI Symbol" "Symbola" "Symbol")
             when (font-installed-p font)
             return (if (< emacs-major-version 27)
                        (set-fontset-font "fontset-default" 'unicode font nil 'prepend)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend)))

    ;; Emoji
    (cl-loop for font in '("Noto Color Emoji" "Apple Color Emoji" "Segoe UI Emoji")
             when (font-installed-p font)
             return (cond
                     ((< emacs-major-version 27)
                      (set-fontset-font "fontset-default" 'unicode font nil 'prepend))
                     ((< emacs-major-version 28)
                      (set-fontset-font t 'symbol (font-spec :family font) nil 'prepend))
                     (t
                      (set-fontset-font t 'emoji (font-spec :family font) nil 'prepend))))

    ;; Specify font for Chinese characters
    (cl-loop for font in '("WenQuanYi Micro Hei" "PingFang SC" "Microsoft Yahei" "STFangsong")
             when (font-installed-p font)
             return (progn
                      (setq face-font-rescale-alist `((,font . 1.3)))
                      (set-fontset-font t '(#x4e00 . #x9fff) (font-spec :family font))))))

(centaur-setup-fonts)
(add-hook 'window-setup-hook #'centaur-setup-fonts)
(add-hook 'server-after-make-frame-hook #'centaur-setup-fonts)

;; Mail
;; (setq message-send-mail-function 'smtpmail-send-it
;;       smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;;       smtpmail-auth-credentials '(("smtp.gmail.com" 587
;;                                    user-mail-address nil))
;;       smtpmail-default-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-server "smtp.gmail.com"
;;       smtpmail-smtp-service 587)

;; Calendar
;; Set location , then press `S' can show the time of sunrise and sunset
;; (setq calendar-location-name "Chengdu"
;;       calendar-latitude 30.67
;;       calendar-longitude 104.07)

;; Misc.
;; (setq confirm-kill-emacs 'y-or-n-p)

;; Enable proxy
;; (proxy-http-enable)
;; (proxy-socks-enable)

;; Display on the specified monitor
;; (when (and (> (length (display-monitor-attributes-list)) 1)
;;            (> (display-pixel-width) 1920))
;;   (set-frame-parameter nil 'left 1920))

;; (put 'cl-destructuring-bind 'lisp-indent-function 'defun)
;; (put 'pdf-view-create-image 'lisp-indent-function 'defun)
;; (put 'treemacs-create-theme 'lisp-indent-function 'defun)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(easy-hugo-basedir "~/workspace/wow-yorick/blog/")
 '(easy-hugo-postdir "content/posts")
 '(leetcode-directory "~/org/leetcode")
 '(leetcode-prefer-language "golang")
 '(leetcode-prefer-sql "mysql")
 '(leetcode-save-solutions t)
 '(linum-mode t t)
 '(menu-bar-mode t)
 '(org-agenda-files
   '("~/org/work-flow.org" "~/org/book.org" "~/org/gtd.org" "~/org/idea.org" "~/org/journal.org" "~/org/note.org"))
 '(org-confirm-babel-evaluate nil)
 '(org-export-backends '(hugo gfm md pdf ascii html icalendar latex odt))
 '(org-export-with-sub-superscripts '{})
 '(org-html-validation-link nil)
 '(org-hugo-base-dir "~/workspace/wow-yorick/blog")
 '(org-plantuml-jar-path "~/.emacs.d/tools/plantuml-1.2024.4.jar")
 '(org-table-convert-region-max-lines 5000)
 '(org-use-sub-superscripts '{})
 '(package-vc-selected-packages
   '((ultra-scroll :vc-backend Git :url "https://github.com/jdtsmith/ultra-scroll")))
 '(scroll-bar-mode 'right)
 '(tool-bar-mode t)
 '(warning-suppress-log-types '((emacs)))
 '(youdao-dictionary-use-chinese-word-segmentation t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 2.0))))
 '(aw-minibuffer-leading-char-face ((t (:inherit font-lock-keyword-face :bold t :height 1.0))))
 '(aw-mode-line-face ((t (:inherit mode-line-emphasis :bold t))))
 '(cfrs-border-color ((t (:background "#5B6268"))) t)
 '(company-box-selection ((t (:inherit company-tooltip :weight semibold :extend t))))
 '(company-tooltip-annotation ((t (:inherit completions-annotations :foreground nil))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold)))))
 '(diff-hl-change ((t (:foreground "#51afef" :background "unspecified"))))
 '(diff-hl-delete ((t (:inherit diff-removed :background "unspecified"))))
 '(diff-hl-insert ((t (:inherit diff-added :background "unspecified"))))
 '(doom-modeline-buffer-file ((t (:inherit (mode-line bold)))))
 '(flycheck-posframe-background-face ((t (:inherit tooltip))))
 '(flycheck-posframe-border-face ((t (:inherit posframe-border))))
 '(flycheck-posframe-face ((t (:foreground "#98be65"))))
 '(flycheck-posframe-info-face ((t (:foreground "#98be65"))))
 '(git-timemachine-minibuffer-author-face ((t (:inherit success))) t)
 '(git-timemachine-minibuffer-detail-face ((t (:inherit warning))) t)
 '(hl-todo ((t (:inherit default :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
 '(ivy-current-match ((t (:inherit region :distant-foreground nil :background nil))))
 '(ivy-minibuffer-match-face-1 ((t (:foreground "#83898d"))))
 '(ivy-minibuffer-match-face-2 ((t (:distant-foreground nil :background nil))))
 '(ivy-minibuffer-match-face-3 ((t (:distant-foreground nil :background nil))))
 '(ivy-minibuffer-match-face-4 ((t (:distant-foreground nil :background nil))))
 '(ivy-posframe ((t (:inherit tooltip))))
 '(ivy-posframe-border ((t (:inherit posframe-border))))
 '(lsp-headerline-breadcrumb-path-error-face ((t :underline (:style wave :color "#ff6c6b") :inherit lsp-headerline-breadcrumb-path-face)) t)
 '(lsp-headerline-breadcrumb-path-hint-face ((t :underline (:style wave :color "#98be65") :inherit lsp-headerline-breadcrumb-path-face)) t)
 '(lsp-headerline-breadcrumb-path-info-face ((t :underline (:style wave :color "#98be65") :inherit lsp-headerline-breadcrumb-path-face)) t)
 '(lsp-headerline-breadcrumb-path-warning-face ((t :underline (:style wave :color "#ECBE7B") :inherit lsp-headerline-breadcrumb-path-face)) t)
 '(lsp-headerline-breadcrumb-symbols-error-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ff6c6b"))) t)
 '(lsp-headerline-breadcrumb-symbols-hint-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#98be65"))) t)
 '(lsp-headerline-breadcrumb-symbols-info-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#98be65"))) t)
 '(lsp-headerline-breadcrumb-symbols-warning-face ((t :inherit lsp-headerline-breadcrumb-symbols-face :underline (:style wave :color "#ECBE7B"))) t)
 '(lsp-ui-sideline-code-action ((t (:inherit warning))) t)
 '(macrostep-expansion-highlight-face ((t (:inherit tooltip :extend t))) t)
 '(org-ellipsis ((t (:foreground "unspecified"))))
 '(org-pomodoro-mode-line ((t (:inherit warning))) t)
 '(org-pomodoro-mode-line-break ((t (:inherit success))) t)
 '(org-pomodoro-mode-line-overtime ((t (:inherit error))) t)
 '(paradox-archive-face ((t (:inherit font-lock-doc-face))))
 '(paradox-description-face ((t (:inherit completions-annotations))))
 '(pulse-highlight-face ((t (:inherit region))))
 '(pulse-highlight-start-face ((t (:inherit region))))
 '(symbol-overlay-default-face ((t (:inherit (region bold)))))
 '(symbol-overlay-face-1 ((t (:background "#3c6d91" :foreground "#bbc2cf"))))
 '(symbol-overlay-face-2 ((t (:background "#68668a" :foreground "#bbc2cf"))))
 '(symbol-overlay-face-3 ((t (:background "#8a7557" :foreground "#bbc2cf"))))
 '(symbol-overlay-face-4 ((t (:background "#81583e" :foreground "#bbc2cf"))))
 '(symbol-overlay-face-5 ((t (:background "#934c4f" :foreground "#bbc2cf"))))
 '(symbol-overlay-face-6 ((t (:background "#775188" :foreground "#bbc2cf"))))
 '(symbol-overlay-face-7 ((t (:background "#60754c" :foreground "#bbc2cf"))))
 '(symbol-overlay-face-8 ((t (:background "#378299" :foreground "#bbc2cf"))))
 '(transient-posframe ((t (:inherit tooltip))))
 '(transient-posframe-border ((t (:inherit posframe-border))))
 '(which-key-posframe ((t (:inherit tooltip))))
 '(which-key-posframe-border ((t (:inherit posframe-border))))
 '(ztreep-arrow-face ((t (:inherit font-lock-comment-face))) t)
 '(ztreep-diff-header-face ((t (:inherit (diff-header bold)))) t)
 '(ztreep-diff-header-small-face ((t (:inherit diff-file-header))) t)
 '(ztreep-diff-model-add-face ((t (:inherit diff-nonexistent))) t)
 '(ztreep-diff-model-diff-face ((t (:inherit diff-removed))) t)
 '(ztreep-diff-model-ignored-face ((t (:inherit font-lock-doc-face :strike-through t))) t)
 '(ztreep-diff-model-normal-face ((t (:inherit font-lock-doc-face))) t)
 '(ztreep-expand-sign-face ((t (:inherit font-lock-function-name-face))) t)
 '(ztreep-header-face ((t (:inherit diff-header))) t)
 '(ztreep-leaf-face ((t (:inherit diff-index))) t)
 '(ztreep-node-face ((t (:inherit font-lock-variable-name-face))) t))

;;; custom.el ends here
