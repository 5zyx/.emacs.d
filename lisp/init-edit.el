;; init-edit.el --- Initialize edit configurations.
;;
;; Author: Vincent Zhang <seagle0128@gmail.com>
;; Version: 2.0.0
;; URL: https://github.com/seagle0128/.emacs.d
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;             Edit configurations.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

;; Miscs
;; (setq initial-scratch-message nil)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*")
(setq adaptive-fill-first-line-regexp "^* *$")
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder
(setq make-backup-files nil)               ; Forbide to make backup files
(setq auto-save-default nil)               ; Disable auto save
;; (setq-default kill-whole-line t)           ; Kill line including '\n'

(setq-default major-mode 'text-mode)
(add-hook 'text-mode-hook
          '(lambda ()
             (turn-on-auto-fill)
             (diminish 'auto-fill-function)))

(setq sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*")
(setq sentence-end-double-space nil)

(add-hook 'abbrev-mode-hook '(lambda () (diminish 'abbrev-mode)))

;; Tab and Space
;; Permanently indent with spaces, never with TABs
(setq-default c-basic-offset   4
              tab-width        4
              indent-tabs-mode nil)

;; Encoding
(add-hook 'after-init-hook
          '(lambda ()
             (set-language-environment 'Chinese-GB18030)
             (set-clipboard-coding-system 'chinese-gb18030)
             (set-keyboard-coding-system 'utf-8)
             (set-terminal-coding-system 'utf-8)
             (set-buffer-file-coding-system 'utf-8)
             (set-default-coding-systems 'utf-8)
             (set-selection-coding-system 'utf-8)
             (modify-coding-system-alist 'process "*" 'utf-8)
             (setq default-process-coding-system '(utf-8 . utf-8))
             (set-file-name-coding-system 'utf-8)
             (prefer-coding-system 'utf-8)))

;; Delete selection if you insert
(use-package delsel
  :init (add-hook 'after-init-hook 'delete-selection-mode))

;; Rectangle
;; for rectangles, CUA is nice
(use-package cua-rect
  :ensure cua-base
  :bind (("<C-return>" . cua-rectangle-mark-mode)))

;; Automatically reload files was modified by external program
(use-package autorevert
  :diminish auto-revert-mode
  :init (add-hook 'after-init-hook 'global-auto-revert-mode))

;; Tree-based completion
(use-package avy
  :bind (("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)
         ("M-g e" . avy-goto-word-0))
  :config (avy-setup-default))

;; Kill text between the point and the character CHAR
(use-package zzz-to-char
  :bind (("M-z" . zzz-to-char)
         ("C-M-z" . zzz-up-to-char)))

;; Quickly follow links
(use-package ace-link
  :init (add-hook 'after-init-hook 'ace-link-setup-default))

;; Minor mode to aggressively keep your code always indented
(use-package aggressive-indent
  :diminish aggressive-indent-mode
  :init (add-hook 'after-init-hook 'global-aggressive-indent-mode)
  :config (dolist (mode '(ruby-mode robot-mode web-mode html-mode css-mode))
            (push mode aggressive-indent-excluded-modes)))

;; Show number of matches in mode-line while searching
(use-package anzu
  :diminish anzu-mode
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp))
  :init (add-hook 'after-init-hook 'global-anzu-mode))

;; Increase selected region by semantic units
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Move to the beginning/end of line or code
(use-package mwim
  :bind (("C-a" . mwim-beginning-of-code-or-line)
         ("C-e" . mwim-end-of-code-or-line)))

;; Windows-scroll commands
(use-package pager
  :pin melpa
  :commands pager-page-down pager-page-up pager-row-down pager-row-up
  :bind (("\C-v"   . pager-page-down)
         ([next]   . pager-page-down)
         ("\ev"    . pager-page-up)
         ([prior]  . pager-page-up)
         ([M-up]   . pager-row-up)
         ([M-kp-8] . pager-row-up)
         ([M-down] . pager-row-down)
         ([M-kp-2] . pager-row-down)))

;; Drag stuff (lines, words, region, etc...) around
(use-package drag-stuff
  :diminish drag-stuff-mode
  :init (add-hook 'after-init-hook 'drag-stuff-global-mode))

;; An all-in-one comment command to rule them all
(use-package comment-dwim-2
  :bind ("M-;" . comment-dwim-2))

;; Edit multiple regions in the same way simultaneously
(use-package iedit
  :bind (("C-;" . iedit-mode)
         ("C-x r RET" . iedit-rectangle-mode)))

;; Multiple cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<". mc/mark-previous-like-this)
         ("C-c C-<". mc/mark-all-like-this)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

;; Treat undo history as a tree
(use-package undo-tree
  :pin gnu
  :diminish undo-tree-mode
  :init (add-hook 'after-init-hook 'global-undo-tree-mode))

;; Handling capitalized subwords in a nomenclature
(use-package subword
  :diminish subword-mode
  :init (add-hook 'prog-mode-hook 'subword-mode))

(provide 'init-edit)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-edit.el ends here
