;; use-package
(require 'use-package)

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; adds custome themes  and loads one
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'gruvbox-dark t)

;; all themes are considered safe
(setq custom-safe-themes t)

;; aesthetics
(set-face-attribute 'default nil :font "Iosevka-15")

;; disable context menu on startup
(tool-bar-mode -1)

;; load custom splash screen
(load "~/.emacs.d/loads/splash-screen.el")
(require 'splash-screen)
;; or if you grow tired, this disables the splash screen
;; (setq inhibit-startup-screen t)

;; backups to backups dir
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups/")))

;; auto-saves to backups dir
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/backups/\\1" t)))

;; lock files to backups dir
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/backups/\\1" t)))

;; set alternate modifier to option key on max
(setq ns-alternate-modifier 'meta)

;; disable alternate modifier on right option key
;; useful for keep using on mac [,],@,{ and so on..
(setq ns-right-alternate-modifier 'none)

;; enables electric pair mode
(electric-pair-mode 1)

;; C settings
(setq c-default-style "linux"
      c-basic-offset 4)

;; haskell
(use-package haskell-mode
  :init
  ;; haskel ghcup path so that haskell mode can see it
  (let ((my-ghcup-path (expand-file-name "~/.ghcup/bin")))
    (setenv "PATH" (concat my-ghcup-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-ghcup-path)))

;; corfu autocompletion
(use-package corfu
  :ensure t
  :custom
  (corfu-auto t)
  (corfu-auto-prefix 2)
  :init
  (global-corfu-mode))

;; cape autocompletion
(use-package cape
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block))

