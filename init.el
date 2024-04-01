(require 'package)
(package-initialize)

;; active packages
(setq package-selected-packages '(haskell-mode))

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

;; set alternate modifier to option key on max
(setq ns-alternate-modifier 'meta)

;; backups to backups dir
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups/")))

;; auto-saves to backups dir
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/backups/\\1" t)))

;; lock files to backups dir
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/backups/\\1" t)))

;; disable alternate modifier on right option key
;; useful for keep using [,],@,{ and so on..
(setq ns-right-alternate-modifier 'none)

;; enables electric pair mode
(electric-pair-mode 1)

;; haskel ghcup path so that haskell mode can see it
(let ((my-ghcup-path (expand-file-name "~/.ghcup/bin")))
  (setenv "PATH" (concat my-ghcup-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-ghcup-path))

;; C settings
(setq c-default-style "linux"
      c-basic-offset 4)

