(require 'package)
(package-initialize)

;; active packages
(setq package-selected-packages '(haskell-mode doom-themes))

;; aesthetics
(load-theme 'doom-manegarm t)
(set-face-attribute 'default nil :font "Monaco-14")

;; disable context menu on startup
(tool-bar-mode -1)

;; load custom splash screen
(load "~/.emacs.d/loads/splash-screen.el")
(require 'splash-screen)

;; or if you grow tired, this disables the splash screen
;; (setq inhibit-startup-screen t)

;; adds custome themes from my folder
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; all themes are considered safe
(setq custom-safe-themes t)

;; set alternate modifier to option key on max
(setq ns-alternate-modifier 'meta)

;; save backup autosave file to .emacs.d folder copying them
(setq backup-directory-alist `(("." . "~/.emacs.d/saves")))
(setq backup-by-copying t)

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

