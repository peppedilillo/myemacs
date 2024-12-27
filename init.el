;; use-package
(require 'use-package)

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; adds custome themes  and loads one
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'gruber-darker t)

;; all themes are considered safe
(setq custom-safe-themes t)

;; sets font specs
(set-face-attribute 'default nil :height 130)

;; disables context menubar, toolbar, scrollbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; pixel level smooth scrolling
(pixel-scroll-precision-mode 1)

;; cua mode
(cua-mode t)

;: fuck-offs custom variables to separate file
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; switch audio bell for visual one
(setq visible-bell nil
      ring-bell-function 'double-flash-mode-line)
(defun double-flash-mode-line ()
  (let ((flash-sec (/ 1.0 20)))
    (invert-face 'mode-line)
    (run-with-timer flash-sec nil #'invert-face 'mode-line)
    (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
    (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))

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

;; enables electric pair mode
(electric-pair-mode 1)

;; default tab length
(setq-default tab-width 4)

;; automatically enables line numbers in prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; olivetti write mode
(use-package olivetti
  :ensure t)

;; company completion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; loads machine-specific init file, if present
(defvar mac-custom-file "~/.emacs.d/init_mac.el")
(when (file-exists-p mac-custom-file) (load mac-custom-file))

