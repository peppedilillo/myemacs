;; use-package
(require 'use-package)

;; MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; adds custome themes  and loads one
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'gruber-darker t)

;; sets font
(set-face-attribute 'default nil :font "JetBrains Mono-13" :weight `extralight)
(setq-default line-spacing 0.2)

;; disables context menubar, toolbar, scrollbar
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

;; load custom splash screen
(load "~/.emacs.d/loads/splash-screen.el")
(require 'splash-screen)
;; or if you grow tired, this disables the splash screen
;; (setq inhibit-startup-screen t)

;; switch audio bell for visual one
(setq visible-bell nil
      ring-bell-function 'double-flash-mode-line)
(defun double-flash-mode-line ()
  (let ((flash-sec (/ 1.0 20)))
    (invert-face 'mode-line)
    (run-with-timer flash-sec nil #'invert-face 'mode-line)
    (run-with-timer (* 2 flash-sec) nil #'invert-face 'mode-line)
    (run-with-timer (* 3 flash-sec) nil #'invert-face 'mode-line)))

;; all themes are considered safe
(setq custom-safe-themes t)

;; pixel-scroll-precision-mode was not available prior to emacs 29
(if (not (version< emacs-version "29"))
  (pixel-scroll-precision-mode 1) nil)

;: fuck-offs custom variables to separate file
(setq custom-file (concat user-emacs-directory "/custom.el"))

;; backups to backups dir
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups/")))

;; auto-saves to backups dir
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/backups/\\1" t)))

;; lock files to backups dir
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/backups/\\1" t)))

;; cua mode
(cua-mode t)

;; enables electric pair mode
(electric-pair-mode 1)

;; default tab length
(setq-default tab-width 4)

;; automatically enables line numbers in prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; company completion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; typographic ligatures.
(defconst jetbrains-ligatures
   '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
     "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
     "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
     "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
     "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
     "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
     "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
     "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
     ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
     "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
     "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
     "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
     "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
     "&="))

(use-package ligature
  :ensure t
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable traditional ligature support in eww-mode, if the
  ;; `variable-pitch' face supports it
  (ligature-set-ligatures 'eww-mode '("ff" "fi" "ffi"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures 'prog-mode jetbrains-ligatures)
  ;; Enables ligature checks globally in all buffers.
  (global-ligature-mode t))

;; olivetti write mode
(use-package olivetti
  :ensure t)

;; loads machine-specific init file, if present
(defvar mac-custom-file "~/.emacs.d/init_mac.el")
(defvar deb-custom-file "~/.emacs.d/init_deb.el")

(cond
 ((eq system-type 'darwin)
  (if (file-exists-p mac-custom-file)
    (load mac-custom-file)))
 ((eq system-type 'gnu/linux)
  (if (file-exists-p deb-custom-file)
      (load deb-custom-file))))


