;; package setup
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(require 'use-package)

;; separate custom-file output
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror 'nomessage)

;; custom themes
(add-to-list 'custom-theme-load-path
             (expand-file-name "themes/" user-emacs-directory))

;; these GUI handlers are somewhat obscure but needed for daemon
;; settings applied to every graphical frame
(add-to-list 'default-frame-alist
             '(font . "JetBrains Mono-13"))

(defun my-gui-frame-setup (frame)
  (with-selected-frame frame
    (when (display-graphic-p frame)
      (unless (custom-theme-enabled-p 'gruvbox-dark)
        (load-theme 'gruvbox-dark t)))))

(add-hook 'after-make-frame-functions #'my-gui-frame-setup)

(unless (daemonp)
  (my-gui-frame-setup (selected-frame)))

;; UI
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; fuck off  auto-revert
(global-auto-revert-mode -1)

;; load custom splash screen if not in daemon
(unless (daemonp)
  (load "~/.emacs.d/loads/splash-screen.el")
  (require 'splash-screen))
;; or if you grow tired, this disables all splash screen
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

;; backups to backups dir
(make-directory
     (expand-file-name "backups/" user-emacs-directory) t)
(setq backup-directory-alist
      '(("." . "~/.emacs.d/backups/")))

;; auto-saves to backups dir
(setq auto-save-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/backups/\\1" t)))

;; lock files to backups dir
(setq lock-file-name-transforms
      '(("\\`/.*/\\([^/]+\\)\\'" "~/.emacs.d/backups/\\1" t)))

;; cua mode
;; (cua-mode t)

;; enables electric pair mode
(electric-pair-mode 1)

;; enables paste and replace selection
(delete-selection-mode 1)

;; default tab length
(setq-default tab-width 4)
;; only use space for indentation
(setq-default indent-tabs-mode nil)

;; automatically enables line numbers in prog mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)

;; company completion
(use-package company
  :ensure t
  :hook (after-init . global-company-mode))

;; powerline
(use-package powerline
  :ensure t)

(use-package spaceline
  :ensure t
  :after powerline
  :config
  (spaceline-emacs-theme))

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

;; docker
(use-package dockerfile-mode
  :ensure t)

;; rainbow delimiters makes nested delimiters easier to understand
(use-package rainbow-delimiters
    :ensure t
    :hook ((prog-mode . rainbow-delimiters-mode)))

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

