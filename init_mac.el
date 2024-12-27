;; system font
(set-face-attribute 'default nil :font "JetBrains Mono-13" :weight `extralight)
(setq-default line-spacing 0.2)

;; haskell
(use-package haskell-mode
  ;; only loads if haskell ghcup is available
  :if (file-directory-p "~/.ghcup/bin")
  :init
  ;; haskel ghcup path so that haskell mode can see it
  (let ((my-ghcup-path (expand-file-name "~/.ghcup/bin")))
    (setenv "PATH" (concat my-ghcup-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-ghcup-path)))

;; rust
(use-package rust-mode
  :ensure t)
