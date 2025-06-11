;; font choice on mac
(set-face-attribute 'default nil :font "JetBrains Mono-13" :weight `light)

;; disable zooming through trackpad
(global-set-key [C-wheel-up] 'ignore)
(global-set-key [C-wheel-down] 'ignore)

;; set alternate modifier to option key on max
(setq ns-alternate-modifier 'meta)

;; disable alternate modifier on right option key
;; useful for keep using on mac [,],@,{ and so on..
(setq ns-right-alternate-modifier 'none)

;; LSPs
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(java-mode . ("jdtls")))
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))
  (add-to-list 'eglot-server-programs '(swift-mode . ("sourcekit-lsp")))
  )

;; python
(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode 1))

(use-package highlight-indent-guides
  :ensure t
  :config
  (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

;; C
(setq-default c-default-style "linux"
              c-basic-offset 4)
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

;; swift
(use-package swift-mode
    :ensure t
    :mode "\\.swift\\'"
    :interpreter "swift")
