;; set alternate modifier to option key on max
(setq ns-alternate-modifier 'meta)

;; disable alternate modifier on right option key
;; useful for keep using on mac [,],@,{ and so on..
(setq ns-right-alternate-modifier 'none)

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
(setq c-default-style "linux" c-basic-offset 4)

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
