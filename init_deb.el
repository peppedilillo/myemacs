;; LSPs
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio"))))

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
