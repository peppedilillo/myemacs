;; debian 12 does not support emacs 29 which ships eglot
(use-package eglot
  :ensure t)

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
