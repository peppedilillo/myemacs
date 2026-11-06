;; sets font for a big lo-res screen
(set-face-attribute 'default nil :font "JetBrains Mono-12")

;; LSPs
(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio"))))


(load (expand-file-name "inits/init_c.el" user-emacs-directory))
(load (expand-file-name "inits/init_python.el" user-emacs-directory))
(load (expand-file-name "inits/init_glsl.el" user-emacs-directory))
(load (expand-file-name "inits/init_sicp.el" user-emacs-directory))
