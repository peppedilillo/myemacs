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

(load (expand-file-name "inits/init_python.el" user-emacs-directory))
(load (expand-file-name "inits/init_c.el" user-emacs-directory))
(load (expand-file-name "inits/init_haskell.el" user-emacs-directory))
(load (expand-file-name "inits/init_rust.el" user-emacs-directory))
(load (expand-file-name "inits/init_swift.el" user-emacs-directory))
(load (expand-file-name "inits/init_glsl.el" user-emacs-directory))

