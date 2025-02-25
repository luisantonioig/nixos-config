(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'elegant-black t)

;; TODO: Quitar esto despues de provar las capacidades de aiken lsp
(setq lsp-log-io t)

;; Graphical user interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; lsp-mode configuration
(require 'lsp-mode)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'tsx-mode-hook #'lsp)

;; lsp-ui-mode configuration
(require 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)

;; tree-sitter-mode configuration
(add-hook 'typescript-mode-hook #'tree-sitter-mode)
(add-hook 'tsx-mode-hook #'tree-sitter-mode)
(setq major-mode-remap-alist
      '((typescript-mode . typescript-ts-mode)
        (tsx-mode . tsx-ts-mode)))

(add-hook 'json-mode-hook (lambda () (setq js-indent-level 2)))
(setq js-indent-level 2)


;; TODO: make aiken-mode to work
(use-package aiken-mode
  :load-path "~/personal/aiken-mode/")

(setq inhibit-startup-screen t)
(setq initial-buffer-choice nil)
(setq inhibit-startup-echo-area-message t)

;; Configuration for haskell-mode
;; (use-package haskell-mode
;;   :ensure t
;;   ;; :hook (haskell-mode . interactive-haskell-mode)
;;   :config
;;   (setq haskell-process-type 'ghci)
;;   (setq haskell-process-log t))

(use-package lsp-haskell
  :ensure t
  :hook (haskell-mode . lsp)
  :config
  (setq lsp-haskell-server-path "haskell-language-server")
  (setq lsp-haskell-server-args '("-d")))

(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp)

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)


;; Configuration for dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 10) ;; Mostrar 10 archivos recientes
                          (bookmarks . 5) ;; Mostrar 5 marcadores
                          (projects  . 5) ;; Mostrar 5 proyectos recientes
                          (agenda    . 5))) ;; Mostrar próximos eventos del calendario

  (setq dashboard-banner-logo-title "Bienvenido a Emacs 🚀")
  (setq dashboard-startup-banner 'official) ;; Usa el logo oficial de Emacs
  (setq dashboard-center-content t) ;; Usa el logo oficial de Emacs
  (setq dashboard-set-heading-icons t) ;; Agrega iconos a las secciones
  (setq dashboard-set-file-icons t)) ;; Usa iconos en la lista de archivos recientes

;; Configuration for projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/iog" "~/personal")) ;; Ajusta a tus rutas
  :config
  (projectile-mode +1)
  (setq projectile-completion-system 'default)
  (setq projectile-sort-order 'recentf) ;; Proyectos recientes primero
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))
;; Added from home.nix
;; Added with source
