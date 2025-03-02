(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'elegant-black t)

;; TODO @luisantonioig: Quitar esto despues de provar las capacidades de aiken lsp
(setq lsp-log-io t)

;; Graphical user interface
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable intant-tabs-mode
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)


(global-display-line-numbers-mode 1)
(global-visual-line-mode t)

;; typescript-mode and tsx-mode configuration
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts\\'" "\\.tsx\\'"))

;; TODO @luisantonioig: What can I do with tree-sitter??
;; tree-sitter-mode configuration
;; (add-hook 'typescript-mode-hook #'tree-sitter-mode)
;; (add-hook 'tsx-mode-hook #'tree-sitter-mode)
;; (setq major-mode-remap-alist
;;       '((typescript-mode . typescript-ts-mode)
;;         (tsx-mode . tsx-ts-mode)))

(use-package web-mode
  :mode ("\\.js\\'" "\\.jsx\\'")
  :hook ((web-mode . (lambda ()
                       (setq indent-tabs-mode nil)
                       (setq web-mode-markup-indent-offset 2)
                       (setq web-mode-css-indent-offset 2)
                       (setq web-mode-code-indent-offset 2))))
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)))

(use-package lsp-mode
  :hook ((typescript-mode . lsp))
  :commands lsp
  :config
  (setq lsp-clients-typescript-init-options '(hostInfo "Emacs"))
  (setq lsp-eslint-auto-fix-on-save t)
  (setq lsp-disabled-clients '()))

;; lsp-mode configuration
(require 'lsp-mode)
(add-hook 'typescript-mode-hook #'lsp)
(add-hook 'tsx-mode-hook #'lsp)

;; lsp-ui-mode configuration
(require 'lsp-ui)
(add-hook 'lsp-mode-hook #'lsp-ui-mode)

(add-hook 'json-mode-hook (lambda () (setq js-indent-level 2)))
(setq js-indent-level 2)


;; TODO @luisantonioig: make aiken-mode to work
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

;; Configuration for projectile
(use-package projectile
  :ensure t
  :init
  (setq projectile-project-search-path '("~/iog" "~/personal")) ;; Ajusta a tus rutas
  (setq projectule-auto-discover t)
  (setq projectile-enable-caching t)
  :config
  (projectile-mode +1)
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-enable-caching t)
  (setq projectile-completion-system 'default)
  (setq projectile-sort-order 'recentf)
  (setq projectile-generic-command "rg --files --hidden --glob '!**node_modules**'")
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

;; Configuration for dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 10) ;; Mostrar 10 archivos recientes
                          (bookmarks . 5) ;; Mostrar 5 marcadores
                          (projects  . 5) ;; Mostrar 5 proyectos recientes
                          (agenda    . 5))) ;; Mostrar próximos eventos del calendario
  (setq dashboard-projects-backend 'projectile)
  (setq dashboard-banner-logo-title "Bienvenido a Emacs 🚀")
  (setq dashboard-startup-banner 'official) ;; Usa el logo oficial de Emacs
  (setq dashboard-center-content t) ;; Usa el logo oficial de Emacs
  (setq dashboard-set-heading-icons t) ;; Agrega iconos a las secciones
  (setq dashboard-set-file-icons t)) ;; Usa iconos en la lista de archivos recientes

;; Search stack
(use-package vertico
  :ensure t
  :init
  (vertico-mode 1))

(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)
	completion-category-defaults nil
	completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :ensure t
  :bind (("C-s" . consult-line)
	("C-x b" . consult-buffer)
	("M-y" . consult-yank-pop))
  :init
  (setq register-preview-delay 0.5
	register-preview-function #'consult-register-preview)
  :hook (completion-list-mode . consult-preview-at-point-mode))

;; Command to assign tasks to myself
(defun insert-todo-luisantonioig ()
  "Inserts a TODO comment using the format of the current mode"
  (interactive)
  (let ((todo-text (concat (or comment-start "//") " TODO @luisantonioig:")))
    (insert todo-text)))

(global-set-key (kbd "C-c t") 'insert-todo-luisantonioig)

;; magit projects same as projectile projects,
(setq magit-repository-directories
      (mapcar (lambda (dir) (cons dir 0)) projectile-known-projects))
