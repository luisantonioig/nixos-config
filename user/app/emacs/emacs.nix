{ config, pkgs, ... }:
{   
  # # Instalar Emacs y configuraciones básicas
  programs.home-manager.enable = true;
  programs.emacs = {
    enable = true;
    package = pkgs.emacs29.override {
      withTreeSitter = true;
    };
    extraPackages = epkgs: [
      epkgs.magit
	    epkgs.company
	    epkgs.company-quickhelp
      # epkgs.treesit-grammars.with-all-grammars
	    epkgs.nix-mode
      epkgs.haskell-mode
      epkgs.lsp-haskell
      epkgs.lsp-mode
      epkgs.lsp-ui
	    epkgs.json-mode
      epkgs.consult
      epkgs.beacon
      epkgs.flycheck
      epkgs.projectile
      epkgs.doom-modeline
      epkgs.nerd-icons
      epkgs.dashboard
      epkgs.typescript-mode
      epkgs.tree-sitter
      # epkgs.tree-sitter-langs
      epkgs.treesit-grammars.with-all-grammars
      epkgs.treesit-auto
    ];
  };
  
  home.file.".emacs".text = ''
    (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
    (load-theme 'elegant-black t)

    ;; Graphical user interface
    (menu-bar-mode -1)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)

    (global-display-line-numbers-mode 1)
    (global-visual-line-mode t)

    ;; TODO: Make treesitter and typescript to work

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

    ;; (add-to-list 'load-path "~/.emacs.d/site-lisp/typescript")

    ;; (require 'typescript)

    ;; (progn
    ;;   ;; load elpa package system
    ;;   ;; require emacs 24
    ;;   (require 'package)
    ;;   ;; Add MELPA repository.
    ;;   (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

    ;;   (when (< emacs-major-version 27) (package-initialize))
    ;;   ;;
    ;;   )

    ;; (setq treesit-extra-load-path '("~/.emacs.d/tree-sitter/typescript"))

    ;; (use-package treesit
    ;;   :ensure nil ;; Viene con Emacs 29+
    ;;   :config
    ;;   (setq major-mode-remap-alist
    ;;         '((typescript-mode . typescript-ts-mode)
    ;;           (tsx-mode . tsx-ts-mode))))

    ;; (use-package treesit-auto
    ;;   :ensure t
    ;;   :config
    ;;   (global-treesit-auto-mode))
    ;; (custom-set-variables
    ;;  ;; custom-set-variables was added by Custom.
    ;;  ;; If you edit it by hand, you could mess it up, so be careful.
    ;;  ;; Your init file should contain only one such instance.
    ;;  ;; If there is more than one, they won't work right.
    ;;  '(package-selected-packages
    ;;    '(dashboard typescript-mode web-mode company-quickhelp treesit-auto tree-sitter-langs tide rjsx-mode prettier-js nodejs-repl company)))
    ;; (custom-set-faces
    ;;  ;; custom-set-faces was added by Custom.
    ;;  ;; If you edit it by hand, you could mess it up, so be careful.
    ;;  ;; Your init file should contain only one such instance.
    ;;  ;; If there is more than one, they won't work right.
    ;;  )

    (add-hook 'json-mode-hook (lambda () (setq js-indent-level 2)))
    (setq js-indent-level 2)

    ;; (add-to-list 'treesit-extra-load-path "~/.emacs.d/tree-sitter/")

    ;; TODO: make aiken-mode to work
    ;; (use-package aiken-mode
    ;;   :load-path "~/personal/aiken-mode/")

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
      (setq dashboard-set-heading-icons t) ;; Agrega iconos a las secciones
      (setq dashboard-set-file-icons t)) ;; Usa iconos en la lista de archivos recientes
      ;; Added from home.nix
  '';
}
