;; --- Bring in Marmalade for package installation.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
;; --- Bring in MELPA for package installation.
(add-to-list 'package-archives
						 '("melpa" .
							 "http://melpa.milkbox.net/packages/") t)
(package-initialize)


;; --- Packages to install.
;; (package-install 'clojure-mode)
;; (package-install 'paredit)
;; (package-install 'nrepl)
;; (package-install 'cyberpunk-theme)
;; (package-install 'rainbow-delimiters)
;; (package-install 'auto-complete)

;; --- User specific settings.
(setq-default tab-width 2)
(setq inhibit-splash-screen t)
(setq inhibit-startup-echo-area-message t)
(load-theme 'cyberpunk t)
(menu-bar-mode -1)


;; --- Mode hooks.
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimeters-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimeters-mode)
