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


;; --- User specific settings.
(setq-default tab-width 2)
(setq inhibit-splash-screen t)
(load-theme 'cyberpunk t)
(menu-bar-mode -1)

