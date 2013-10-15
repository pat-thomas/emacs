;; --- Bring in Marmalade for package management.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" .
               "http://marmalade-repo.org/packages/"))
(package-initialize)

;; --- Packages to install.
;; (package-install 'clojure-mode)
;; (package-install 'paredit)
;; (package-install 'nrepl)

;; --- User specific settings.
(setq-default tab-width 2)
