;; CL (Common Lisp) mode for additional functionality.
(require 'cl)

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
(defvar pthomas/packages '(clojure-mode
			   paredit
			   nrepl
			   cyberpunk-theme
			   rainbow-delimiters
			   auto-complete)
  "Default packages")

(defun pthomas/packages-installed-p ()
  (loop for pkg in pthomas/packages
				when (not (package-installed-p pkg)) do (return nil)
				finally (return t)))

(unless (pthomas/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg pthomas/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))


;; --- User specific settings.
(setq tab-width 2
      indent-tabs-mode nil)
(setq inhibit-splash-screen t
      initial-scratch-message nil)
(setq inhibit-startup-echo-area-message t)
(load-theme 'cyberpunk t)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)


;; Autocomplete
(require 'auto-complete-config)
(ac-config-default)


;; --- Global major modes
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)


;; --- Mode hooks.
(add-hook 'clojure-mode-hook 'paredit-mode)
(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
