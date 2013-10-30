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
(defvar pthomas/packages '(auto-complete
			   clojure-mode
			   cyberpunk-theme
			   go-mode
			   nrepl
                           paredit
                           rainbow-delimiters)
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
(setq standard-indent 2)
(setq-default tab-width 2)
(setq inhibit-splash-screen t
      initial-scratch-message nil)
(setq inhibit-startup-echo-area-message t)
(load-theme 'cyberpunk t)
(menu-bar-mode -1)
(setq make-backup-files nil)
(defalias 'yes-or-no-p 'y-or-n-p)
(setq js-indent-level 2)
(setq javascript-indent-level 2)


;; --- Keybindings.
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)


;; --- Autocomplete.
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)


;; --- Global major modes
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)


;; --- Mode hooks.
(add-hook 'clojure-mode-hook    'paredit-mode)
(add-hook 'clojure-mode-hook    'rainbow-delimiters-mode)
(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'nrepl-mode-hook      'paredit-mode)
(add-hook 'nrepl-mode-hook      'rainbow-delimiters-mode)
(add-hook 'nrepl-repl-mode-hook 'paredit-mode)
(add-hook 'nrepl-repl-mode-hook 'rainbow-delimiters-mode)


;; Ruby file extension/file name associations.
(add-to-list 'auto-mode-alist '("\\.rake$"    . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$"      . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile"    . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile"     . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile"     . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode))



;; --- Custom keybindings. ---
;; Lein deps.
(global-set-key
 (kbd "C-c l d")
 (lambda ()
	 (interactive)
	 (shell-command "lein deps")))
