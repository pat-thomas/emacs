(require 'cl)

;; --- Bring in Marmalade for package installation.
(defun load-marmalade ()
	(require 'package)
	
	;; --- Bring in MELPA for package installation.
	(add-to-list 'package-archives
							 '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
	(add-to-list 'package-archives
							 '("melpa" .
								 "http://melpa.milkbox.net/packages/") t)
	;;(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)
	(package-initialize))

;; --- Packages to install.
(defvar pthomas/packages '(align-cljlet
													 aggressive-indent
													 auto-complete
                           cider
                           clj-refactor
													 clojure-mode
                           color-theme-sanityinc-tomorrow
													 company
													 cyberpunk-theme
													 erlang
													 elixir-mode
													 go-mode
													 haskell-mode
                           paredit
                           rainbow-delimiters
													 rust-mode
													 smex
													 tagedit
													 ujelly-theme
													 yasnippet)
  "Default packages")

(defun pthomas/packages-installed-p ()
  (loop for pkg in pthomas/packages
	when (not (package-installed-p pkg)) do (return nil)
	finally (return t)))

(defun install-packages ()
	(unless (pthomas/packages-installed-p)
		(message "%s" "Refreshing package database...")
		(package-refresh-contents)
		(dolist (pkg pthomas/packages)
			(when (not (package-installed-p pkg))
				(package-install pkg)))))

(defun load-user-specific-misc-settings ()
	(setq tab-width 2
				indent-tabs-mode nil)
	(setq standard-indent 2)
	(setq-default tab-width 2)
	(setq inhibit-splash-screen t
				initial-scratch-message nil)
	(setq inhibit-startup-echo-area-message t)
	(load-theme 'ujelly t)
	(menu-bar-mode -1)
	(setq make-backup-files nil)
	(defalias 'yes-or-no-p 'y-or-n-p)
	(setq js-indent-level 2)
	(setq javascript-indent-level 2)
	(show-paren-mode 1)
	(global-hl-line-mode 1)
	;;(global-aggressive-indent-mode 1)
	(set-face-background hl-line-face "black20"))

(defun load-ido-mode ()
	(ido-mode t)
	(setq ido-enable-flex-matching t
				ido-use-virtual-buffers t)
	(ido-everywhere))

(defun load-lisp-mode-hooks ()
	(add-hook 'emacs-lisp-mode-hook 'paredit-mode)
	(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
	(add-hook 'lisp-mode-hook       'paredit-mode)
	(add-hook 'lisp-mode-hook       'rainbow-delimiters-mode)
	(add-hook 'scheme-mode-hook     'paredit-mode)
	(add-hook 'scheme-mode-hook     'rainbow-delimiters-mode))

(defun load-clojure-mode-hooks ()
	(add-hook 'clojure-mode-hook    'paredit-mode)
	(add-hook 'clojure-mode-hook    'rainbow-delimiters-mode)
	(add-hook 'cider-mode-hook      'paredit-mode)
	(add-hook 'cider-mode-hook      'rainbow-delimiters-mode)
	(add-hook 'cider-mode-hook      'company-mode) ;; also need to turn off auto-complete somehow...
	(add-hook 'cider-repl-mode-hook 'paredit-mode)
	(add-hook 'cider-repl-mode-hook 'rainbow-delimiters-mode)
	(add-hook 'cider-repl-mode-hook 'company-mode))

(defun load-ruby-file-extension-mode-mappings ()
	;; Ruby file extension/file name associations.
	(add-to-list 'auto-mode-alist '("\\.rake$"    . ruby-mode))
	(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
	(add-to-list 'auto-mode-alist '("\\.ru$"      . ruby-mode))
	(add-to-list 'auto-mode-alist '("Rakefile"    . ruby-mode))
	(add-to-list 'auto-mode-alist '("Gemfile"     . ruby-mode))
	(add-to-list 'auto-mode-alist '("Capfile"     . ruby-mode))
	(add-to-list 'auto-mode-alist '("Vagrantfile" . ruby-mode)))

(defun pt-revert-buffer ()
	(interactive)
	(revert-buffer t t)
	(cider-clear-compilation-highlights t))

(defun load-custom-keybindings ()
	;; join-line
	(global-set-key
	 (kbd "C-c j l")
	 'join-line)
	
	;; Lein deps.
	(global-set-key
	 (kbd "C-c l d")
	 (lambda ()
		 (interactive)
		 (shell-command "lein deps")))
	
	;; Go run.
	(global-set-key
	 (kbd "C-c g r")
	 (lambda ()
		 (interactive)
		 (shell-command (format "go run %s" (buffer-file-name)))))
	
	;; Better keybinding for paredit-forward-slurp-sexp:
	(add-hook
	 'paredit-mode-hook
	 '(lambda ()
			(local-set-key "\M-Oc" 'paredit-forward-slurp-sexp)))
	(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "M-)") 'paredit-forward-slurp-sexp))

	;; Keybinding for align-cljlet:
	(eval-after-load 'paredit '(define-key paredit-mode-map (kbd "C-c a") 'align-cljlet))

	;; Better keybinding for newline-and-indent:
	(global-set-key (kbd "RET") 'newline-and-indent)

	;; Better keybinding for comment-or-uncomment-region:
	(global-set-key (kbd "C-;") 'comment-or-uncomment-region)

	;; Always use Smex:
	(global-set-key (kbd "M-x") 'smex)

	;; Keybinding to revert buffer:
	(global-set-key [f5] 'pt-revert-buffer)
  (global-set-key [f9] 'join-line))

(defun compile-and-load-el-files-in-library (library-path)
  (dolist (file (directory-files (format "%s/" library-path) t "^[^#]+\\.el$"))
    (let ((cfile (format "%sc" file)))
      (byte-compile-file file)
			(load-file file))))

(defun load-custom-libraries ()
	(compile-and-load-el-files-in-library "~/.emacs.d/lib")
	(add-to-list 'load-path "~/.emacs.d/lib/pt-clojure.el")
	(add-to-list 'load-path "~/.emacs.d/lib/pt-yasnippet.el"))

(defun load-yasnippet ()
	(yas-global-mode 1))

(defun load-haskell-mode ()
	(defun bind-runhaskell ()
		(local-set-key (kbd "C-c C-k")
									 (lambda ()
										 (interactive)
										 (async-shell-command (concat "runhaskell " buffer-file-name)))))
  (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
  (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
	(add-hook 'haskell-mode-hook 'bind-runhaskell))

(defun load-tidal ()
	(when (file-exists-p "~/tidal/")
		(setq load-path (cons "~/tidal/" load-path))
		(require 'tidal)
		(setq tidal-interpreter "/usr/local/bin/ghci")))

(defun load-rust-mode-customizations ()
	(setq rust-indent-offset 2))

(defun load-company-mode-customizations ()
	(setq company-idle-delay .14)
	(setq company-selection-wrap-around t)
	(add-hook 'after-init-hook 'global-company-mode)
	(eval-after-load 'company
		'(progn
			 (define-key company-active-map (kbd "C-n") 'company-select-next)
			 (define-key company-active-map (kbd "C-p") 'company-select-previous)
			 (define-key company-active-map (kbd "TAB") 'company-complete-selection)
			 (define-key company-active-map [tab] 'company-select-next))))

(defun load-clojure-mode-custom-keybindings ()
	(add-hook
	 'clojure-mode-hook
	 (lambda ()
		 (local-set-key (kbd "C-c d a")
										(lambda ()
											(interactive)
											(pt-clj-fn-args-to-defs))))))

(defun load-clojure-mode-customizations ()
  (add-hook
    'clojure-mode-hook
    (lambda ()
      (clj-refactor-mode 1)
      (cljr-add-keybindings-with-prefix "C-c C-m")))
	(add-hook
	 'clojure-mode-hook
	 (lambda ()
     (put-clojure-indent 'ebb-fn 1)
     (put-clojure-indent 'uses-type 1)
     (put-clojure-indent 'state 1)
		 (put-clojure-indent 'defcomponent 1))))

(setq byte-compile-warnings nil)
(load-marmalade)
(install-packages)
(load-custom-libraries)
(load-user-specific-misc-settings)
(load-custom-keybindings)
(load-ido-mode)
(load-lisp-mode-hooks)
(load-clojure-mode-hooks)
(load-clojure-mode-custom-keybindings)
(load-clojure-mode-customizations)
(load-ruby-file-extension-mode-mappings)
(load-custom-keybindings)
(load-yasnippet)
(load-haskell-mode)
(load-tidal)
(load-rust-mode-customizations)
(load-company-mode-customizations)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
	 [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
	 (vector "#ffffff" "#ff9da4" "#d1f1a9" "#ffeead" "#bbdaff" "#ebbbff" "#99ffff" "#003f8e"))
 '(custom-enabled-themes (quote (sanityinc-tomorrow-night)))
 '(custom-safe-themes
	 (quote
		("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" default)))
 '(fci-rule-color "#003f8e")
 '(package-selected-packages
	 (quote
		(color-theme-sanityinc-tomorrow ujelly-theme tagedit smex rust-mode rainbow-delimiters haskell-mode go-mode erlang elixir-mode cyberpunk-theme company clj-refactor auto-complete align-cljlet aggressive-indent)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
	 (quote
		((20 . "#ff9da4")
		 (40 . "#ffc58f")
		 (60 . "#ffeead")
		 (80 . "#d1f1a9")
		 (100 . "#99ffff")
		 (120 . "#bbdaff")
		 (140 . "#ebbbff")
		 (160 . "#ff9da4")
		 (180 . "#ffc58f")
		 (200 . "#ffeead")
		 (220 . "#d1f1a9")
		 (240 . "#99ffff")
		 (260 . "#bbdaff")
		 (280 . "#ebbbff")
		 (300 . "#ff9da4")
		 (320 . "#ffc58f")
		 (340 . "#ffeead")
		 (360 . "#d1f1a9"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
