;;; init.el --- Emacs configuration of Kristiyan Kanchev

;;; Commentary:
;; Emacs configuration of Kristiyan Kanchev

;;; Code:

;; ------------------------
;; Emacs Customization file
(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ------------
;; OS-dependent

;; Append MSYS2 to PATH on Windows
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  (setenv "PATH" (concat "C:\\msys64\\mingw64\\bin;" (getenv "PATH")))
  (setq exec-path (append '("C:\\msys64\\mingw64\\bin") exec-path))

  (setenv "PATH" (concat "C:\\msys64\\usr\\bin;" (getenv "PATH")))
  (setq exec-path (append '("C:\\msys64\\usr\\bin") exec-path))

  ;; Fix 'find' listing on Windows
  (setq find-ls-option '("-exec ls -ldh {} +" . "-ldh")))

;; Append Homebrew bin dir to exec-path on OSX
(when (eq system-type 'darwin)
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (setq exec-path (append '("/usr/local/bin") exec-path)))

;; -------
;; Adding local-configs to load-path
(add-to-list 'load-path (file-name-as-directory
			 (expand-file-name "local-configs"
					   user-emacs-directory)))

;; ---------
;; Built-ins

;; Config for built-ins
(setq column-number-mode t)           ; show column number in modeline
(global-linum-mode t)                 ; display line numbers
(setq scroll-conservatively 10000)    ; allow to scrol line-by-line

;; (ido-mode t)                          ; enable IDO mode
;; (ido-everywhere)		      ; enable IDO everywhere (?)

(add-hook 'before-save-hook
	  'delete-trailing-whitespace)	       ; delete whitespaces
(put 'dired-find-alternate-file 'disabled nil) ; reuse dired buffers

;; Shows current function name
(which-function-mode t)

;; find-grep-dired to not recurse in .svn folder
;; TODO: make it more general
(setq find-grep-options "-Iq --exclude=\"*\\.svn*\"")

;; Don't ask newbie questions
(defalias 'yes-or-no-p 'y-or-n-p)

;; Prevent accidental exiting
(setq confirm-kill-emacs 'y-or-n-p)

;; Email config
(setq send-mail-function    'smtpmail-send-it
      user-mail-address     "skrechy@gmail.com"
      smtpmail-smtp-server  "smtp.gmail.com"
      smtpmail-stream-type  'ssl
      smtpmail-smtp-service 465)

;; Keys for built-in
(global-set-key (kbd "C-x C-b") 'bs-show)

;; ----
;; ELPA

;; Init ELPA
(setq package-enable-at-startup nil)    ; Do not init packages after init file
(package-initialize)                    ; Init ELPA packages

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)		; refresh packages cache
  (package-install 'use-package))

;; ------------------------------
;; Packages init with use-package

;; +++
;; Always-on

;; Themes
(use-package zenburn-theme
  :ensure t)

(use-package color-theme-sanityinc-tomorrow
  :ensure t
  :init
  (load-theme 'sanityinc-tomorrow-eighties t))

;; Helm
(use-package helm
  :ensure t
  :init
  (require 'helm-config)		; adds C-x c prefix
  (helm-mode 1)				; start helm mode

  :bind (("M-x" . helm-M-x)
	 ("C-x C-f" . helm-find-files)
	 ("C-x C-b" . helm-buffers-list)
	 ("C-x b" . helm-mini)
	 ("M-y" . helm-show-kill-ring)
	 ("M-s o" . helm-occur)

	 :map helm-map
	 ("<tab>" . helm-execute-persistent-action) ; swap <tab> and C-z
	 ("C-z" . helm-select-action))

  :config
  (setq helm-split-window-in-side-p t) ; helm window in current window
  (setq helm-ff-skip-boring-files t)   ; don't show eg. temp files and vc
  :diminish helm-mode)

;; Projectile
(use-package projectile
  :ensure t
  :init (projectile-mode)
  :config
  (setq projectile-indexing-method 'native
	projectile-globally-ignored-directories
	(append '("*__pycache__/" "__pycache__" "*pycache/")
		projectile-globally-ignored-directories)))

;; Helm-Projectile
(use-package helm-projectile
  :ensure t
  :if (and (featurep 'helm) (featurep 'projectile))
  :init (helm-projectile-on))

(use-package yasnippet
  :ensure t
  :init (yas-global-mode)
  :diminish yas-minor-mode)

;; Smex
;; (use-package smex
;;  :ensure t
;;  :bind ("M-x" . smex))

;; IDO Ubiquitous
;; (use-package ido-ubiquitous
;;   :ensure t
;;   :init (ido-ubiquitous-mode))

;; Which key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish which-key-mode)

;; Company-mode
(use-package company
  :ensure t
  :init (global-company-mode)
  :diminish company-mode)

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

;; Flycheck pos-tip
(use-package flycheck-pos-tip
  :ensure t
  :init (with-eval-after-load 'flycheck
	  (flycheck-pos-tip-mode)))

;; +++
;; Deffered

;; Subword -- allows to move on sub-word in CamelCase
(use-package subword
  :defer t
  :init
  (progn
    ;; Enable for Python mode
    (add-hook 'python-mode-hook 'subword-mode)

    ;; Enable for Clojure mode
    (add-hook 'clojure-mode-hook 'subword-mode)

    ;; Enable for C-like modes
    (add-hook 'c-mode-common-hook 'subword-mode)))

;; Helm-Gtags
(use-package helm-gtags
  :ensure t
  :if (featurep 'helm)
  :defer t
  :init
  (progn
    ;; Set the default key mapping
    (setq helm-gtags-prefix-key "\C-cg"
	  helm-gtags-suggested-key-mapping t
	  helm-gtags-ignore-case t
	  helm-gtags-auto-update t)

    ;; Enable for C-like modes
    (add-hook 'c-mode-common-hook 'helm-gtags-mode))
  :diminish helm-gtags-mode
  )

;; +++-
;; Clojure
(use-package cider
  :ensure t
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'cider-mode))

;; +++-
;; Python

;; Python mode
(use-package python
  :init
  (progn
    (add-hook 'python-mode-hook
    	      (lambda () (set (make-local-variable
    			       'comment-inline-offset) 2)))
    (add-hook 'python-mode-hook (lambda () (setq fill-column 79)))))

;; WORKAROUND: for imenu bug in emacs 24.5. Remove after update!
;; (use-package semantic
;;   :config (remove-hook 'python-mode-hook 'wisent-python-default-setup)
;;   )

;; Anaconda-mode
(use-package anaconda-mode
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  ;; (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :diminish anaconda-mode)

;; Anacoda company backend
(use-package company-anaconda
  :ensure t
  :defer t
  :init (with-eval-after-load 'company
  	  (add-to-list 'company-backends 'company-anaconda)))

;; Pyenv-mode
(use-package pyenv-mode
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'pyenv-mode))

;; +++
;; Installed from source
(setq local-sources-dir (file-name-as-directory (expand-file-name
						 "local-sources"
						 user-emacs-directory)))

;; +++- Orgmine
;; +++-- Orgmine Dependencies
(use-package elmine
  :ensure t
  :defer t)

(use-package markdown-mode
  :ensure t
  :defer t)

(use-package request
  :ensure t
  :defer t)

;; Add the path to source in load-path
(add-to-list 'load-path (file-name-as-directory (expand-file-name
						 "orgmine"
						 local-sources-dir)))
(when (require 'orgmine nil t)
  (add-hook 'org-mode-hook
	    (lambda () (if (assoc "om_project" org-file-properties)
			   (orgmine-mode))))
  (require 'orgmine-config))

;;; init.el ends here
