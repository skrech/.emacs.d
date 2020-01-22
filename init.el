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

;; Swap Command and Control keys on OSX because if we use Mac with
;; their own keyboard and we remapped these keys in System
;; Preferences.
(setq mac-command-modifier 'control
      mac-control-modifier 'command)

(when (eq system-type 'gnu/linux)
  (setq exec-path (cons (expand-file-name "bin" "~") exec-path)))

;; ----------------------------------
;; Adding local-configs to load-path.
(add-to-list 'load-path (file-name-as-directory
			 (expand-file-name "local-configs"
					   user-emacs-directory)))

;; ------------------------
;; Global built-in configs.

;; Disable toobar.
(tool-bar-mode -1)

;; Navigation numbers here and there.
(setq column-number-mode t)	    ; show column number in modeline
(if (>= emacs-major-version 26)	    ; display line numbers (> ver. 26)
    (global-display-line-numbers-mode)
  (global-linum-mode t))

;; Fix scroll.
(setq scroll-conservatively 10000)    ; allow to scrol line-by-line

;; Parentheses stuff
(electric-pair-mode)		      ; auto-close parentheses
(show-paren-mode)		      ; show matching parentheses

;; Delete trailing space before save.
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)	       ; delete whitespaces
(put 'dired-find-alternate-file 'disabled nil) ; reuse dired buffers

;; Shows current function name -- in the header for modes derived from
;; prog-mode.
(defun sch/which-function-in-header ()
  "Puts which-function output in header line."
  (setq header-line-format '("" which-func-format " ")))

(which-function-mode t)
(add-hook 'prog-mode-hook 'sch/which-function-in-header)
(let ((misc-info (assq-delete-all 'which-function-mode mode-line-misc-info)))
  (add-to-list 'misc-info
           '(which-function-mode   ;Only display if mode is enabled.
                 (which-func-mode  ;Only display if buffer supports it.
                  (:eval           ;Only for major modes not derived from prog-mode
           (unless (derived-mode-p 'prog-mode)
             (concat (format-mode-line which-func-format) " "))))))
  (setq mode-line-misc-info misc-info))

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

;; Abbreviate long path-like Git branch names
(defun shorten-git-mode-line (return-string)
  "Abbreviates path-like Git branches and preserves the prefix.
RETURN-STRING - the string returned by vc-git-mode-line-string."
  (let ((prefix (substring return-string 0 4)))
    (concat prefix (replace-regexp-in-string "\\([^/]\\{2\\}\\)[^/]*/"
					     "\\1/"
					     return-string
					     nil nil nil 4))))
(advice-add 'vc-git-mode-line-string
	    :filter-return
	    'shorten-git-mode-line)

;; ----
;; Straight

;; Disable straight.el customization hacks.
(defvar straight-enable-package-integration)
(setq straight-enable-package-integration nil)

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; ----
;; ELPA

;; Set repos
(setq package-archives
      ;; Package archives
      '(("GNU ELPA" . "http://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")))

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

  :bind
  (("M-x" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("C-x b" . helm-mini)
   ("M-y" . helm-show-kill-ring)
   ("M-s o" . helm-occur)

   :map helm-map
   ("<tab>" . helm-execute-persistent-action) ; swap <tab> and C-z
   ("C-z" . helm-select-action))

  :config
  (setq helm-split-window-inside-p t)  ; helm window in current window
  (setq helm-ff-skip-boring-files t)   ; don't show eg. temp files and vc
  :diminish helm-mode)

;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :config
  (setq projectile-indexing-method 'native
	projectile-enable-caching t
	projectile-mode-line-function '(lambda () (format " Prj[%s]" (projectile-project-name)))
	;; projectile-svn-command "svn list -R --include-externals . | grep -v '/$' | tr '\\n' '\\0'"
	projectile-globally-ignored-directories (append '("*__pycache__/")
							projectile-globally-ignored-directories)
	)
  :bind-keymap
  ("C-x p" . projectile-command-map))

;; Helm-Projectile
(use-package helm-projectile
  :ensure t
  :if
  (and (featurep 'helm) (featurep 'projectile))
  :init
  (helm-projectile-on))

;; Helm-ag
(use-package helm-ag
  :ensure t
  :defer t
  :if (featurep 'helm))

;; Yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode)
  :diminish yas-minor-mode)

(use-package yasnippet-snippets
  :ensure t
  :if (featurep 'yasnippet))

;; Semantic - Source code lexical analysis from CEDET
(use-package semantic
  :init
  (semantic-mode 1)
  :config
  ;; Remove python from semantic
  (assoc-delete-all 'python-mode semantic-new-buffer-setup-functions))

;; Which key
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :diminish which-key-mode)

;; Company-mode
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :diminish company-mode)

;; Flycheck
;; (use-package flycheck
;;   :ensure t
;;   :init
;;   (global-flycheck-mode)
;;   :config
;;   (setq flycheck-python-flake8-executable "python"))

;; Flycheck pos-tip
;; (use-package flycheck-pos-tip
;;   :ensure t
;;   :init
;;   (with-eval-after-load 'flycheck
;;     (flycheck-pos-tip-mode)))

;;  Diminish - needed for proper work of use-package
(use-package diminish
  :ensure t)

;; +++
;; Deffered

;; +++-
;; Built-in

;; Flymake -- And shorten the mode-line string.
(use-package flymake
  :defer t
  :bind
  (:map flymake-mode-map
	("M-n" . flymake-goto-next-error)
	("M-p" . flymake-goto-prev-error)))

(defun sch/transform-flymake-mode-line-format (ret)
  "Change the output of `flymake--mode-line-format'."
  (setf (seq-elt (car ret) 1) " Fly")
  ret)

(advice-add 'flymake--mode-line-format
	    :filter-return 'sch/transform-flymake-mode-line-format)

;; ElDoc -- just diminish the minor mode.
(use-package eldoc
  :defer t
  :diminish eldoc-mode)

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

;; Org-mode -- Emacs' flawless organize package.
(use-package org
  :defer t
  :bind (("C-x j l" . org-store-link)
	 ("C-x j a" . org-agenda)
	 ("C-x j c" . org-capture)
	 ("C-x j b" . org-switchb))
  :config
  (setq org-directory "~/org"
	org-default-notes-file (concat org-directory "/refile.org")

	;; Search for agenda files in this file.
	org-agenda-files (concat org-directory "/agenda_files")

	;; Custom capture templates.
	;; NOTE: This is variable from package org-capture...
	org-capture-templates '(("t" "Task" entry (file "")
				 "* TODO %?\n  Logged on: %u")

				("n" "Note" entry (file "notes.org")
				 "* %?\n  Logged on: %u"))

	org-refile-targets '((org-agenda-files . (:level . 1)))))

;; +++-
;; LSP Client -- common for many languages.
(use-package eglot
  :ensure t
  :defer t)

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
(defun sch-python-fill-column ()
  (setq fill-column 79))

(use-package python
  :init
  (progn
    (add-hook 'python-mode-hook
    	      (lambda () (set (make-local-variable
    			       'comment-inline-offset) 2)))
    (add-hook 'python-mode-hook 'sch-python-fill-column)
    (add-hook 'python-mode-hook 'eglot-ensure)))

;; Anaconda-mode
;; (use-package anaconda-mode
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'python-mode-hook 'anaconda-mode)
;;   (add-hook 'python-mode-hook 'anaconda-eldoc-mode)
;;   :diminish anaconda-mode)

;; Anacoda company backend
;; (use-package company-anaconda
;;   :ensure t
;;   :defer t
;;   :init (with-eval-after-load 'company
;;   	  (add-to-list 'company-backends 'company-anaconda)))

;; Pyenv
(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :init
  ;; This is a global mode, but I use python buffers to defer it's enablement.
  (add-hook 'python-mode-hook 'global-pyenv-mode)
  :bind (("C-x M-v" . pyenv-use))
  :config
  (when (eq system-type 'darwin)
    (setq pyenv-executable "/usr/local/bin/pyenv")))

;; Sphinx docstrings generation
(use-package sphinx-doc
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'sphinx-doc-mode)
  :diminish sphinx-doc-mode)

;; Syntax highlight and fill-paragraph for docstrings.
(use-package python-docstring
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'python-docstring-mode)
  :diminish python-docstring-mode)

;; +++-
;; Misc

;; Rainbow-Delimiters -- colors parentheses in programming modes.
(use-package rainbow-delimiters
  :ensure t
  :defer t
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  :diminish rainbow-delimiters-mode)

;; Helm-Gtags -- helm interface to gtags.
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
  :diminish helm-gtags-mode)

;; Epub reader
(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode))

;; +++
;; Installed from source
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

(let ((local-sources-dir (file-name-as-directory
			  (expand-file-name
			   "local-sources"
			   user-emacs-directory))))
  ;; Add the path to source in load-path
  (add-to-list 'load-path (file-name-as-directory
			   (expand-file-name
			    "orgmine"
			    local-sources-dir)))
  (when (require 'orgmine nil t)
    (add-hook 'org-mode-hook
	      (lambda () (if (assoc "om_project" org-file-properties)
			     (orgmine-mode))))
    (require 'orgmine-config)))

;;; init.el ends here
