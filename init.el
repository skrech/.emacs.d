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

;; Windows
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  ;; Append MSYS2 to PATH on Windows
  (setenv "PATH" (concat "C:\\msys64\\mingw64\\bin;" (getenv "PATH")))
  (setq exec-path (append '("C:\\msys64\\mingw64\\bin") exec-path))

  (setenv "PATH" (concat "C:\\msys64\\usr\\bin;" (getenv "PATH")))
  (setq exec-path (append '("C:\\msys64\\usr\\bin") exec-path))

  ;; Fix 'find' listing on Windows
  (setq find-ls-option '("-exec ls -ldh {} +" . "-ldh")))

;; MacOS
(when (eq system-type 'darwin)
  ;; Swap Command and Control keys on OSX because if we use Mac with
  ;; their own keyboard and we remapped these keys in System
  ;; Preferences.
  (setq mac-command-modifier 'meta
	mac-option-modifier 'super
	mac-right-option-modifier 'control)

  ;; Append Homebrew bin dir to exec-path on OSX
  (setenv "PATH" (concat "/usr/local/bin:"
			 (expand-file-name "~/go/bin:")
			 (getenv "PATH")))
  (add-to-list 'exec-path "/usr/local/bin")
  (add-to-list 'exec-path (expand-file-name "~/go/bin"))

  ;; Enable emoji for macOS
  (set-fontset-font t 'symbol "Apple Color Emoji" nil 'prepend)

  ;; Force en_US.UTF-8 locale because macOS creates en_BG.UTF-8 when
  ;; selecting English as main language and Bulgaria as
  ;; Region. However, this locale is not defined (checked with "locale
  ;; -a")
  (setenv "LANG" "en_US.UTF-8"))

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

;; Shows current function name -- in the header for modes are
;; specified in `sch/which-function-in-header-modes'
(defvar sch/which-function-in-header-modes '(python-mode))
(defvar-local sch/which-function-in-mode-line t)

(defun sch/which-function-in-header ()
  "Puts `which-function' output in header line for the designated modes."
  (setq sch/which-function-in-mode-line
	(not (memq major-mode sch/which-function-in-header-modes)))
  (unless sch/which-function-in-mode-line
      (setq header-line-format '(" " which-func-format " "))))

(defun sch/which-function-disable-mode-line ()
  "Don't show `which-function' in modes that want to show it in header."
  (let ((old-construct (assq 'which-function-mode mode-line-misc-info))
	(misc-info (assq-delete-all 'which-function-mode mode-line-misc-info)))
    (add-to-list 'misc-info
		 (list 'sch/which-function-in-mode-line old-construct))
    (setq mode-line-misc-info misc-info)))

(add-hook 'after-change-major-mode-hook 'sch/which-function-in-header)
(which-function-mode t)
(sch/which-function-disable-mode-line)

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
;; Tree-sitter

(require 'treesit nil 'noerror)

(defun sch/treesit-available-p (&optional lang)
  "Check if current Emacs is built with tree-sitter support.
LANG might be supplied (a symbol), which will additionally check if
that language is available for tree-sitter."
  (when (featurep 'treesit)
    (if lang
	(treesit-ready-p lang)
      (treesit-available-p))))

(defun sch/setup-treesit-grammers ()
  "Configure Tree-Sitter grammer sources alist and install grammers.
Will execute only if Tree-Sitter is actually available."
  (if (sch/treesit-available-p)
      (progn
	(setq treesit-language-source-alist
	      '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
		(heex "https://github.com/phoenixframework/tree-sitter-heex")))
	;; Install the grammers (if not already installed)
	(mapc #'treesit-install-language-grammar
	      (seq-remove (lambda (lang) (treesit-language-available-p lang))
			  (mapcar #'car treesit-language-source-alist))))
    (message "Tree-Sitter not available. Skipping its initialization.")))

(sch/setup-treesit-grammers)

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

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)		; refresh packages cache
  (package-install 'use-package))

;; ------------------------------
;; Packages init with use-package

;; +++
;; Always-on

;; Themes
;; (use-package color-theme-sanityinc-tomorrow
;;   :ensure t
;;   :init
;;   (load-theme 'sanityinc-tomorrow-eighties t))

(use-package solarized-theme
  :ensure t
  :init
  ;; Don't change the font for some headings and titles
  ;; (setq solarized-use-variable-pitch nil)
  ;; Remove scaling of fonts
  ;; (setq solarized-scale-org-headlines nil
  ;; 	solarized-height-minus-1 1.0
  ;; 	solarized-height-plus-1 1.0
  ;; 	solarized-height-plus-2 1.0
  ;; 	solarized-height-plus-3 1.0
  ;; 	solarized-height-plus-4 1.0)
  (load-theme 'solarized-light t))

;; Ivy -- Completion mechanism.
(use-package ivy
  :ensure t
  :init
  (ivy-mode)
  :bind (:map ivy-minibuffer-map
	      ([tab] . ivy-alt-done))
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "
	ivy-on-del-error-function 'ignore)
  :diminish ivy-mode)

;; Ivy Hydra
(use-package ivy-hydra
  :ensure t)

;; Counsel -- Power replacement for Emacs' commands and some external tools.
(use-package counsel
  :ensure t
  :init
  (counsel-mode)
  :bind
  (("C-x c r" . ivy-resume)
   ("C-x c g" . counsel-git)
   ("C-x c j" . counsel-git-grep)
   ("C-x c L" . counsel-git-log)
   ("C-x c l" . counsel-locate)
   ("C-x c a" . counsel-ag)
   ("C-x c i" . counsel-imenu))
  :config
  (setq counsel-ag-base-command "ag --nogroup --nocolor %s"
	counsel-find-file-ignore-regexp (concat "\\(?:^[#.]\\)"
						"\\|"
						"\\(?:[#~]$\\)" ;lock/temp files
						"\\|"
						"\\(?:\\.pyc$\\)" ;.pyc files
						"\\|"
						"\\(?:^__pycache__$\\)" ;pycache
						))
  :diminish counsel-mode)

;; Swiper -- Isearch on steroids using Ivy.
(use-package swiper
  :ensure t
  :bind
  (("C-s" . swiper-isearch)
   ("M-s ." . swiper-isearch-thing-at-point)))

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
	projectile-completion-system 'ivy
	)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;; Avy -- Jump to visible text
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g f") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1))

;; Counsel-projectile -- Integrates projectile with Ivy
(use-package counsel-projectile
  :ensure t
  :if (and (featurep 'counsel) (featurep 'projectile))
  :init
  (counsel-projectile-mode)
  :diminish)

;; Company-mode
(use-package company
  :ensure t
  :init
  (global-company-mode)
  :config
  (setq company-show-quick-access t)
  :bind
  (("C-c a" . company-complete-tooltip-row))
  :diminish company-mode)

;; Which key
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :diminish which-key-mode)

;; Magit -- A Git Porcelain inside Emacs.
(use-package magit
  :ensure t
  :bind (("C-c g g" . magit-status)
	 ("C-c g d" . magit-dispatch)
	 ("C-c g f" . magit-file-dispatch)))

;; Yasnippet
(use-package yasnippet-snippets
  :ensure t)

(use-package yasnippet
  :ensure t
  :after (yasnippet-snippets)
  :init
  (yas-global-mode)
  :diminish yas-minor-mode)

;; Semantic - Source code lexical analysis from CEDET
(use-package semantic
  :init
  (semantic-mode 1)
  :config
  ;; Remove python from semantic
  (assoc-delete-all 'python-mode semantic-new-buffer-setup-functions))

;; Diminish - needed for proper work of use-package
(use-package diminish
  :ensure t)

;; EditorConfig support
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1)
  :diminish)

;; +++
;; Deffered

;; +++-
;; Built-in

;; Flymake -- And shorten the mode-line string.
(use-package flymake
  :defer t
  :init
  (add-hook 'emacs-lisp-mode-hook 'flymake-mode)
  :bind (:map flymake-mode-map
	      ("M-n" . flymake-goto-next-error)
	      ("M-p" . flymake-goto-prev-error)))

(defun sch/shorten-flymake-mode-line (ret)
  "Change the output of `flymake--mode-line-format'.
RET is the original return from the function."
  (setf (seq-elt (car ret) 1) " Fly")
  ret)

(advice-add 'flymake--mode-line-format
	    :filter-return 'sch/shorten-flymake-mode-line)

;; Spell-checking
(use-package flyspell
  :defer t
  :hook
  (text-mode . flyspell-mode)
  :diminish)

;; ElDoc -- just diminish the minor mode.
(use-package eldoc
  :defer t
  :diminish eldoc-mode)

;; Subword -- allows to move on sub-word in CamelCase
(use-package subword
  :defer t
  :hook
  ((python-mode clojure-mode c-mode-common typescript-mode org-mode) . subword-mode))

;; Org-mode -- Emacs' flawless organize package.
(use-package org
  :defer t
  :bind (("C-c o l" . org-store-link)
	 ("C-c o a" . org-agenda)
	 ("C-c o c" . org-capture)
	 ("C-c o b" . org-switchb))
  :config
  ;; Use enhanced exporter if available
  (if (require 'ox-confluence-en nil t)
      ;; In current work place, Confluence doen't support PlantUML, so
      ;; disable macro export.
      (setq ox-confluence-en-use-plantuml-macro nil)
    (require 'ox-confluence))
  (setq org-directory "~/org"

	;; TODO keywords colours
	org-todo-keyword-faces '(("TODO" . org-warning)
				 ("PRG" . "yellow")
				 ("WAIT" . "orange")
				 ("DONE" . org-done))

	;; File to keep the captured items
	org-default-notes-file (concat org-directory "/refile.org")

	;; Custom capture templates.
	;; NOTE: This is variable from package org-capture...
	org-capture-templates '(("t" "Task" entry (file "")
				 "* TODO %?\n  Logged on: %u")

				("n" "Note" entry (file "notes.org")
				 "* %?\n  Logged on: %u"))

	;; Agenda config
	org-agenda-files (concat org-directory "/agenda_files")
	org-agenda-restore-windows-after-quit t
	org-agenda-todo-list-sublevels nil
	;;; In TODO View of agenda, make visible only "open" (with not
	;;; active timestamp) TODOs
	org-agenda-todo-ignore-with-date t

	org-refile-targets '((org-agenda-files . (:regexp . "Tasks$"))
			     (org-agenda-files . (:level . 1)))
	org-refile-use-outline-path t
	org-outline-path-complete-in-steps nil
	org-plantuml-exec-mode 'plantuml)

  ;; Load babel evaluation languages support.
  (org-babel-do-load-languages 'org-babel-load-languages
			       '((emacs-lisp . t)
				 (plantuml . t))))

;; +++-
;; LSP Client -- common for many languages.
(use-package eglot
  :ensure t
  :defer t
  :bind (:map eglot-mode-map
	 ("C-c e f" . eglot-format))
  :hook
  ((python-mode . eglot-ensure)
   (go-mode . eglot-ensure)
   (typescript-mode . eglot-ensure)))

;; +++- Lisp Modes
(use-package paredit
  :ensure t
  :defer t
  :bind (:map paredit-mode-map
	      ("M-s" . nil)
	      ("M-r" . nil)
	      ("M-?" . nil)
	      ("C-c e s" . paredit-splice-sexp)
	      ("C-c e r" . paredit-raise-sexp)
	      ("C-c e c" . paredit-convolute-sexp))
  :hook (lisp-mode emacs-lisp-mode clojure-mode)
  :diminish)

;; +++-
;; Clojure
(use-package cider
  :ensure t
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'cider-mode))

;; +++-
;; Guile Scheme
(use-package geiser-guile
  :ensure t
  :defer t
  :hook (scheme-mode . geiser-mode))

;; +++-
;; Elixir
(use-package elixir-ts-mode
  :ensure t
  :defer t
  :if (sch/treesit-available-p 'elixir)
  :init
  ;; Emacs 30 has builtin support for `elixir-mode' and
  ;; `elixir-ts-mode', so we only remap. Older versions of Emacs will
  ;; automatically update 'auto-mode-alist' from the autoloads in the
  ;; (backported/this) MELPA package.
  (when (>= emacs-major-version 30)
    (push '(elixir-mode . elixir-ts-mode) major-mode-remap-alist)))

;; +++-
;; Python

;; Python mode
(defun sch/python-inline-comment-offset ()
  "Set inline offset for comments in Python buffers."
  (set (make-local-variable 'comment-inline-offset) 2))

(use-package python
  :defer t
  :init
  (add-hook 'python-mode-hook 'sch/python-inline-comment-offset)
  :config
  ;; Make indentation compatible with black
  (setq python-indent-def-block-scale 1))


;; Pyenv
(defface sch/pyenv-face '((t (:weight bold :foreground "#de935f")))
  "The face used to highlight the current python on the modeline."
  :group 'pyenv)

(defun sch/restart-eglot-on-pyenv-change ()
  "Restart eglot LSP when python version is changed."
  (and (featurep 'eglot) (eglot-ensure)))

(defun sch/pyenv-modeline-function (current-python)
  "Custom mode-line for pyenv.
CURRENT-PYTHON - string, currently selected python version."
  `(:eval (if (eq major-mode 'python-mode)
	      (format "|%s|" (propertize
			      ,current-python
			      'face
			      'sch/pyenv-face)))))

(use-package pyenv
  :straight (:host github :repo "aiguofer/pyenv.el")
  :defer t
  :if (executable-find "pyenv")
  :init
  ;; Search for the executable in PATH (why isn't this the default)
  (setq pyenv-executable "pyenv")

  ;; Change mode-line func.
  (setq pyenv-modeline-function 'sch/pyenv-modeline-function)

  ;; TODO: See the comment below for deferring.
  (global-pyenv-mode 1)
  :bind (("C-c v" . pyenv-use))
  :hook (
	 ;; TODO: Find a way to properly defer activating the global minor mode.
	 ;; (python-mode . global-pyenv-mode)
	 (pyenv-mode . sch/restart-eglot-on-pyenv-change)))

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
;; SQL
(use-package sql-indent
  :ensure t)

;; +++-
;; Shell script
(use-package flymake-shellcheck
  :ensure t
  :defer t
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))

;; +++-
;; Typescript
(use-package typescript-mode
  :ensure t
  :defer t
  :init
  (add-hook 'typescript-mode-hook
	    (lambda () (setq indent-tabs-mode nil))))

;; +++-
;; Angular
(use-package ng2-mode
  :ensure t
  :defer t)

;; +++-
;; HTTP

;; Clever way of making requests.
(use-package restclient
  :ensure t
  :mode ("\\.restclient\\'" . restclient-mode))

;; Support for 'jq'-based hooks in 'restclient'
(use-package restclient-jq
  :ensure t
  :after restclient) ; note: `:after' uses eval-after-load; `:defer' should not be combined with `:after'!

;; jq-mode -- mainly as dependency for restclient-jq
(use-package jq-mode
  :ensure t
  :defer t)

;; HTTP requests lib.
(use-package request
  :ensure t
  :defer t)

;; +++-
;; Dockerfile
(use-package dockerfile-mode
  :ensure t
  :defer t
  :config
  (setq dockerfile-enable-auto-indent nil))

;; +++-
;; Kubernetes (old)
;; (use-package kubernetes
;;   :ensure t
;;   :commands (kubernetes-overview))

;; +++-
;; Kubernetes
(use-package kele
  :ensure t
  :bind-keymap
  ("C-c k" . kele-command-map)
  :config
  ;; Temporary force kubeconfig to the default one
  (setq kele-kubeconfig-path "~/.kube/config"))

;; +++-
;; Rego Policy Major Mode
(use-package rego-mode
  :ensure t
  :defer t)

;; +++-
;; Groovy major mode.
(use-package groovy-mode
  :ensure t
  :defer t)

;; +++-
;; Misc

;; Org community contributions
(use-package org-contrib
  :straight t
  :ensure t
  :defer t)

;; Rainbow-Delimiters -- colors parentheses in programming modes.
;; (use-package rainbow-delimiters
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;   :diminish rainbow-delimiters-mode)

;; Helm-Gtags -- helm interface to gtags.
;; (use-package helm-gtags
;;   :ensure t
;;   :if (featurep 'helm)
;;   :defer t
;;   :init
;;   (progn
;;     ;; Set the default key mapping
;;     (setq helm-gtags-prefix-key "\C-cg"
;; 	  helm-gtags-suggested-key-mapping t
;; 	  helm-gtags-ignore-case t
;; 	  helm-gtags-auto-update t)

;;     ;; Enable for C-like modes
;;     (add-hook 'c-mode-common-hook 'helm-gtags-mode))
;;   :diminish helm-gtags-mode)

;; Counsel-Gtags -- ivy interface to gtags.
(use-package counsel-gtags
  :ensure t
  :if (featurep 'counsel)
  :defer t
  :bind (:map counsel-gtags-mode-map
	      ("M-." . counsel-gtags-find-definition)
	      ("M-r" . counsel-gtags-find-reference)
	      ("M-s" . counsel-gtags-find-symbol)
	      ("M-," . counsel-gtags-go-backward))
  :init
  ;; Enabled for C-like modes
  (add-hook 'c-mode-common-hook 'counsel-gtags-mode)
  :diminish)

;; Amx - smex-like sorting in counsel-M-x.
(use-package amx
  :ensure
  :defer t)

;; Markdown major mode.
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :defer t)

;; PlantUML major mode.
(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :config
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-output-type "png")

  ;; Integrate with org-mode editing
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))

;; Epub reader
(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode))

;; +++
;; Installed from source
;; +++- Orgmine
;; + ++-- Orgmine Dependencies
(use-package elmine
  :ensure t
  :defer t)


(let ((local-sources-dir (file-name-as-directory
			  (expand-file-name
			   "local-sources"
			   user-emacs-directory))))
  ;; Orgmine
  (add-to-list 'load-path (file-name-as-directory
			   (expand-file-name
			    "orgmine"
			    local-sources-dir)))
  (when (require 'orgmine nil t)
    (add-hook 'org-mode-hook
	      (lambda () (if (assoc "om_project" org-file-properties)
			     (orgmine-mode))))
    (require 'orgmine-config))

  ;; Confluence enhanced exporter
  (add-to-list 'load-path (file-name-as-directory
			   (expand-file-name
			    "ox-confluence-en"
			    local-sources-dir))))

;;; init.el ends here
