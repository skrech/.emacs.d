;;; init.el --- Emacs configuration of Kristiyan Kanchev

;;; Commentary:
;;; Emacs configuration of Kristiyan Kanchev
;;;
;;; Structure of the file:
;;; = Medium
;;; -- Customization file
;;; -- Tree-sitter
;;; -- Package managers
;;; = Global
;;; -- Emacs base config
;;; -- Global minor modes
;;; = Deferred
;;; -- Tools
;;; -- Tool modes (minor modes)
;;; -- Programming modes
;;; -- Object Notation / Serialization
;;; -- Query modes
;;; -- Markup modes
;;; -- Styling modes
;;; -- Purpose-specific
;;; -- Organization modes
;;; -- Misc major modes
;;; = Site-specific


;;; Code:
;;; ----------------------------------------------
;;; ------------------------------------- Medium ;
;;; ----------------------------------------------

;;; * Setup infrastructure for extending Emacs. In this section the
;;; * facilities and mechanisms for inatalling new modules to Emacs
;;; * would be configured and/or executed. They might do one-time
;;; * idempotent installation or prepare themselves for subsequent use
;;; * in the rest of this file.


;;; ------------------
;;; Customization file

(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))


;;; -----------
;;; Tree-sitter

(defvar treesit-language-source-alist)
(setq treesit-language-source-alist
      '((elixir "https://github.com/elixir-lang/tree-sitter-elixir")
	(heex "https://github.com/phoenixframework/tree-sitter-heex")
	(javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (jsdoc "https://github.com/tree-sitter/tree-sitter-jsdoc")
	(typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
	(tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
	(yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(defun sch/treesit-available-p (lang)
  "Check if the supplied LANG (a symbol) is available for tree-sitter."
  (when (featurep 'treesit)
    (declare-function treesit-ready-p "treesit")
    (treesit-ready-p lang)))

(defun sch/setup-treesit-grammers ()
  "Configure Tree-Sitter grammer sources alist and install grammers.
Needs Tree-Sitter to actually be available."
  ;; Install the grammers (if not already installed)
  (mapc #'treesit-install-language-grammar
	(seq-remove (lambda (lang) (treesit-language-available-p lang))
		    (mapcar #'car treesit-language-source-alist))))

;;; Check if current Emacs is built with tree-sitter support and set it up.
(if (eval-when-compile (and (fboundp 'treesit-available-p) (treesit-available-p)))
    (progn
      (require 'treesit)
      (sch/setup-treesit-grammers))
  (message "Tree-Sitter not available. Skipping its initialization."))


;;; ----------------
;;; Package managers


;;; +++ Package.el

(require 'package)

;; Call `eval-and-compile' to make the byte-compiler aware of the
;; load-path modifications and autoloadeds in the installed packages.

;; -- EXPLANATION: `package.el' creates autoloads file when installing
;; every package. `package-initialize' enumerates the installed
;; packages and then 'activates' them (meaning, modifying load-path
;; and then loading their autoloads).  Byte-compiler should *eval* the
;; initialization code in addition to compiling it (because compile
;; only records the function call w/o knowing what would happen
;; inside), so it knows the effects of activation. Usually, package
;; initialization happens before user init is executed. However,
;; byte-compiler (for linting) is always started with `emacs -q'
;; switch, which doesn't initialize the packages as normal (see the
;; docs of `package.el'). That's why we introduced early-init.el and
;; manually call `package-initialize', so the code for byte-compiler
;; and interpreter are the same.

;; -- WARNING: I came up with this solution *myself*. Check from time to
;; time what would happen if eval-and-compile is removed --
;; `package.el' or `use-package' might have prepared a built-in
;; solution.
(eval-and-compile
  (package-initialize))

;;; Set repos
(setq package-archives
      ;; Package archives
      '(("GNU ELPA" . "http://elpa.gnu.org/packages/")
        ("MELPA"    . "https://melpa.org/packages/")))


;;; +++ Straight

;;; Disable straight.el customization hacks.
(defvar straight-enable-package-integration)
(setq straight-enable-package-integration nil)

;;; Bootstrap straight.el
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


;;; +++ Use-package

;;; Install `use-package' if using older version of Emacs.
(unless (package-installed-p 'use-package)
  (package-refresh-contents)		; refresh packages cache
  (package-install 'use-package))

(require 'use-package)

;;; Diminish - needed for proper work of use-package
(use-package diminish
  :ensure t)


;;; ----------------------------------------------
;;; ------------------------------------- Global ;
;;; ----------------------------------------------

;;; * Global (always-on) configuration. These modes might provide
;;; * essential and/or advanced functionality. Note, packages related
;;; * to the one being configured are allowed in this section even if
;;; * they are not providing global minor mode, or are themselves more
;;; * like tools (and thus seem to fit better different sections in
;;; * this file).


;;; -------------------
;;; Base customizations

;;; * Configuration of variables and behaviours part of the Emacs
;;; * base. The base consists of packages that are part of Emacs
;;; * distribution and are *already loaded* on startup. These are
;;; * mostly variable configurations defined in C source, but might
;;; * also include fundamental behaviours, GUI setup, and
;;; * environment-dependent tuning. Let's imagine Emacs base as
;;; * C-source and very thin layer of Elisp comprising of modules such
;;; * as startup.el, simple.el, files.el and some other. It OK to
;;; * place here things that *feel* fundamental, or you can't decide
;;; * where to put. However, if the facility being considered can even
;;; * remotely be classified as a sub-system in spite of being
;;; * perceived as fundamental, such as Dired, separate it into its
;;; * own section, especially if custom defuns are needed.

(use-package emacs
  :config
  ;; +++ Emacs-wide varialbes
  (setq user-mail-address "skrechy@gmail.com")

  ;; +++ Behaviours
  ;; Scroll by line
  (setq scroll-conservatively 10000)

  ;; Prevent accidental exiting
  (setq confirm-kill-emacs 'y-or-n-p)

  ;; Delete trailing whitespace before save.
  (add-hook 'before-save-hook
	    'delete-trailing-whitespace)

  ;; +++ GUI
  ;; Disable the toolbar with icons
  (tool-bar-mode -1)

  ;; +++ Editing
  ;; Auto-close parentheses
  (electric-pair-mode)

  ;; Show matching parentheses
  (show-paren-mode)

  ;; Disable tabs as indentation by default
  (setq-default indent-tabs-mode nil)

;;; +++ Visual
  ;; Show line numbers in the fringe
  (if (eval-when-compile (>= emacs-major-version 26))
      (global-display-line-numbers-mode)
    (global-linum-mode t))

  ;; Show column number in modeline
  (setq column-number-mode t)

  ;; *** OS-specific
  (cond
   ;; Windows
   ((memq system-type '(windows-nt msdos))
    ;; Append MSYS2 to PATH on Windows
    (setenv "PATH" (concat "C:\\msys64\\mingw64\\bin;" (getenv "PATH")))
    (add-to-list 'exec-path "C:\\msys64\\mingw64\\bin")
    (setenv "PATH" (concat "C:\\msys64\\usr\\bin;" (getenv "PATH")))
    (add-to-list 'exec-path "C:\\msys64\\usr\\bin")

    ;; Fix 'find' listing on Windows
    (with-eval-after-load 'find-dired
      (if (boundp 'find-ls-option)
	  (setq find-ls-option '("-exec ls -ldh {} +" . "-ldh")))))

   ;; MacOS
   ((eval-when-compile (eq system-type 'darwin))
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
    (setenv "LANG" "en_US.UTF-8"))))


;;; ----------------------------------------------
;;; Global minor modes

;;; Theme
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

;;; Which-function (builtin) -- Shows which function the pointer is at in the minibuffer.
(use-package which-func
  :config
  (add-hook 'after-change-major-mode-hook 'sch/which-function-in-header)
  (which-function-mode)
  ;; Change the global mode-line *template*.
  (sch/which-function-disable-mode-line)
  :preface
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
      (setq mode-line-misc-info misc-info))))

;;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode)
  :config
  (setq projectile-indexing-method 'hybrid
	projectile-enable-caching t
	projectile-mode-line-function '(lambda () (format " Prj[%s]" (projectile-project-name)))
	;; projectile-svn-command "svn list -R --include-externals . | grep -v '/$' | tr '\\n' '\\0'"
	projectile-globally-ignored-directories (append '("*__pycache__/")
							projectile-globally-ignored-directories)
	projectile-completion-system 'ivy
	)
  :bind-keymap
  ("C-c p" . projectile-command-map))

;;; Ivy -- Completion mechanism.
(use-package ivy
  :ensure t
  :demand t
  :bind (:map ivy-minibuffer-map
	      ([tab] . ivy-alt-done))
  :config
  (setq ivy-use-virtual-buffers t
	ivy-count-format "(%d/%d) "
	ivy-on-del-error-function 'ignore)
  (ivy-mode t)
  :diminish ivy-mode)

;; Ivy Hydra
(use-package ivy-hydra
  :ensure t
  :after ivy)

;;; Swiper -- Isearch on steroids using Ivy. Swiper is actually a
;;; "tool" (in my own terminology) but `counsel' depends on it, so
;;; it's easier to demand load it here.
(use-package swiper
  :ensure t
  :demand t
  :requires ivy
  :bind
  (("C-s" . swiper-isearch)
   ("M-s ." . swiper-isearch-thing-at-point)))

;;; Counsel -- Power replacement for Emacs' commands and some external tools.
(use-package counsel
  :ensure t
  :requires (ivy swiper)
  :demand t
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
  (counsel-mode)
  :diminish counsel-mode)

;;; Counsel-projectile -- Integrates projectile with Ivy
(use-package counsel-projectile
  :ensure t
  :after (counsel projectile)
  :config
  (counsel-projectile-mode)
  :diminish)

;;; Counsel-Gtags -- ivy interface to gtags.
(use-package counsel-gtags
  :ensure t
  :requires counsel
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

;;; Company-mode
(use-package company
  :ensure t
  :demand t
  :bind
  (("C-c a" . company-complete-tooltip-row))
  :config
  (setq company-show-quick-access t)
  (global-company-mode)
  :diminish company-mode)

;;; Which key
(use-package which-key
  :ensure t
  :demand t
  :config
  (which-key-mode)
  :diminish which-key-mode)

(use-package yasnippet
  :ensure t
  :demand t
  :config
  (yas-global-mode)
  :diminish yas-minor-mode)

;;; Yasnippet
(use-package yasnippet-snippets
  :ensure t
  :after yasnippet)

;;; EditorConfig support
(use-package editorconfig
  :ensure t
  :demand t
  :config
  (editorconfig-mode 1)
  :diminish)


;;; ----------------------------------------------
;;; ----------------------------------- Deferred ;
;;; ----------------------------------------------

;;; * Configuration for deferred packages. They might be one of the
;;; * following categories: Tools, Tool modes or Major modes.  Tools
;;; * are packages providing stand-alone functionality. This includes
;;; * interactive functions or library-type of code that is used by
;;; * other modes or functions. Interactive might include some one-off
;;; * actions or dispatching to thier own internal major mode. Tool
;;; * modes are non-global minor modes that provide some funcitonality
;;; * in the context of some major mode. Tool modes tend to provide
;;; * generic functionality that might be further cusmomized to
;;; * specific major mode (usually by mechanism similar to
;;; * "backends"). Place tools and tool modes here, when they are
;;; * shared by more than one consumming mode (thus, common in their
;;; * name). Major modes need no explanaition. Major modes are
;;; * categorized by functional domain. It's preferred to place a
;;; * tool, tool mode or tool backend (plugin/addon) along with its
;;; * major mode if it has a meaning only fot that major mode. Since
;;; * major modes often need no customization on their own its common
;;; * to have sections with only tools definitions that register into
;;; * the mode's hooks.


;;; ----------------------------------------------
;;; Tools

;;; ++* Auth and encryption

;;; Auth-source-pass (builtin) -- Integrate with `pass' UNIX utility.
(use-package auth-source-pass
  :after auth-source
  :config
  (setq auth-sources '("~/.authinfo" "~/.authinfo.gpg" password-store)))


;;; +++ File browsing

;;; Dired (builtin)
(use-package dired
  :defer t
  :config
  ;; Reuse Dired buffers
  (put 'dired-find-alternate-file 'disabled nil))

;;; Grep in Dired to not recurse into .svn directories
(use-package find-dired
  :defer t
  :config
  (setq find-grep-options "-Iq --exclude=\"*\\.svn*\""))


;;; +++ Version Control

;;; VC-Git (builtin) -- The code that shows Git info in the minibuffer.
(use-package vc-git
  :defer t
  :config
  ;; Abbreviate long path-like Git branch names
  (advice-add 'vc-git-mode-line-string
	      :filter-return
	      'shorten-git-mode-line)
  :preface
  (defun shorten-git-mode-line (return-string)
    "Abbreviates path-like Git branches and preserves the prefix.
RETURN-STRING - the string returned by `vc-git-mode-line-string'."
    (let ((prefix (substring return-string 0 4)))
      (concat prefix (replace-regexp-in-string "\\([^/]\\{2\\}\\)[^/]*/"
					       "\\1/"
					       return-string
					       nil nil nil 4)))))

;;; Magit -- A Git Porcelain inside Emacs.
(use-package magit
  :ensure t
  :bind (("C-c g g" . magit-status)
	 ("C-c g d" . magit-dispatch)
	 ("C-c g f" . magit-file-dispatch)))


;;; +++ Kubernetes

;;; Kele
(when (>= emacs-major-version 29)
  (use-package kele
    :ensure t
    :defer t
    :bind-keymap
    ("C-c k" . kele-command-map)
    :config
    ;; Temporary force kubeconfig to the default one
    (setq kele-kubeconfig-path "~/.kube/config")))


;;; +++ Efficient seraching and finding

;;; Amx - smex-like sorting.
;;; Used by "counsel-M-x".
(use-package amx
  :ensure
  :defer t)

;;; Avy -- Jump to visible text
;;; Also used by "counsel"
(use-package avy
  :ensure t
  :defer t
  :bind (("C-:" . avy-goto-char)
	 ("C-'" . avy-goto-char-2)
	 ("M-g f" . avy-goto-line)
	 ("M-g w" . avy-goto-word-1)))


;;; ----------------------------------------------
;;; Tool modes (minor modes)


;;; +++ Navigation and editing

;;; Subword -- allows to move on sub-word in CamelCase
(use-package subword
  :defer t
  :hook
  ((prog-mode org-mode) . subword-mode))


;;; +++ On-the-fly analisys

;;; Flymake -- And shorten the mode-line string.
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

;;; Spell-checking
(use-package flyspell
  :defer t
  :hook
  ;; Full spell-checking
  ((text-mode . flyspell-mode)

   ;; Prog spell-checking
   ((python-mode
     elixir-ts-mode
     go-mode
     js-base-mode
     typescript-ts-base-mode) . flyspell-prog-mode))
  :diminish)

;;; ElDoc -- just diminish the minor mode.
(use-package eldoc
  :defer t
  :diminish eldoc-mode)


;;; +++ Language server protocl

;;; LSP Client -- common for many languages.
(use-package eglot
  :ensure t
  :defer t
  :config
  (when (< emacs-major-version 30)
    ;; `eglot' in Emacs before version 30.* doesn't have support for
    ;; Lexical (Elixir) LSP. Add support for Lexical in that case.
    (add-to-list 'eglot-server-programs
		 `((elixir-ts-mode heex-ts-mode) .
                   ,(if (and (fboundp 'w32-shell-dos-semantics)
                             (w32-shell-dos-semantics))
			'("language_server.bat")
                      (eglot-alternatives
                       '("language_server.sh" "start_lexical.sh"))))))
  :bind (:map eglot-mode-map
	      ("C-c e f" . eglot-format))
  :hook
  ((python-mode
    elixir-mode
    elixir-ts-mode
    go-mode
    js-base-mode
    typescript-ts-base-mode) . eglot-ensure))


;;; +++ Paren matching

                                        ;TODO: Move electric-pair-mode
                                        ;setup here and setup it for
                                        ;text modes, org, python, etc.
;; (use-package elec-pair
;;   :ensure t
;;   :defer t
;;   :hook ((text-mode org-mode markdown-mode) . electric-pair-mode))

;;; Ruby-end -- mode for handling ruby-like do..end blocks
(use-package ruby-end
  :ensure t
  :defer t
  :hook ((elixir-mode elixir-ts-mode) . sch/activate-ruby-end-mode-for-elixir)
  :preface
  (defun sch/activate-ruby-end-mode-for-elixir ()
    (set (make-local-variable 'ruby-end-expand-keywords-before-re)
         "\\(?:^\\|\\s-+\\)\\(?:do\\)")
    (set (make-local-variable 'ruby-end-check-statement-modifiers) nil)
    (ruby-end-mode))
  :diminish)

;;; Smartparens -- For all modes that use complex delimiters
;; (use-package smartparens
;;   :ensure t
;;   :defer t
;;   :hook ((elixir-mode elixir-ts-mode) . smartparens-mode)
;;   :config
;;   ;; load default config
;;   (require 'smartparens-config)
;;   :diminish)

;;; Paredit -- Common for all Lisp modes
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
  :hook ((lisp-mode emacs-lisp-mode clojure-mode) . paredit-mode)
  :diminish)


;;; +++ Misc

;;; Rainbow-Delimiters -- colors parentheses in programming modes.
;; (use-package rainbow-delimiters
;;   :ensure t
;;   :defer t
;;   :init
;;   (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;;   :diminish rainbow-delimiters-mode)


;;; ---
;;; Programming modes


;;; +++ Clojure

;;; Full development environment
(use-package cider
  :ensure t
  :defer t
  :init
  (add-hook 'clojure-mode-hook 'cider-mode))


;;; +++ Guile Scheme

;;; Full development environment
(use-package geiser-guile
  :ensure t
  :defer t
  :hook (scheme-mode . geiser-mode))


;;; +++ Elixir

(when (<= emacs-major-version 29)
    ;; Elixir support is natively added in Emacs 30. So, until then,
    ;; install elixir-mode from MELPA.
    (use-package elixir-mode
      :ensure t
      :defer t
      :mode ("\\.ex\\'" "\\.exs\\'" "mix\\.lock" "\\.elixir\\'")))

;; Major mode for Elixir development with Tree-sitter support. Again,
;; it's a builtin mode starting from Emacs 30, however, there is a
;; backported package for 29 (hence the overlap in the ranges in this
;; config).
(when (>= emacs-major-version 29)
  (use-package elixir-ts-mode
    :ensure t
    :defer t
    :if (sch/treesit-available-p 'elixir)
    :init
    (add-to-list 'major-mode-remap-alist '(elixir-mode . elixir-ts-mode))))

;;; +++ Javascript/Typescript/JSX/TSX

;;; Javascript (builtin)
(use-package js
  :defer t
  :bind (;; JS mode defines its own find-symbol func. It's worthless
	 ;; but shadows the eglot keybinding, so remove it.
	 :map js-mode-map
	 ("M-." . nil)
	 :map js-ts-mode-map
	 ("M-." . nil))
  :config
  (setq js-indent-level 2))

(use-package rjsx-mode
  :ensure t
  :defer t
  :mode ("\\.jsx\\'" "components\\/.*\\.js\\'"))

;;; Typescript (builtin, Tree-Sitter only)
(when (sch/treesit-available-p 'typescript)
  (use-package typescript-ts-mode
    :defer t
    :init
    (push '("\\.ts\\'" . typescript-ts-mode) auto-mode-alist)
    (push '("\\.tsx\\'" . tsx-ts-mode) auto-mode-alist))
  :config
  (setq typescript-ts-mode-indent-offset 2))


;;;  Javascript/Typescript/JSX/TSX (Tree-sitter only)
(when (sch/treesit-available-p 'javascript)
  (use-package jtsx
    :ensure t
    :defer t
    :hook (              ;TODO: Mode this config into Hideshow section
           (jtsx-jsx-mode . hs-minor-mode)
           (jtsx-tsx-mode . hs-minor-mode)
           (jtsx-typescript-mode . hs-minor-mode))
    :init
    (add-to-list 'major-mode-remap-alist '(js-mode . jtsx-jsx-mode))
    ;; `javascript-mode' is an alias of `js-mode' but needs to be
    ;; declared separately. Actually, this is what triggers remap 99% of
    ;; the time.
    (add-to-list 'major-mode-remap-alist '(javascript-mode . jtsx-jsx-mode))
    (add-to-list 'major-mode-remap-alist '(rjsx-mode . jtsx-jsx-mode))
    (add-to-list 'major-mode-remap-alist '(typescript-ts-mode . jtsx-typescript-mode))
    (add-to-list 'major-mode-remap-alist '(tsx-ts-mode . jtsx-tsx-mode))))

(use-package flymake-eslint
  :ensure t
  :defer t
  :hook
  ( ;; The following doesn't work because of
    ;; https://github.com/orzechowskid/flymake-eslint/issues/23
    ;; (js-base-mode typescript-ts-base-mode) . flymake-eslint-enable)
   eglot-managed-mode . (lambda ()
                          (when (derived-mode-p 'js-base-mode 'typescript-ts-base-mode)
                            (flymake-eslint-enable)))))

(use-package prettier-js
  :ensure t
  :defer t
  :commands prettier-js)

;;; +++ Python

(defun sch/python-inline-comment-offset ()
  "Set inline offset for comments in Python buffers."
  (set (make-local-variable 'comment-inline-offset) 2))

(use-package python
  :defer t
  :config
  (add-hook 'python-mode-hook 'sch/python-inline-comment-offset)
  ;; Make indentation compatible with black
  (setq python-indent-def-block-scale 1))


;;; Pyenv
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

					;TODO: Find a way to properly defer activating the global minor mode.
  (global-pyenv-mode 1)
  :bind (("C-c v" . pyenv-use))
  :hook (
	 ;; (python-mode . global-pyenv-mode)
	 (pyenv-mode . sch/restart-eglot-on-pyenv-change)))

;;; Sphinx docstrings generation
(use-package sphinx-doc
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'sphinx-doc-mode)
  :diminish sphinx-doc-mode)

;;; Syntax highlight and fill-paragraph for docstrings.
(use-package python-docstring
  :ensure t
  :defer t
  :init
  (add-hook 'python-mode-hook 'python-docstring-mode)
  :diminish python-docstring-mode)


;;; +++ Groovy

;;; Groovy major mode
(use-package groovy-mode
  :ensure t
  :defer t)


;;; +++ Shell script

;;; Flymake backend for shell scripting
(use-package flymake-shellcheck
  :ensure t
  :defer t
  :init
  (add-hook 'sh-mode-hook 'flymake-shellcheck-load)
  (add-hook 'sh-mode-hook 'flymake-mode))


;;; -------------------------------
;;; Object Notation / Serialization

					;TODO: Add some entries here


;;; -----------
;;; Query modes


;;; +++ SQL

;;; SQL indentation
(use-package sql-indent
  :ensure t
  :defer t
  :hook
  (sql-mode . sqlind-minor-mode))


;;; +++ Rego

;;; Rego major mode
(use-package rego-mode
  :ensure t
  :defer t)


;;; JQ

;;; Major mode for JQ scripts
;;; Also used as dependency for restclient-jq
(use-package jq-mode
  :ensure t
  :defer t
  :mode ("\\.jq\\'"))


;;; ------------
;;; Markup modes

;;; Markdown major mode.
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :defer t)


;;; YAML TS (builtin)
(use-package yaml-ts-mode
  :defer t
  :if (sch/treesit-available-p 'yaml)
  :init
  (add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode)))

;;; -------------
;;; Styling modes

					;TODO; Add some entries here

;;; ----------------
;;; Purpose-specific

;;; +++-
;;; Dockerfile
(use-package dockerfile-mode
  :ensure t
  :defer t
  :config
  (setq dockerfile-enable-auto-indent nil))


;;; ------------------
;;; Organization modes

;;; Org-mode -- Emacs' flawless organize package.
(use-package org
  :defer t
  :bind (("C-c o l" . org-store-link)
	 ("C-c o a" . org-agenda)
	 ("C-c o c" . org-capture)
	 ("C-c o b" . org-switchb)
         :map org-mode-map
         ;; Free up the M-<left/right> binding to work as work
         ;; movement command. For this to happend, whole family of
         ;; commands need to be remapped.
         ("M-<left>" . nil)
         ("C-<left>" . org-metaleft)
         ("M-S-<left>" . org-shiftcontrolleft)
         ("C-S-<left>" . org-shiftleft)
         ("M-<right>" . nil)
         ("C-<right>" . org-metaright)
         ("M-S-<right>" . org-shiftcontrolright)
         ("C-S-<right>" . org-shiftright))
  :config
  ;; Use enhanced exporter if available
  ;; (if (require 'ox-confluence-en nil t)
  ;;     ;; In current work place, Confluence doen't support PlantUML, so
  ;;     ;; disable macro export.
  ;;     (setq ox-confluence-en-use-plantuml-macro nil)
  ;;   (require 'ox-confluence))
  (setq org-directory (file-name-as-directory (expand-file-name "~/org"))
	org-todo-keyword-faces '(("TODO" . org-warning)
				 ("PRG" . "yellow")
				 ("WAIT" . "orange")
				 ("DONE" . org-done))

	;; File to keep the captured items
	org-default-notes-file (concat org-directory "refile.org")

	;; Custom capture templates.
	;; NOTE: This is variable from package org-capture...
	org-capture-templates '(("t" "Task" entry (file "")
				 "* TODO %?\n  Logged on: %u")

				("n" "Note" entry (file "notes.org")
				 "* %?\n  Logged on: %u"))

	;; Agenda config
	org-agenda-files (concat org-directory "agenda_files")
	org-agenda-restore-windows-after-quit t
	org-agenda-todo-list-sublevels nil
	;; In TODO View of agenda, make visible only "open" (with not
	;; active timestamp) TODOs
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

;;; Org community contributions
;; (use-package org-contrib
;;   :straight t
;;   :ensure t
;;   :defer t)


;;; ----------------
;;; Misc major modes


;;; +++ Diagramming

;;; PlantUML major mode.
(use-package plantuml-mode
  :ensure t
  :mode "\\.plantuml\\'"
  :config
  (setq plantuml-default-exec-mode 'executable)
  (setq plantuml-output-type "png")

  ;; Integrate with org-mode editing
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml)))


;;; +++ Document viewer

;;; Epub reader
(use-package nov
  :ensure t
  :defer t
  :mode ("\\.epub\\'" . nov-mode))


;;; +++ HTTP

;;; Clever way of making requests.
(use-package restclient
  :ensure t
  :mode ("\\.restclient\\'" . restclient-mode))

;;; Support for 'jq'-based hooks in 'restclient'
(use-package restclient-jq
  :ensure t
  :if (package-installed-p 'jq-mode)
  :after restclient)


;;; ----------------------------------------------
;;; ------------------------------ Site-specific ;
;;; ----------------------------------------------

;;; * Custom implementation of site-specific configuration
;;; * loading. This allows keeping this init file as generic as
;;; * possible and specify overrides (and maybe keeping/handling
;;; * secrets) depending on the place I'm working from.


(defvar sch/site-config-dir (file-name-as-directory
			     (expand-file-name "site-config"
					       user-emacs-directory))
  "Directory where site-specific configurations reside.")

(defun sch/maybe-load-site-conf (filename)
  "Load site-specific config contained in FILENAME.

FILENAME is searched in the `site-config-dir' dir."
  (let  ((abs-filename (concat sch/site-config-dir
			       filename)))
    (when (file-exists-p abs-filename)
      (load abs-filename))))

;;; Load generic site-specific configurations File is not version
;;; controlled and is missing by default.
;;; _NOTE_: If some requred library is expected to have site-specific
;;; configuration every time, it's better to split it under its own
;;; file under `site-config'.
(sch/maybe-load-site-conf "site-init.el")


;;; ----------------------------------------------
;;; ---------------------------- ( DEPRECATED! ) ;
;;; ----------------------------------------------

;; Left here only to illustrate the idea of `local-config',
;; `local-sources', `local-tempaltes'. I've replaced the
;; `local-config' and `local-templates' with new `site-config' and
;; `site-templates'. There is no alternative for `local-sources' for
;; now because I don't have the case, but maybe `straight' will be a
;; solution for that case.

;; +++
;; Installed from source
;; +++- Orgmine
;; + ++-- Orgmine Dependencies
;; (use-package elmine
;;   :ensure t
;;   :defer t)

;; ----------------------------------
;; Adding local-configs to load-path.
;; (add-to-list 'load-path (file-name-as-directory
;; 			 (expand-file-name "local-configs"
;; 					   user-emacs-directory)))

;; (let ((local-sources-dir (file-name-as-directory
;; 			  (expand-file-name
;; 			   "local-sources"
;; 			   user-emacs-directory))))
;;   ;; Orgmine
;;   (add-to-list 'load-path (file-name-as-directory
;; 			   (expand-file-name
;; 			    "orgmine"
;; 			    local-sources-dir)))
;;   (when (require 'orgmine nil t)
;;     (add-hook 'org-mode-hook
;; 	      (lambda () (if (assoc "om_project" org-file-properties)
;; 			     (orgmine-mode))))
;;     (require 'orgmine-config))

;;   ;; Confluence enhanced exporter
;;   (add-to-list 'load-path (file-name-as-directory
;; 			   (expand-file-name
;; 			    "ox-confluence-en"
;; 			    local-sources-dir))))

;;; init.el ends here.
