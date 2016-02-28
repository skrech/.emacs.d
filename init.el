;;; init.el --- Emacs configuration of Kristiyan Kanchev

;;; Commentary:
;; Emacs configuration of Kristiyan Kanchev

;;; Code:

;; ------------------------
;; Emacs Customization file
(setq custom-file (expand-file-name "customize.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;; ---------
;; Built-ins

;; Config for built-ins
(setq column-number-mode t)           ; show column number in modeline
(global-linum-mode t)                 ; display line numbers
(setq scroll-conservatively 10000)    ; allow to scrol line-by-line
(ido-mode t)                          ; enabe IDO mode
(add-hook 'before-save-hook
	  'delete-trailing-whitespace)	       ; delete whitespaces
(put 'dired-find-alternate-file 'disabled nil) ; reuse dired buffers
(which-function-mode 1)		      ; shows current function name
(defalias 'yes-or-no-p 'y-or-n-p)     ; don't ask newbie questions

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

;; Smex
(use-package smex
 :ensure t
 :bind ("M-x" . smex))

;; Which key
(use-package which-key
  :ensure t
  :init (which-key-mode)
  :diminish (which-key-mode . " Ⓚ"))

;; Company-mode
(use-package company
  :ensure t
  :init (global-company-mode)
  :diminish (company-mode . " ⓐ"))

;; Flycheck
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :diminish (flycheck-mode . " Ⓢ"))

;; Flycheck pos-tip
(use-package flycheck-pos-tip
  :ensure t
  :init (with-eval-after-load 'flycheck
	  (flycheck-pos-tip-mode)))

;; +++
;; Deffered

;; ++
;; Python

;; Python mode
(use-package python
  :defer t
  :config
  (progn
    (add-hook 'python-mode-hook 'subword-mode)
    (add-hook 'python-mode-hook '(diminish 'subword-mode))
    (add-hook 'python-mode-hook
    	      (lambda () (set (make-local-variable
    			       'comment-inline-offset) 2)))
    (add-hook 'python-mode-hook (lambda() (setq fill-column 79)))
    )
  )


;; Anaconda-mode
(use-package anaconda-mode
  :ensure t
  :defer t
  :init (add-hook 'python-mode-hook 'anaconda-mode)
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

;; ----
;; Misc

;; Append MinGW to PATH on Windows
(when (or (eq system-type 'windows-nt) (eq system-type 'msdos))
  (setenv "PATH" (concat "C:\\MinGW\\msys\\1.0\\bin;" (getenv "PATH"))))

;;; init.el ends here
