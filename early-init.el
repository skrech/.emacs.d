;;; early-init.el --- Early init file

;;; Commentary:
;; Early init file executed before GUI and the package system.

;;; Code:

;;; Don't initialize packages before normal `init.el'. I know what I'm doing!
(setq package-enable-at-startup nil)

;;; early-init.el ends here.
