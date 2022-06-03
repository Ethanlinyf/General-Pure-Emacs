;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;; Commentary:
;; Add feature defined in the lisp folder
;;--------------------------------------------------------------------
;;; Code:

;; Initialise the major mode for scratch
(setq initial-major-mode 'fundamental-mode ;'emacs-lisp-mode
      package--init-file-ensured t)

;; Load the settings recorded through emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) ;; Load the custom file if it exists
  (load custom-file))

;; Define a file to record emacs macros.
(defvar pure-macro (expand-file-name "macros.el" user-emacs-directory)
  "A file to record emacs macros.")

;; Load the recorded emacs macros if it exists
(when (file-exists-p pure-macro) 
  (load pure-macro))

;; Add to list to load the el files in a specific folder;
(defun update-load-pathe (&rest _)
  "To load folders includ el files."
  (dolist (path '("lisp" "site-lisp"))
    (push (expand-file-name path user-emacs-directory) load-path)))
(update-load-pathe)

(require 'init-const)
(require 'init-custom)
(require 'init-emacro)
(require 'init-speedup)
(require 'init-package)
;; (require 'init-dashboard)
(require 'init-abbr)
(require 'init-basic)
(require 'init-ui)
(require 'init-yasnippet)
(require 'init-dired)
(require 'init-treemacs)
(require 'init-org)
(require 'init-tex)
(require 'init-lsp)
(require 'pure-function)
(require 'init-test)

;;----------------------------------------------------------------------------
(provide 'init)
;;; init.el ends here
