;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;; Commentary:
;; Add feature defined in the lisp folder
;;--------------------------------------------------------------------
;;; Code:

(setq
 ;; initial-major-mode 'fundamental-mode
 initial-major-mode 'emacs-lisp-mode
 package--init-file-ensured t)


(require 'init-const)
(require 'init-custom)

(require 'init-package)

(require 'init-dashboard)

(require 'init-abbr)
(require 'init-basic)

(require 'init-ui)
(require 'init-org)
(require 'init-tex)
(require 'init-yasnippet)
;;(require 'init-org-site)
;;(require 'init-twbs)
(require 'init-dired)

(require 'pure-function)
(require 'init-test)
(require 'init-treemacs)
(require 'init-lsp)

;;----------------------------------------------------------------------------
(provide 'init)
;;; init.el ends here
