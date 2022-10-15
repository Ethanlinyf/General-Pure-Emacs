;;; init-p-julia.el --- Settings for Julia. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;; Commentary:
;; Configurations for Julia programming
;;--------------------------------------------------------------------
;;; Code:

(require 'julia-mode)

(require 'julia-repl)
(add-hook 'julia-mode-hook 'julia-repl-mode) ;; always use minor mode

;;----------------------------------------------------------------------------
(provide 'init-p-julia)
;;; init.el ends here
