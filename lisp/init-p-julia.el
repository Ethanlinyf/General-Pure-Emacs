;;; init-julia.el --- Settings for Julia. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for Julia programming
;;--------------------------------------------------------------------
;;; Code:

(use-package julia-mode
  :ensure t)

(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode))

;;-------------------------------------------------------------------------------------------------
(provide 'init-p-julia)
;;; init-p-julia.el ends here.
