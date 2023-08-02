;;; init-p-cpp.el --- Settings for C/C++. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for C/C++ programming
;;--------------------------------------------------------------------
;;; Code:

(use-package eglot
  :ensure t
  :config
  ;(require 'eglot)
  (add-to-list 'eglot-server-programs '((c++-mode c-mode) "clangd"))
  (add-hook 'c-mode-hook #'eglot-ensure)
  (add-hook 'c++-mode-hook #'eglot-ensure))

(use-package quickrun
    :ensure t
    :commands (quickrun)
    :config
    (quickrun-add-command "c++/c1z"
    '((:command . "g++")
	(:exec . ("%c -std=c++1z %o -o %e %s"
		"%e %a"))
	(:remove . ("%e")))
    :default "c++")
    :bind ("<f5>" . quickrun))

;; (global-set-key (kbd "<f5>") 'quickrun)

;;--------------------------------------------------------------------------------------------------
(provide 'init-p-cpp)
;;; init-p-cpp.el ends here
