;;; init-lua.el -- For lua programming language. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for lua programming language for Neovim settings
;;--------------------------------------------------------------------
;;; Code:

(use-package lua-mode
  :ensure t
  :magic ("\\.lua$" . lua-mode)
  :config
  (add-to-list 'interpreter-mode-alist '("lua" . lua-mode)))

;;-------------------------------------------------------------------------------------------------
(provide 'init-p-lua)
;;; init-p-lua.el ends here.
