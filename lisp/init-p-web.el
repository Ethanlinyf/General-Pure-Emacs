;;; init-p-ts.el --- Settings for Typecript. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for Web programming
;;--------------------------------------------------------------------
;;; Code:

;; Tide, TypeScript Interactive Development Environment for Emacs: https://github.com/ananthakumaran/tide/
;; ts-comint, a Typescript REPL in Emacs: https://github.com/emacs-typescript/typescript.el
(use-package typescript-mode
  :ensure t
  :mode ("\\.ts[x]\\'" . typescript-mode))

(use-package json-mode
  :ensure t)

;;-------------------------------------------------------------------------------------------------
(provide 'init-p-web)
;;; init-p-web.el ends here
