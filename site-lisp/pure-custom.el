;;; pure-custom.el --- Custom set varaiables by hand. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Setting variables by hand
;;--------------------------------------------------------------------
;;; Code:

(when *is-mac*
  (set-face-attribute 'default nil :font "Menlo-16" ))


;; Add when is-a-mac.
;; ExecPath: https://www.emacswiki.org/emacs/ExecPath
(when *is-mac*
  (setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
  (setq exec-path (append exec-path '("/opt/homebrew/bin"))))


;; (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;; (add-to-list 'default-frame-alist '(ns-appearance . dark))

(add-to-list 'default-frame-alist '(drag-internal-border . 1))
(add-to-list 'default-frame-alist '(internal-border-width . 5))

;;-------------------------------------------------------------------------------------------------
(provide 'pure-custom)
;;; init-custom.el ends here
