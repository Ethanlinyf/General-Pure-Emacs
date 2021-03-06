;;; early-init.el --- Early initialisation. -*- lexical-binding: t -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;----------------------------------------------------------------------
;; Commentary:
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialisation happens.
;;----------------------------------------------------------------------
;;; Code:

;; Debugging for the setting update. -TE
(setq debug-on-error t)

;; Puremacs is compatible from the emacs version 27.1 -TE
(let ((minver "27.1"))
(when (version< emacs-version minver)
  (error "Puremacs requires V%s or higher versions." minver)))

;; Garbage collection in the startup process -TE
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Default character encoding -TE
(set-language-environment "UTF-8")

;; After early-init-file to initialise 'package' -TE
(setq package-enable-at-startup nil)

;; Inhibit resising Puremacs frame
(setq frame-inhibit-implied-resize t)

;; Remove some warnings
(setq load-prefer-newer t)
(setq byte-compile-warnings '(cl-functions))

;; Suppress a second case-insensitive search through the auto-mode-alist -TE
(setq auto-mode-case-fold nil)

;;----------------------------------------------------------------------
;;; early-init.el ends here -TE
