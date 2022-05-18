;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;; Commentary:
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;--------------------------------------------------------------------
;;; Code:

;; For debugging purposes
(setq debug-on-error t)

;; Puremacs is based on the version 27.1 or above ;
(let ((minver "27.1"))
(when (version< emacs-version minver)
  (error "Puremacs requires V%s or higher." minver)))

;; Add to list to load the el files in a specific folder;
(defun update-load-pathe (&rest _)
  "To load folders includ el files."
  (dolist (path '("lisp" "site-lisp"))
    (push (expand-file-name path user-emacs-directory) load-path)))
(update-load-pathe)

;; for Emacs 28.1
;; Silence compiler warnings as they can be pretty disruptive
;;(when (native-comp-available-p) 
;;  (setq borg-compile-function #'native-compile))

;; This file is used to record the settings through emacs
(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
(when (file-exists-p custom-file) ;; Load the custom file if it exists
  (load custom-file))

;; Define a file to record emacs macros.
(defvar pure-file (expand-file-name "lisp/pure.el" user-emacs-directory)
  "A file to record emacs macros.")

;; Load the recorded emacs macros if it exists
(when (file-exists-p pure-file) 
  (load pure-file))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

(require 'init-speedup)

;; Remove some warnings
(setq load-prefer-newer t)
(setq byte-compile-warnings '(cl-functions))

;;--------------------------------------------------------------------
(provide 'early-init)
;;; early-init.el ends here
