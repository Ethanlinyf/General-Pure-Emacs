;;; early-init.el --- Early initialisation. -*- lexical-binding: t -*-
;;
;; Copyleft (CL) 2018-2028 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; Settings before loading init.el

;;; Code:
;; Debugging for the setting update.
(setq debug-on-error nil)

;; Puremacs is compatible from the emacs version 29.1.
(let ((minver "29.1"))
  (when (version< emacs-version minver)
    (error "The GPE Baisc requires V%s or higher versions" minver)))

;; Garbage collection in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

;; Prevent unwanted run-time compilation for native-comp users
(when (>= emacs-major-version 29)
  (setq inhibit-automatic-native-compilation t))

;; After early-init-file to initialise 'package'. Make initialisation slightly faster
(setq package-enable-at-startup nil)
;; Prevent loading from the package cache (same reason).
(setq package-quickstart nil)

;; In noninteractive sessions, prioritise non-byte-compiled source
;; files to prevent the use of stale byte-code. Otherwise, it saves us
;; a little IO time to skip the time checks on every *.elc file.
(setq load-prefer-newer noninteractive)

;; Inhibit frame resizing
(setq frame-inhibit-implied-resize t)

;; Ignore compile warnings
(setq byte-compile-warnings nil)

;; Default settings for the frame before initialisation
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; not compile on this stage
(setq comp-deferred-compilation nil)

;; Turn off the startup help screen
(setq inhibit-splash-screen t)

(fset 'display-startup-echo-area-message 'ignore)

;;-------------------------------------------------------------------------------------------------
;;; early-init.el ends here.
