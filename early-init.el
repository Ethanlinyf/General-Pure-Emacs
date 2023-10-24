;;; early-init.el --- Early initialisation. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin <e.yflin@gmail.com>
;;
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; Settings before loading init.el

;;; Code:
;; Debugging for the setting update.
(setq debug-on-error nil)

;; Garbage collection in the startup process
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for native-comp users
(setq native-comp-deferred-compilation nil)
(setq native-comp-jit-compilation nil)
(setq load-prefer-newer noninteractive)
(setq native-comp-async-report-warnings-errors 'silent)

;; Package initialize occurs automatically, before `user-init-file' is loaded
;; but after `early-init-file'.
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

; ;Suppress GUI features
(setq inhibit-startup-screen t)
(setq use-dialog-box nil)
(setq use-file-dialog nil)
(setq inhibit-splash-screen t) ; Turn off the startup help screen
(setq inhibit-startup-echo-area-message t)
(setq inhibit-startup-screen t)
(setq inhibit-startup-buffer-menu t)
(setq inhibit-x-resources t)
(setq inhibit-default-init t)
(setq inhibit-startup-message t)

;; Default settings for the frame before initialisation
(push '(scroll-bar-mode . nil) default-frame-alist)
(push '(tool-bar-mode . nil) default-frame-alist)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(push '(ns-transparent-titlebar . t) default-frame-alist)
(push '(ns-appearance . dark) default-frame-alist)

;;-------------------------------------------------------------------------------------------------

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; early-init.el ends here.
