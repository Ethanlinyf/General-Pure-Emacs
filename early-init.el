;;; early-init.el --- Early initialisation. -*- lexical-binding: t; -*-

;;
;; Copyleft (CL) 2018-2028 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; Settings before loading init.el

;;; Code:
;; Debugging for the setting update.
(setq debug-on-error nil)

;; Garbage collection in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; Prevent unwanted runtime compilation for native-comp users
(setq native-comp-deferred-compilation nil)
(setq load-prefer-newer noninteractive)

;; Package initialize occurs automatically, before `user-init-file' is loaded
;; but after `early-init-file'.
(setq package-enable-at-startup nil)

;; Inhibit frame resizing
(setq frame-inhibit-implied-resize t)
(setq inhibit-startup-screen t)

;; Default settings for the frame before initialisation
(push '(scroll-bar-mode . nil) default-frame-alist)
(push '(tool-bar-mode . nil) default-frame-alist)

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Turn off the startup help screen
(setq inhibit-splash-screen t)

;;-------------------------------------------------------------------------------------------------

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; early-init.el ends here.
