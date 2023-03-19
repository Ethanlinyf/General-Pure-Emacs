;;; early-init.el --- Early initialisation. -*- lexical-binding: t -*-
;;
;; Copyleft (CL) 2018-2028 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;----------------------------------------------------------------------
;;; Commentary:
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialisation happens.
;; Emacs 29 supports native compilation.
;;----------------------------------------------------------------------
;;; Code:

;; Debugging for the setting update.
(setq debug-on-error nil)
(when init-file-debug
  (profiler-start 'cpu)
  (add-hook 'window-setup-hook #'profiler-stop 0))

;; Puremacs is compatible from the emacs version 27.1.
(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "The GPE requires V%s or higher versions" minver)))

;; Garbage collection in the startup process
;; (setq gc-cons-threshold most-positive-fixnum
;;       gc-cons-percentage 0.5)
;; (add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.5)

;; Prevent unwanted run-time compilation for native-comp users
(when (>= emacs-major-version 29)
  (setq inhibit-automatic-native-compilation nil))

;; Suppress a second case-insensitive search through the auto-mode-alist
(setq auto-mode-case-fold nil)

(setq use-file-dialog nil)

;; After early-init-file to initialise 'package'. Make initialization
;; slightly faster See the (package-initialize) in the file
;; init-0-bridge.el, which make the initiation will be executed just
;; once.
(setq package-enable-at-startup nil)
;; Prevent loading from the package cache (same reason).
(setq package-quickstart nil)

;; In noninteractive sessions, prioritise non-byte-compiled source
;; files to prevent the use of stale byte-code. Otherwise, it saves us
;; a little IO time to skip the time checks on every *.elc file.
;; from Doom Emacs
(setq load-prefer-newer noninteractive)

;; Inhibit resizing Puremacs frame
(setq frame-inhibit-implied-resize t)

;; Remove some warnings
(setq byte-compile-warnings nil)

;; Default settings for the frame before initialisation
;; To prevent the glimpse of un-styled Emacs by disabling the following UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Turn off the startup help screen
(setq inhibit-splash-screen t)

;; Prevent flash of un-styled modeline at startup
(setq-default mode-line-format nil)
(fset 'display-startup-echo-area-message 'ignore)

;;----------------------------------------------------------------------
;;; early-init.el ends here.
