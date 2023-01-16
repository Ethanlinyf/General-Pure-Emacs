;;; early-init.el --- Early initialisation. -*- lexical-binding: t -*-
;;
;; Copyleft (CL) 2018-2028 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/PureEmacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;----------------------------------------------------------------------
;;; Commentary:
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialisation happens.
;;----------------------------------------------------------------------
;;; Code:

;; Debugging for the setting update.
(setq debug-on-error nil)

;; Puremacs is compatible from the emacs version 27.1.
(let ((minver "26.1"))
(when (version< emacs-version minver)
  (error "Puremacs requires V%s or higher versions" minver)))

(when (version< emacs-version "27.1")
  (message "Your Emacs is old, and some functionality in this config will be disabled. Please upgrade to 28.2 at least if possible."))

;; Garbage collection in the startup process
;; (setq gc-cons-threshold most-positive-fixnum
;;       gc-cons-percentage 0.5)
;; (setq gc-cons-threshold most-positive-fixnum)
;; (add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))
(setq gc-cons-threshold most-positive-fixnum)

;; Prevent unwanted runtime compilation for native-comp users
(setq native-comp-deferred-compilation nil)

;; Suppress a second case-insensitive search through the auto-mode-alist
(setq auto-mode-case-fold nil)

;; After early-init-file to initialise 'package'. Make initialization
;; slightly faster See the (package-initialize) in the file
;; init-a-engine.el, which make the initiation will be executed just
;; once.
(setq package-enable-at-startup nil)

; (add-hook 'after-init-hook (lambda () (load-theme 'doom-one)))

;; In noninteractive sessions, prioritise non-byte-compiled source
;; files to prevent the use of stale byte-code. Otherwise, it saves us
;; a little IO time to skip the mtime checks on every *.elc file. 
;; from Doom Emacs
(setq load-prefer-newer noninteractive)

;; Inhibit resising Puremacs frame
(setq frame-inhibit-implied-resize t)

;; To suppress flashing at startup
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; Remove some warnings
(setq byte-compile-warnings '(cl-functions))

;; Default settings for the frame before initialisation
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Turn off the startup help screen
(setq inhibit-splash-screen 1)
;;----------------------------------------------------------------------
;;; early-init.el ends here
