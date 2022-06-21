;;; early-init.el --- Early initialisation. -*- lexical-binding: t -*-
;;
;; Copyleft (CL) 2018-2028 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/PureEmacs
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
(let ((minver "28.1"))
(when (version< emacs-version minver)
  (error "Puremacs requires V%s or higher versions." minver)))

;; Garbage collection in the startup process -TE
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Suppress a second case-insensitive search through the auto-mode-alist -TE
(setq auto-mode-case-fold nil)

;; After early-init-file to initialise 'package' -TE
(setq package-enable-at-startup nil)

;; In noninteractive sessions, prioritize non-byte-compiled source files to
;; prevent the use of stale byte-code. Otherwise, it saves us a little IO time
;; to skip the mtime checks on every *.elc file. -- Doom Emacs
(setq load-prefer-newer noninteractive)

;; Inhibit resising Puremacs frame
(setq frame-inhibit-implied-resize t)

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (add-hook 'window-setup-hook
            (lambda ()
              (setq-default inhibit-redisplay nil
                            inhibit-message nil)
              (redisplay)))

;; Default encoding system -TE
(set-language-environment "UTF-8")


;; Remove some warnings
;;(setq load-prefer-newer t)
;;(setq byte-compile-warnings '(cl-functions))



;;----------------------------------------------------------------------
;;; early-init.el ends here -TE
