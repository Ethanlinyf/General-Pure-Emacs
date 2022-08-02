;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;; Commentary:
;; Add feature defined in the lisp folder
;;--------------------------------------------------------------------
;;; Code:

(setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 80000000
                  gc-cons-percentage 0.1)))

(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract
                               after-init-time before-init-time)))
                     gcs-done)))


;; Initialise the major mode for scratch
(setq initial-major-mode 'emacs-lisp-mode ;;'fundamental-mode
      package--init-file-ensured t)

;; Load the settings recorded through emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Load the custom file if it exists
(when (file-exists-p custom-file) 
  (load custom-file))

;; Define a file to record emacs macros.
(defvar pure-macro (expand-file-name "macros.el" user-emacs-directory)
  "A file to record emacs macros.")
;; Load the macro file if it exists
(when (file-exists-p pure-macro) 
  (load pure-macro))

;; Add to list to load the el files in a specific folder;
(defun update-load-pathe (&rest _)
  "To load folders includ el files."
  (dolist (path '("lisp" "site-lisp"))
    (push (expand-file-name path user-emacs-directory) load-path)))
(update-load-pathe)

;; load in a order
(require 'init-a-abbr)
(require 'init-b-basic)
(require 'init-c-package)
(require 'init-d-enhance)
(require 'init-e-ui)
(require 'init-e-dired)
(require 'init-e-treemacs)
(require 'init-f-dashboard)
(require 'init-g-yasnippet)
(require 'init-h-lsp)
(require 'init-i-org)
(require 'init-i-roam)
(require 'init-i-tex)
(require 'init-i-python)
(require 'init-j-purefunction)
(require 'init-z-test)

;;----------------------------------------------------------------------------
(provide 'init)
