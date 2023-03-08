;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; init file for General Pure Emacs
;;--------------------------------------------------------------------
;;; Code:

(require 'cl-lib)

;; Startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract
                               after-init-time before-init-time)))
                     gcs-done)))

(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; set the startup default directory,
;; for the generic, it can be set as defaults
;; for the specific, you could change to as you want after initiation.
(setq default-directory "~/")
(setq user-emacs-directory "~/.emacs.d/")

;; Initialise the major mode for scratch, fundamental-mode or text-mode
;; Prevent setting it as a rich mode, such as org-mode, which will
;; slow down the sartup speed.
(setq initial-major-mode 'text-mode);
      ;package--init-file-ensured t)

;; Load the settings recorded through Emacs
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (shell-command (concat "touch " custom-file)))
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Load the custom file if it exists
;; (load custom-file t), or
(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Suppress file handlers operations at startup (from Centaur Emacs).
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

;; Define a file to record emacs macros.
(defvar pure-macro (expand-file-name "site-lisp/pure-macros.el" user-emacs-directory)
  "A file to record Emacs macros.")
;; Load the macro file if it exists
(when (file-exists-p pure-macro)
  (load pure-macro :noerror :nomessage))

;; Personal settings for GPE.
(defvar pure-individual (expand-file-name "init-individual.el" user-emacs-directory)
  "A file to record Emacs macros.")
;; Load the personal setting file if it exists
(when (file-exists-p pure-individual)
  (load pure-individual :noerror :nomessage))

;; Add to list to load the el files in a specific folder;
(defun update-load-path (&rest _)
  "To update 'load-path'."
  (dolist (path '("lisp" "site-lisp"))
    (push (expand-file-name path user-emacs-directory) load-path)))

;; (defun add-extradirs-to-load-path (&rest _)
;;   "Include extra dirs to 'load-path'."
;;   (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
;;     (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
;; (advice-add #'package-initialize :after #'add-extradirs-to-load-path)

(update-load-path)

(require 'init-0-bridge)
(require 'init-1-system)

(require 'init-a-authentication)
(require 'init-b-basic)
(require 'init-c-minibuffer)
(require 'init-d-dired)

(require 'init-e-enhance)
(require 'init-f-platform)
(require 'init-g-interface)
(require 'init-h-dashboard)
(require 'init-i-i18n)
(require 'init-o-org)
;; (require 'init-o-roam)

(require 'init-p-python)
(require 'init-p-lua)
;; (require 'init-p-julia)
(require 'init-p-web)

(require 'init-r-research)
(require 'init-r-tex)

(require 'init-u-update)

;; (require 'init-z-test) ;; for debugging purposes

;;----------------------------------------------------------------------------
;;; init.el ends here.
