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

;; Startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract
                               after-init-time before-init-time)))
                     gcs-done)))

;; set the startup default directory
;(setq default-directory "~/")
(setq user-emacs-directory "~/.emacs.d/")

;; Initialise the major mode for scratch, fundamental-mode or text-mode
;; Prevent setting it as a rich mode, such as org-mode, which will 
;; slow down the sartup speed. 
(setq initial-major-mode 'text-mode);
      ;package--init-file-ensured t)

;; Load the settings recorded through emacs
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; Load the custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

;; Define a file to record emacs macros.
(defvar pure-macro (expand-file-name "site-lisp/pure-macros.el" user-emacs-directory)
  "A file to record Emacs macros.")
;; Load the macro file if it exists
(when (file-exists-p pure-macro)
  (load pure-macro))

;; Personal settings for GPE.
(defvar pure-individual (expand-file-name "init-individual.el" user-emacs-directory)
  "A file to record Emacs macros.")
;; Load the personal setting file if it exists
(when (file-exists-p pure-individual)
  (load pure-individual))

;; Add to list to load the el files in a specific folder;
(defun update-load-path (&rest _)
  "To update 'load-path'"
  (dolist (path '("lisp" "site-lisp"))
    (push (expand-file-name path user-emacs-directory) load-path)))

(defun add-extradirs-to-load-path (&rest _)
  "Include extra dirs to 'load-path'"
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-extradirs-to-load-path)

(update-load-path)

;(require 'init-0-plugin)
(require 'init-a-engine)

(require 'init-b-basic)
(require 'init-c-minibuffer)
(require 'init-d-dired)

(require 'init-e-enhance)
(require 'init-f-platform)

(require 'init-g-interface)
(require 'init-h-dashboard)

(require 'init-i-org)
(require 'init-i-roam)
(require 'init-i-tex)
(require 'init-i-python)
(require 'init-i-lua)
(require 'init-p-julia)

;; (require 'init-z-test)

;;----------------------------------------------------------------------------
;; (provide 'init)
;;; init.el ends here
