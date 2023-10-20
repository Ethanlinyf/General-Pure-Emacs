;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; init file for General Pure Emacs basic branch
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

;; set gc-threshold after init
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 20 1024 1024))))

;; Initialise the major mode for scratch, fundamental-mode or text-mode
(setq initial-major-mode 'text-mode);
(setq-default major-mode 'text-mode
              fill-column 70
              tab-width 4
              indent-tabs-mode nil)

;; Load the settings recorded through Emacs
(defconst custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (shell-command (concat "touch " custom-file)))
(when (file-exists-p custom-file)
  (load custom-file :noerror :nomessage))

;; Define a file to record emacs macros.
(defvar gpe-macro (expand-file-name "core/gpe-macros.el" user-emacs-directory)
  "A file to record Emacs macros.")
(unless (file-exists-p gpe-macro)
  (shell-command (concat "touch " gpe-macro)))
;; Load the macro file if it exists
(when (file-exists-p gpe-macro)
  (load gpe-macro :noerror :nomessage))

;; Personal settings.
(defvar gpe-u-setting (expand-file-name "gpe-u-setting.el" user-emacs-directory)
  "A file with personal settings.")
(unless (file-exists-p  gpe-u-setting)
  (shell-command (concat "touch " gpe-u-setting)))
;; Load the personal setting file if it exists
(when (file-exists-p gpe-u-setting)
  (load gpe-u-setting :noerror :nomessage))

;; Add to list to load the el files in a specific folder;
(defun update-load-path (&rest _)
  "To update 'load-path'."
  (dolist (path '("core" "site-lisp"))
    (push (expand-file-name path user-emacs-directory) load-path)))

(defun add-extradirs-to-load-path (&rest _)
  "Include extra dirs to 'load-path'."
  (let ((default-directory (expand-file-name "module" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-extradirs-to-load-path)
(update-load-path)

;; (require 'init-0-bridge)
;; (require 'init-1-system)

;; (require 'init-a-authentication)
;; (require 'init-b-basic)
;; (require 'init-c-i18n)
;; (require 'init-d-update)

;; (require 'init-e-enhance)
;; (require 'init-f-dired)
;; (require 'init-g-interface)
;; (require 'init-h-dashboard)
;; (require 'init-i-minibuffer)
;; (require 'init-j-platform)
;; (require 'init-k-org)
;; (require 'init-l-markdown)

;; (require 'init-p-python)
;; (require 'init-p-cpp)
;; (require 'init-p-lua)
;; (require 'init-p-julia)
;; (require 'init-p-web)
;; (require 'init-p-treesit)

;; (require 'init-r-roam)
;; (require 'init-r-research)
;; (require 'init-r-tex)

;; (require 'init-z-test)

;;-------------------------------------------------------------------------------------------------
;; init.el ends here.
