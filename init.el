;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; init file for General Pure Emacs basic branch

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
  "To update the path to be loaded."
  (dolist (path '("core" "extension" "module/platform" "module/practice"))
    (push (expand-file-name path user-emacs-directory) load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(require 'init-system)
(require 'init-bridge)
(require 'init-builtin)
(require 'init-enhancement)

(require 'mpf-minibuffer)
(require 'mpf-completion)
(require 'mpf-userinterface)
(require 'mpf-workspace)

(require 'mpc-org)
(require 'mpc-tex)
(require 'mpc-python)
(require 'mpc-markdown)

;;-------------------------------------------------------------------------------------------------
(provide 'init)
;;; init.el ends here.
