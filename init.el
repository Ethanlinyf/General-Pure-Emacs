;;; init.el --- The main entry of Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin <e.yflin@gmail.com>
;;
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; init file for General Pure Emacs basic branch

;;; Code:
(require 'cl-lib)

(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold 800000)))

;; Startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract
                               after-init-time before-init-time)))
                     gcs-done)))

;; Enhance the smoothness of Emacs startup
(when (display-graphic-p)
  (setq-default inhibit-redisplay t
                inhibit-message t)
  (defun reset-inhibit-vars ()
    (setq-default inhibit-redisplay nil
                  inhibit-message nil)
    (redraw-frame))
  (add-hook 'window-setup-hook #'reset-inhibit-vars)
  (define-advice startup--load-user-init-file (:after (&rest _) reset-inhibit-vars)
    (and init-file-had-error (reset-inhibit-vars))))

(unless (or (daemonp) noninteractive init-file-debug)
  ;; Optimise the file handlers operations at startup (from Centaur Emacs).
  ;; `file-name-handler-alist' is consulted on each call to `require' and `load'
  (let ((old-value file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (set-default-toplevel-value 'file-name-handler-alist file-name-handler-alist)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "To recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist old-value))))
              101)))

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
(when (file-exists-p gpe-u-setting)
  (load gpe-u-setting :noerror :nomessage))

;; Add to list to load the el files in a specific folder;
(defun update-load-path (&rest _)
  "To update the path to be loaded."
  (dolist (path '("core" "extension" "module/platform" "module/practice"))
    (push (expand-file-name path user-emacs-directory) load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(update-load-path)

(setq-default make-backup-files nil)

(require 'init-e-system)
(require 'init-n-bridge)
(require 'init-s-builtin)
(require 'init-w-enhance)

(require 'mpf-en-userinterface)
(require 'mpf-es-completion)
(require 'mpf-wn-minibuffer)
(require 'mpf-ws-workspace)

(require 'mpc-000-org)
(require 'mpc-001-markdown)
(require 'mpc-010-python)
(require 'mpc-011-tex)

;;-------------------------------------------------------------------------------------------------
(provide 'init)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init.el ends here.
