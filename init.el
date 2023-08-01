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

;; set gc-threshold after init
(add-hook 'after-init-hook #'(lambda () (setq gc-cons-threshold (* 20 1024 1024))))

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

;; Initialise the major mode for scratch, fundamental-mode or text-mode
;; Prevent setting it as a rich mode, such as org-mode, which will
;; slow down the sartup speed.
(setq initial-major-mode 'text-mode);

;; (package--init-file-ensured t)

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

;; Define a file to record emacs macros.
(defvar pure-macro (expand-file-name "site-lisp/pure-macros.el" user-emacs-directory)
  "A file to record Emacs macros.")
;; Load the macro file if it exists
(when (file-exists-p pure-macro)
  (load pure-macro :noerror :nomessage))

;; Personal settings.
(defvar pure-p-setting (expand-file-name "init-p-setting.el" user-emacs-directory)
  "A file with personal settings.")
;; Load the personal setting file if it exists
(when (file-exists-p pure-p-setting)
  (load pure-p-setting :noerror :nomessage))

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
(require 'init-c-i18n)
(require 'init-d-update)

(require 'init-e-enhance)
(require 'init-f-dired)
(require 'init-g-interface)
(require 'init-h-dashboard)
(require 'init-i-minibuffer)
(require 'init-j-platform)
(require 'init-k-org)
(require 'init-l-roam)

(require 'init-p-cpp)
(require 'init-p-python)
(require 'init-p-lua)
(require 'init-p-julia)
(require 'init-p-web)

(require 'init-r-research)
(require 'init-r-tex)

(require 'init-z-test) ; for testing purposes

;;-------------------------------------------------------------------------------------------------
;; init.el ends here.
