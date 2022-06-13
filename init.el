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

;; speedup Emacs after sartup
;; (defvar better-gc-cons-threshold (* 8 1024 1024)
;;   "The default value for `gc-cons-threshold'.
;; If freezing, decrease it. If stuttering, increase it.")

;; (defvar startup/file-name-handler-alist file-name-handler-alist)
;; (setq file-name-handler-alist nil)

;; (defun startup/revert-file-name-handler-alist ()
;;   (setq file-name-handler-alist startup/file-name-handler-alist))

;; (defun startup/reset-gc ()
;;   (setq gc-cons-threshold better-gc-cons-threshold
;; 	gc-cons-percentage 0.1))

;; (add-hook 'emacs-startup-hook 'startup/revert-file-name-handler-alist)
;; (add-hook 'emacs-startup-hook 'startup/reset-gc)

;; (defun gc-minibuffer-setup-hook ()
;;   (setq gc-cons-threshold (* better-gc-cons-threshold 2)))

;; (defun gc-minibuffer-exit-hook ()
;;   (garbage-collect)
;;   (setq gc-cons-threshold better-gc-cons-threshold))

;; (add-hook 'minibuffer-setup-hook #'gc-minibuffer-setup-hook)
;; (add-hook 'minibuffer-exit-hook #'gc-minibuffer-exit-hook)

;; (add-hook 'emacs-startup-hook
;;           (lambda ()
;;             (if (boundp 'after-focus-change-function)
;;                 (add-function :after after-focus-change-function
;;                               (lambda ()
;;                                 (unless (frame-focus-state)
;;                                   (garbage-collect))))
;; (add-hook 'after-focus-change-function 'garbage-collect))))

;; (setq auto-mode-case-fold nil)

(unless (or (daemonp) noninteractive)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    ;; If `file-name-handler-alist' is nil, no 256 colors in TUI
    ;; @see https://emacs-china.org/t/spacemacs-centaur-emacs/3802/839
    (setq file-name-handler-alist
          (unless (display-graphic-p)
            '(("\\(?:\\.tzst\\|\\.zst\\|\\.dz\\|\\.txz\\|\\.xz\\|\\.lzma\\|\\.lz\\|\\.g?z\\|\\.\\(?:tgz\\|svgz\\|sifz\\)\\|\\.tbz2?\\|\\.bz2\\|\\.Z\\)\\(?:~\\|\\.~[-[:alnum:]:#@^._]+\\(?:~[[:digit:]]+\\)?~\\)?\\'" . jka-compr-handler))))
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
(when (file-exists-p custom-file) ;; Load the custom file if it exists
  (load custom-file))

;; Define a file to record emacs macros.
(defvar pure-macro (expand-file-name "macros.el" user-emacs-directory)
  "A file to record emacs macros.")

;; Load the recorded emacs macros if it exists
(when (file-exists-p pure-macro) 
  (load pure-macro))

;; Add to list to load the el files in a specific folder;
(defun update-load-pathe (&rest _)
  "To load folders includ el files."
  (dolist (path '("lisp" "site-lisp"))
    (push (expand-file-name path user-emacs-directory) load-path)))
(update-load-pathe)

;; load with a sequence
(require 'init-a-abbr)
(require 'init-b-basic)
(require 'init-c-package)
(require 'init-e-ui)
(require 'init-e-dired)
(require 'init-e-treemacs)
(require 'init-f-dashboard)
(require 'init-g-yasnippet)
;; (require 'init-h-lsp)
;; (require 'init-i-org)
;; (require 'init-i-roam)
;; (require 'init-i-tex)
;; (require 'init-i-python)
;; (require 'init-j-purefunction)
;; (require 'init-z-test)

;;----------------------------------------------------------------------------
(provide 'init)
;;; init.el ends here
