;;; init-bridge --- To Install Plugins for GPEmacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; How to add plugins, add pacakges or by use-package
;;--------------------------------------------------------------------
;;; Code:

;; (require 'cl)
(require 'package)

(when (>= emacs-major-version 26)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  )
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;;--------------------------------------------------------------------
;; Another way to manage packages
;;--------------------------------------------------------------------
;; (defvar puremacs/packages '(
                            
;; 			    use-package
                            
;;                             )  "Default packages.")

;; (setq package-selected-packages puremacs/packages)

;; (defun puremacs/packages-installed-p ()
;;   "Looping all the packages."
;;   (cl-loop for pkg in puremacs/packages
;; 	   when (not (package-installed-p pkg)) do (cl-return nil)
;; 	   finally (cl-return t)))

;; (unless (puremacs/packages-installed-p)
;;   (message "%s" "Refreshing package database...")
;;   (package-refresh-contents)
;;   (dolist (pkg puremacs/packages)
;;     (when (not (package-installed-p pkg))
;;       (package-install pkg))))

;;--------------------------------------------------------------------
;; use-package with straight
;;--------------------------------------------------------------------
;; (setq package-check-signature t
;;        load-prefer-newer t)

;; ;; Install straight.el
;; (defvar bootstrap-version)
;; (let ((bootstrap-file
;;        (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
;;       (bootstrap-version 5))
;;   (unless (file-exists-p bootstrap-file)
;;     (with-current-buffer
;;         (url-retrieve-synchronously
;;          "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
;;          'silent 'inhibit-cookies)
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   (load bootstrap-file nil 'nomessage))

;;--------------------------------------------------------------------
;; Setup 'use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

(setq use-package-compute-statistics t)

;; Required by `use-package', as use-package optional dependency
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update
  :ensure t)

(use-package things-engine
  :ensure nil
  :init
  (message "Something Good as Indicated"))

;;--------------------------------------------------------------------
;;  (straight-use-package 'use-package
;;  (use-package straight
;;   :custom (straight-use-package-by-default t))
;;   :bind  (("C-<f2>" . hydra-straight-helper/body)))

;;--------------------------------------------------------------------
(provide 'init-0-bridge)
;;; init-0-bridge.el ends here
