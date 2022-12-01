;;; bridge-engine.el --- Pligins for GPEmacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Add needed plugins to build General Pure Emacs if you are not using use-package
;;--------------------------------------------------------------------
;;; Code:

;; (require 'cl)
(require 'package)

(when (>= emacs-major-version 28)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  )
(package-initialize)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; Add whatever packages you want here
(defvar puremacs/packages '(
			    org-superstar
                                 )  "Default packages.")

(setq package-selected-packages puremacs/packages)

(defun puremacs/packages-installed-p ()
  "Looping all the packages."
  (cl-loop for pkg in puremacs/packages
	when (not (package-installed-p pkg)) do (cl-return nil)
	finally (cl-return t)))

(unless (puremacs/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg puremacs/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;--------------------------------------------------------------------
(provide 'bridge-engine)
;;; init-0-plugin.el ends here
