;;; init.el ---Testing purposes for Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;; Commentary:
;; This is a test file for Puremacs
;;--------------------------------------------------------------------
;;; Code:

(push '(vertical-scroll-bars) default-frame-alist)

(auto-image-file-mode 1)

;;--------------------------------------------------------------------
(provide 'init-z-test)
;;; init-test.el ends here



;;; The following are the testing examples
;;; Backup examples:

;;----- in init.el -----
;; (add-hook 'after-init-hook
;;           (lambda () (require 'server)
;;             (unless (server-running-p) (server-start))))

;;(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;(advice-add #'package-initialize :after #'update-load-path)

;;; --- in early-init.el ---
;; Faster to disable these here (before they've been initialized)
;; (push '(menu-bar-lines . 0) default-frame-alist)
;; (push '(tool-bar-lines . 0) default-frame-alist)
;; (push '(vertical-scroll-bars) default-frame-alist)
;; (when (featurep 'ns)
;;   (push '(ns-transparent-titlebar . t) default-frame-alist))

;; Go through the list of installed packages and run their auto-loads so that all the packages previously installed will work
