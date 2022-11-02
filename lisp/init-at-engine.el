;;; init-a-engine.el --- Pligins for Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Add packages as plugins to facilitate Emascs as General Pure Emacs
;;--------------------------------------------------------------------
;;; Code:

;; (require 'cl)
(require 'package)

;; Install straight.el
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


(when (>= emacs-major-version 28)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
  )

;; Initialise packages
(unless (bound-and-true-p package--initialized)
  (package-initialize))

;; (setq package-check-signature t
;;        load-prefer-newer t)

;;--------------------------------------------------------------------
;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)

;; (require 'use-package)
;;--------------------------------------------------------------------
;;  (straight-use-package 'use-package)
;; (use-package straight
;;   :custom (straight-use-package-by-default t))
  ;:bind  (("C-<f2>" . hydra-straight-helper/body)))

;;--------------------------------------------------------------------
;; A few more useful configurations...

;;--------------------------------------------------------------------
(provide 'init-at-engine)
;;; init-a-engine.el ends here. +
