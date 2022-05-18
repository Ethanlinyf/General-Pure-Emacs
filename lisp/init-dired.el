;;; dired.el --- Settings for dired-mode -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;; Commentary:
;; Configurations for dired-mode
;;--------------------------------------------------------------------
;;; Code:

;; Colourful dired
(use-package diredfl
  :init (diredfl-global-mode 1))

; (diredful-mode 1)

 ;; Show directory first
(setq dired-listing-switches "-alh --group-directories-first")

  ;; Show git info in dired
  (use-package dired-git-info
    :bind (:map dired-mode-map
           (")" . dired-git-info-mode)))

;; add when is-a-mac
(when *is-mac*
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first"))


;;----------------------------------------------------------------------------
(provide 'init-dired)
;;; ends here
