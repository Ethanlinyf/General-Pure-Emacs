;;; init-d-dired.el --- Settings for dired-mode -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for dired-mode
;;--------------------------------------------------------------------
;;; Code:




;; Loading later
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(require 'dired-x)
;; (setq dired-recursive-deletes 'always)
(setq dired-recursive-deletes 'top)
(setq dired-recursive-copies 'always)
(put 'dired-find-alternate-file 'disabled nil)

;; Colourful dired
(use-package diredfl
  :ensure t
  :init (diredfl-global-mode 1))

 ;; Show directory first
(setq dired-listing-switches "-alh --group-directories-first")

;; Show git info in dired
(use-package dired-git-info
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))



(use-package all-the-icons
  :ensure t
  :config
  (setq all-the-icons-color-icons t))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

;; move this part to basic
;; (with-eval-after-load "dired"
;;   (put 'dired-find-alternate-file 'disabled nil)
;;   (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
;;   (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-alternate-file)
;;   )

;;(require 'diredful)
;;(diredful-mode 1)
;; (setq all-the-icons-color-icons t)
;;(setq all-the-icons-ibuffer-color-icon t)

(use-package all-the-icons-ibuffer
  :ensure t
  :hook (ibuffer-mode . all-the-icons-ibuffer-mode)
  :config
  (setq all-the-icons-ibuffer-color-icon t))

;; add when is-a-mac
(when *is-mac*
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first"))

;;----------------------------------------------------------------------------
(provide 'init-d-dired)
;;; init-d-dired.el ends here
