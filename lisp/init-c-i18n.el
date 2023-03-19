;; init-c-i18n.el -- Settings for Internationalisation . -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; The configurations for internationalisation, including imput methods, fonts, coding system, and so on.
;;--------------------------------------------------------------------
;;; Code:

(use-package pyim
  :ensure t)

(use-package define-word
  :commands define-word-at-point
  :ensure t
  :bind
  ("s-6" . define-word-at-point))

(use-package powerthesaurus
  :commands powerthesaurus-lookup-dwim
  :ensure t
  :bind
  ("s-7" . powerthesaurus-lookup-dwim))

;;--------------------------------------------------------------------
(provide 'init-c-i18n)
;;; init-c-i18n.el ends here
