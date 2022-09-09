;; init-ui.el -- UI configurations. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for User Interface
;;--------------------------------------------------------------------
;;; Code:

  ;; Premature redisplays can substantially affect startup times and produce
  ;; ugly flashes of unstyled Emacs.





;; no title bar (not covienient)
;; (when *is-mac*
;;   (setq default-frame-alist '((undecorated . t)))
;;   (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;;   (add-to-list 'default-frame-alist '(internal-border-width . 5)))

;; (package-install 'popwin)

;;--------------------------------------------------------------------
(provide 'init-e-ui)
;;; init-e-ui.el ends here
