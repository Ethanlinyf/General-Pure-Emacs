;;; mpf-userinterface.el --- Elisp Template . -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;;

;;; Code:

;;--------------------------------------------------------------------
;; theme doom-one
;; see the PR: https://github.com/doomemacs/themes/pull/779
(use-package doom-themes
  :custom
  (doom-themes-enable-bold t)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic t) ; if nil, italics is universally disabled
  :config
  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  :init
  (load-theme 'doom-one t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode))

(use-package hide-mode-line
  :ensure t)

;;-------------------------------------------------------------------------------------------------
(provide 'mpf-userinterface)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; mpf-userinterface.el ends here
