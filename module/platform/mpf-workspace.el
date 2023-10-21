;;; mpf-workspace.el --- Project Utilities -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; Needed elements for a project as IDE

;;; code:

(require 'treemacs)
(global-set-key (kbd "s-2") #'treemacs)

(require 'centaur-tabs)
(centaur-tabs-mode t)
(global-set-key (kbd "C-<prior>")  'centaur-tabs-backward)
(global-set-key (kbd "C-<next>") 'centaur-tabs-forward)


;; (use-package aweshell
;;   :load-path "site-lisp/gpe-aweshell"
;;   ;; :ensure company
;;   :ensure nil
;;   ;; :functions (aweshell-auto-suggestion)
;;   :bind
;;   ("s-1" . aweshell-dedicated-toggle)
;;   (:map eshell-mode-map ("C-x C-g" . aweshell-dedicated-close))
;;   ; :config (setq aweshell-auto-suggestion-p t)
;;   ; :hook (eshell-mode . company-mode)
;;   )

(add-to-list 'load-path "/Users/ethanlin/.emacs.d/extension/gpe-aweshell/")
(require 'aweshell)
(global-set-key (kbd "s-1") 'aweshell-dedicated-toggle)




(message "project")

;;-------------------------------------------------------------------------------------------------
(provide 'mpf-workspace)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; mpf-workspace.el ends here.
