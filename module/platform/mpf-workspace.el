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


(message "project")

;;-------------------------------------------------------------------------------------------------
(provide 'mpf-workspace)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; mpf-workspace.el ends here.
