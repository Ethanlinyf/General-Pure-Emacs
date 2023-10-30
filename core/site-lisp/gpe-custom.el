;;; gpe-custom.el --- Elisp Template . -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;;

;;; Code:

(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))

;; (add-to-list 'default-frame-alist '(drag-internal-border . 1))
;; (add-to-list 'default-frame-alist '(internal-border-width . 5))

;; To open a specific file
(defun open-org-brainstom-file()
  "Open a org file for note taken."
  (interactive)
  (find-file "~/Documents/Org/bainstorm.org"))
(global-set-key (kbd "s-4") 'open-org-brainstom-file)

(set-face-attribute 'default nil :height 180);;
;;-------------------------------------------------------------------------------------------------
(provide 'gpe-custom)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; gpe-custom.el ends here.
