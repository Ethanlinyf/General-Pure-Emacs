;;; init-r-roam.el -- Setting for roam -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for roam research
;;--------------------------------------------------------------------
;;; Code:
(use-package org-roam
  :ensure t
  :init
  (defvar GPE-roam (expand-file-name "GPE-Org/roam/" user-emacs-directory))
  (unless (file-directory-p GPE-roam)
    (make-directory GPE-roam))
  :custom
  (org-roam-directory (file-truename GPE-roam))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode)
  ;; If using org-roam-protocol
  (require 'org-roam-protocol)
  (when emacs/>=27p
    (use-package org-roam-ui
      :ensure t
      :init
      (when (featurep 'xwidget-internal)
        (setq org-roam-ui-browser-function #'xwidget-webkit-browse-url)))))

;;-------------------------------------------------------------------------------------------------
(provide 'init-r-roam)
;;; init-r-roam.el ends here.
