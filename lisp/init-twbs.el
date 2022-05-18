;;; init-twbs.el --- Org2xhtml with boostrap. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Export org-mode docs as HTML compatible with Twitter Bootstrap.
;;--------------------------------------------------------------------
;;; Code:

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/Org-twbs/org/"
         :publishing-directory "~/Org-twbs/public_html/"
         :publishing-function org-twbs-publish-to-html
         :with-sub-superscript nil
         )))

(defun my-org-publish-buffer ()
  (interactive)
  (save-buffer)
  (save-excursion (org-publish-current-file))
  (let* ((proj (org-publish-get-project-from-filename buffer-file-name))
         (proj-plist (cdr proj))
         (rel (file-relative-name buffer-file-name
                                  (plist-get proj-plist :base-directory)))
         (dest (plist-get proj-plist :publishing-directory)))
    (browse-url (concat "file://"
                        (file-name-as-directory (expand-file-name dest))
                        (file-name-sans-extension rel)
                        ".html"))))

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "s-\\") 'my-org-publish-buffer)))


;;--------------------------------------------------------------------
(provide 'init-twbs)
;;; ends here
