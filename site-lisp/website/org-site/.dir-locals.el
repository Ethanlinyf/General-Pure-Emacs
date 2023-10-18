;;; .dir-locals.el --- local variables for Org-site Project. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Ethan YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;;  Define a set of local variables for Org-website Project
;;--------------------------------------------------------------------
;;; Code:

((org-mode . (
              (user-full-name . "Dr YF Lin")
              (org-export-in-background . t)
              (org-html-htmlize-output-type . inline-css)
              (org-html-head-include-default-style . nil)
              (org-html-head-include-scripts . nil)
              (org-publish-project-alist . (("orgfiles"
                                             ;; source and public for this project.
                                             :base-directory "~/Org-site/source/org/" ;; the source org folder to export html files
                                             :publishing-directory "~/Org-site/public/" ;; publishing-directory for this project 
                                             ;; :preparation-function
                                             ;; :complete-function
                                             :base-extension "org"
                                             ;; :exclude "PrivatePage.org"     ;; regexp supported
                                             ;; :include
                                             :recursive t
                                             ;; :auto-sitemap t
                                             ;; :sitemap-filename "sitemap.org"
                                             ;; :sitemap-title "Sitemap"
                                             ;; :sitemap-sort-folders last           

                                             ;;Publishing behaviours
                                             :publishing-function org-html-publish-to-html

                                             ;; Generic properties for this project
                                             :headline-levels 4    ;; org-export-headline-levels
                                             :language "en"        ;; org-export-default-language
                                             :section-numbers nil  ;; org-export-with-section-numbers
                                             :with-planning t      ;; org-export-with-planning
                                             :with-priority t      ;; org-export-with-priority ;
                                             ;;  :with-tags not-in-toc ;; org-export-with-tags
                                             :with-toc t           ;; org-export-with-toc

                                             :html-doctype "html5" ;; org-html-doctype
                                             ;;  :html-metadata-timestamp-format "%Y-%m-%d" ;; org-html-metadata-timestamp-format
                                             :html-head-include-default-style nil ;; org-html-head-include-default-style
                                             :html-head-include-scripts nil ;; org-html-head-include-scripts
                                             ;; :html-head
                                             ;; "<link rel=\"shortcut icon\" href=\"themes/assets/icon.png\" type=\"image/x-icon\" />
                                             ;; <link rel=\"stylesheet\" href=\"themes/style.css\" type=\"text/css\"  />
                                             ;; <script type=\"module\" src=\"themes/main.js\" defer></script>" ;; org-html-head
                                             :html-checkbox-type unicode  ;; org-html-checkbox-type
                                             :html-indent t               ;; org-html-indent
                                             ;; :html-link-home "index.html"	;; org-html-link-home
                                             ;; :html-link-up "uUP"          ;; org-html-link-up
                                             :html-validation-link "<a href=\"http://www.thingsengine.org/\">ThingsEngine</a>"    ;; org-html-validation-link
                                             )

                                            ;; static assets
                                            ("org-site"
                                             :base-directory "~/Org-site/source/"
                                             :base-extension "js"
                                             :publishing-directory "~/Org-site/public/"
                                             :recursive nil
                                             :publishing-function org-publish-attachment
                                             )
                                            ("images"
                                             :base-directory "~/Org-site/source/img"
                                             :base-extension any
                                             :publishing-directory "~/Org-site/public/img"
                                             :recursive t
                                             :publishing-function org-publish-attachment
                                             )
                                            ("themes"
                                             :base-directory "~/Org-site/source/theme/"
                                             :base-extension any
                                             :publishing-directory "~/Org-site/public/theme/"
                                             :recursive t
                                             :publishing-function org-publish-attachment
                                             )

                                            ;; reverse static assets
                                            ("rorg-site"
                                             :base-directory "~/Org-site/public/"
                                             :base-extension "js"
                                             :publishing-directory "~/Org-site/source/"
                                             :recursive nil
                                             :publishing-function org-publish-attachment
                                             )
                                            ("rimages"
                                             :base-directory "~/Org-site/public/img/"
                                             :base-extension any
                                             :publishing-directory "~/Org-site/source/img/"
                                             :recursive t
                                             :publishing-function org-publish-attachment
                                             )
                                            ("rthemes"
                                             :base-directory "~/Org-site/public/theme/"
                                             :base-extension any
                                             :publishing-directory "~/Org-site/source/theme/"
                                             :recursive t
                                             :publishing-function org-publish-attachment
                                             )

                                            ("website" :components ("orgfiles" "org-site" "images" "themes"))
                                            ("statics" :components ("confs" "images" "themes"))
                                            ("rstatics" :components ("rorg-site" "rimages" "rthemes"))
                                            ))
              (eval .
                    (progn
                      (defun save-and-publish-website()
                        "Save all buffers and publish."
                        (interactive)
                        (when (yes-or-no-p "Really save and publish current project?")
                          (save-some-buffers t)
                          (org-publish-project "website" t)
                          (message "Site published done.")))


                      (defun save-and-publish-statics ()
                        "Just copy statics like js, css, and image file .etc."
                        (interactive)
                        (org-publish-project "statics" t)
                        (message "Copy statics done."))

                      (defun save-and-publish-rstatics ()
                        "Just copy statics like js, css, and image file .etc.
                         Which is a reverse operation of `save-and-publish-statics'."
                        (interactive)
                        (org-publish-project "rstatics" t)
                        (message "Copy rstatics done."))

                      (defun save-and-publish-file ()
                        "Save current buffer and publish."
                        (interactive)
                        (save-buffer t)
                        (org-publish-current-file t))

                      (defun delete-org-and-html ()
                        "Delete current org and the relative html when it exists."
                        (interactive)
                        (when (yes-or-no-p "Really delete current org and the relative html?")

                          (let ((fileurl (concat "~/site/public/" (file-name-base (buffer-name)) ".html")))
                            (if (file-exists-p fileurl)
                                (delete-file fileurl))
                            (delete-file (buffer-file-name))
                            (kill-this-buffer)
                            (message "Delete org and the relative html done."))))

                      (defun just-delete-relative-html ()
                        "Just delete the relative html when it exists."
                        (interactive)
                        (when (yes-or-no-p "Really delete the relative html?")

                          (let ((fileurl (concat "~/site/public/" (file-name-base (buffer-name)) ".html")))
                            (if (file-exists-p fileurl)
                                (progn
                                  (delete-file fileurl)
                                  (message "Delete the relative html done.")
                                  )
                              (message "None relative html.")))))

                      (define-minor-mode auto-save-and-publish-file-mode
                        "Toggle auto save and publish current file."
                        :global nil
                        :lighter ""
                        (if auto-save-and-publish-file-mode
                            ;; When the mode is enabled
                            (progn
                              (add-hook 'after-save-hook #'save-and-publish-file :append :local))
                          ;; When the mode is disabled
                          (remove-hook 'after-save-hook #'save-and-publish-file :local)))
                      ;; (use-package auto-save-and-publish-file-mode
                      ;;   :hook (org-mode))

                      (add-hook 'org-mode-hook #'auto-save-and-publish-file-mode)

                      (use-package simple-httpd
                        :ensure t
                        :config
                        (setq httpd-root "~/Org-site/public"))

                      (defun preview-current-buffer-in-browser ()
                        "Open current buffer as html."
                        (interactive)
                        (let ((fileurl (concat "http://127.0.0.1:8080/" (file-name-base (buffer-name)) ".html")))
                          (save-and-publish-file)
                          (unless (httpd-running-p) (httpd-start))
                          (browse-url fileurl))))))))
