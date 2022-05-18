;;; custom.el --- Default Variables -*- lexical-binding: t; -*-
;;
;;--------------------------------------------------------------------
;; Commentary:
;; Variables are added by setting Emacs and .dir-locals.el 
;;--------------------------------------------------------------------
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   '((output-dvi "open")
     (output-pdf "PDF Tools")
     (output-html "open")))
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "1d44ec8ec6ec6e6be32f2f73edf398620bb721afeed50f75df6b12ccff0fbb15" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "0d01e1e300fcafa34ba35d5cf0a21b3b23bc4053d388e352ae6a901994597ab1" "a6e620c9decbea9cac46ea47541b31b3e20804a4646ca6da4cce105ee03e8d0e" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "1bddd01e6851f5c4336f7d16c56934513d41cc3d0233863760d1798e74809b4b" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "d268b67e0935b9ebc427cad88ded41e875abfcc27abd409726a92e55459e0d01" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" default))
 '(display-time-mode t)
 '(highlight-indent-guides-method 'character)
 '(initial-frame-alist '((fullscreen . maximized)))
 '(ns-use-fullscreen-animation t)
 '(ns-use-native-fullscreen t)
 '(org-agenda-files '("~/Org-site/source/org/org2xhtml.org"))
 '(package-selected-packages
   '(lsp-pyright treemacs-icons-dired treemacs-tab-bar treemacs-persp treemacs-magit treemacs-projectile treemacs-evil good-scroll simple-httpd htmlize dired-git-info diredful gnu-elpa-keyring-update diminish use-package company doom-modeline doom-themes all-the-icons magit window-numbering lsp-mode flycheck which-key yasnippet projectile treemacs highlight-indent-guides multi-term ivy-rich all-the-icons-ivy-rich amx auctex reftex cdlatex auto-complete all-the-icons-dired diredfl))
 '(safe-local-variable-values
   '((eval progn
           (defun save-and-publish-website nil "Save all buffers and publish."
                  (interactive)
                  (when
                      (yes-or-no-p "Really save and publish current project?")
                    (save-some-buffers t)
                    (org-publish-project "website" t)
                    (message "Site published done.")))
           (defun save-and-publish-statics nil "Just copy statics like js, css, and image file .etc."
                  (interactive)
                  (org-publish-project "statics" t)
                  (message "Copy statics done."))
           (defun save-and-publish-rstatics nil "Just copy statics like js, css, and image file .etc.
                         Which is a reverse operation of `save-and-publish-statics'."
                  (interactive)
                  (org-publish-project "rstatics" t)
                  (message "Copy rstatics done."))
           (defun save-and-publish-file nil "Save current buffer and publish."
                  (interactive)
                  (save-buffer t)
                  (org-publish-current-file t))
           (defun delete-org-and-html nil "Delete current org and the relative html when it exists."
                  (interactive)
                  (when
                      (yes-or-no-p "Really delete current org and the relative html?")
                    (let
                        ((fileurl
                          (concat "~/site/public/"
                                  (file-name-base
                                   (buffer-name))
                                  ".html")))
                      (if
                          (file-exists-p fileurl)
                          (delete-file fileurl))
                      (delete-file
                       (buffer-file-name))
                      (kill-this-buffer)
                      (message "Delete org and the relative html done."))))
           (defun just-delete-relative-html nil "Just delete the relative html when it exists."
                  (interactive)
                  (when
                      (yes-or-no-p "Really delete the relative html?")
                    (let
                        ((fileurl
                          (concat "~/site/public/"
                                  (file-name-base
                                   (buffer-name))
                                  ".html")))
                      (if
                          (file-exists-p fileurl)
                          (progn
                            (delete-file fileurl)
                            (message "Delete the relative html done."))
                        (message "None relative html.")))))
           (define-minor-mode auto-save-and-publish-file-mode "Toggle auto save and publish current file." :global nil :lighter ""
             (if auto-save-and-publish-file-mode
                 (progn
                   (add-hook 'after-save-hook #'save-and-publish-file :append :local))
               (remove-hook 'after-save-hook #'save-and-publish-file :local)))
           (add-hook 'org-mode-hook #'auto-save-and-publish-file-mode)
           (use-package simple-httpd :ensure t :config
             (setq httpd-root "~/Org-site/public"))
           (defun preview-current-buffer-in-browser nil "Open current buffer as html."
                  (interactive)
                  (let
                      ((fileurl
                        (concat "http://127.0.0.1:8080/"
                                (file-name-base
                                 (buffer-name))
                                ".html")))
                    (save-and-publish-file)
                    (unless
                        (httpd-running-p)
                      (httpd-start))
                    (browse-url fileurl))))
     (org-publish-project-alist
      ("orgfiles" :base-directory "~/Org-site/source/org/" :publishing-directory "~/Org-site/public/" :base-extension "org" :recursive t :publishing-function org-html-publish-to-html :headline-levels 4 :language "en" :section-numbers nil :with-planning t :with-priority t :with-toc t :html-doctype "html5" :html-head-include-default-style nil :html-head-include-scripts nil :html-checkbox-type unicode :html-indent t :html-validation-link "<a href=\"http://www.thingsengine.org/\">ThingsEngine</a>")
      ("org-site" :base-directory "~/Org-site/source/" :base-extension "js" :publishing-directory "~/Org-site/public/" :recursive nil :publishing-function org-publish-attachment)
      ("images" :base-directory "~/Org-site/source/img" :base-extension any :publishing-directory "~/Org-site/public/img" :recursive t :publishing-function org-publish-attachment)
      ("themes" :base-directory "~/Org-site/source/theme/" :base-extension any :publishing-directory "~/Org-site/public/theme/" :recursive t :publishing-function org-publish-attachment)
      ("rorg-site" :base-directory "~/Org-site/public/" :base-extension "js" :publishing-directory "~/Org-site/source/" :recursive nil :publishing-function org-publish-attachment)
      ("rimages" :base-directory "~/Org-site/public/img/" :base-extension any :publishing-directory "~/Org-site/source/img/" :recursive t :publishing-function org-publish-attachment)
      ("rthemes" :base-directory "~/Org-site/public/theme/" :base-extension any :publishing-directory "~/Org-site/source/theme/" :recursive t :publishing-function org-publish-attachment)
      ("website" :components
       ("orgfiles" "org-site" "images" "themes"))
      ("statics" :components
       ("confs" "images" "themes"))
      ("rstatics" :components
       ("rorg-site" "rimages" "rthemes")))
     (org-html-head-include-scripts)
     (org-html-htmlize-output-type . inline-css)
     (org-export-in-background . t)
     (user-full-name . "Dr YF Lin")))
 '(split-width-threshold 80)
 '(tool-bar-mode nil)
 '(treemacs-indentation 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold))))))
