(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("353ffc8e6b53a91ac87b7e86bebc6796877a0b76ddfc15793e4d7880976132ae" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "333958c446e920f5c350c4b4016908c130c3b46d590af91e1e7e2a0611f1e8c5" "74b9e99a8682c86659b8ace1610c4556c4619e6ca812a37b32d2c5f844fdafca" "f7fed1aadf1967523c120c4c82ea48442a51ac65074ba544a5aefc5af490893b" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "8146edab0de2007a99a2361041015331af706e7907de9d6a330a3493a541e5a6" "4f1d2476c290eaa5d9ab9d13b60f2c0f1c8fa7703596fa91b235db7f99a9441b" "a7b20039f50e839626f8d6aa96df62afebb56a5bbd1192f557cb2efb5fcfb662" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "e8df30cd7fb42e56a4efc585540a2e63b0c6eeb9f4dc053373e05d774332fc13" "6f4421bf31387397f6710b6f6381c448d1a71944d9e9da4e0057b3fe5d6f2fad" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "028c226411a386abc7f7a0fba1a2ebfae5fe69e2a816f54898df41a6a3412bb5" "613aedadd3b9e2554f39afe760708fc3285bf594f6447822dd29f947f0775d6c" "f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "a9a67b318b7417adbedaab02f05fa679973e9718d9d26075c6235b1f0db703c8" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "66bdbe1c7016edfa0db7efd03bb09f9ded573ed392722fb099f6ac6c6aefce32" "c0d992b42529cc61d03cbb8668df5c928d179ab5babbd21c9673b9aa47707f90" "6d741c51b4fd0dd4211fe4134c55b95018e94765e0dfd27771a2f54642ba11f8" "89feed18f1d627659e68e457852ffff5bd63c5103f5d23fbc949db121d4ce742" "8e8152ac5b1c2a4f55928ca03a6e6d93647b9a9900f7613e433092b202191963" "2f241ca8cad85d33a0443998c8b105d8d5508e93fbde415fa8402f1987799fb0" "5a6854c6ad74c99ced6e42ed19f0856d2feba54fdaafe05e15fec509a1d1bd7a" "025fd2aaf2c446acda600c3f3b17dbaf3a77cf93578325d6ec8a778f8bc91341" "b9e222c23b493f3f0a452e06135fb108f062c31e4adc00842ce2f9e3c3c9368e" "e19ac4ef0f028f503b1ccafa7c337021834ce0d1a2bca03fcebc1ef635776bea" "22ce392ec78cd5e512169f8960edf5cbbad70e01d3ed0284ea62ab813d4ff250" "1bddd01e6851f5c4336f7d16c56934513d41cc3d0233863760d1798e74809b4b" "3319c893ff355a88b86ef630a74fad7f1211f006d54ce451aab91d35d018158f" default))
 '(display-time-mode t)
 '(org-agenda-files
   '("~/.config/nvim/plan.org" "/Users/ethanlin/Documents/Test/test.org"))
 '(package-selected-packages
   '(valign company exec-path-from-shell doom-modeline doom-themes dashboard page-break-lines projectile counsel-projectile all-the-icons all-the-icons-completion undo-tree treemacs multi-term all-the-icons-dired diredfl ns-auto-titlebar all-the-icons-ibuffer treemacs-all-the-icons corfu orderless epc corfu-doc which-key window-numbering magit rainbow-delimiters hungry-delete ctrlf popwin minions cape yasnippet posframe markdown-mode org-superstar org-noter org-ql org-download Valign org-roam org-roam-ui org-roam-bibtex websocket dash f s emacsql emacsql-sqlite magit-section auctex reftex cdlatex company-auctex pdf-tools latex-preview-pane zotxt htmlize keycast))
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
           (defun save-and-publish-rstatics nil "Just copy statics like js, css, and image file .etc.\12                         Which is a reverse operation of `save-and-publish-statics'."
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
 '(size-indication-mode t)
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Menlo" :foundry "nil" :slant normal :weight regular :height 160 :width normal))))
 '(dashboard-heading ((t (:inherit (font-lock-string-face bold))))))
