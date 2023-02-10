;;; init-d-dired.el --- Settings for dired-mode -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for dired-mode
;;--------------------------------------------------------------------
;;; Code:

(use-package dired
  :ensure nil
  :bind (:map dired-mode-map
              ("RET" . dired-find-alternate-file)
              ;; allow to edit the names of files within a dired buffer under wdired-mode
              ("C-c C-p" . wdired-change-to-wdired-mode)
              )
  :config
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always
        dirvish-override-dired-mode t)
  (put 'dired-find-alternate-file 'disabled nil)

  (when sys/macp
    (setq dired-use-ls-dired nil)
    
    (when (executable-find "gls")
      (setq insert-directory-program "gls"
            dired-use-ls-dired t)))
  
  (when (or (and sys/macp (executable-find "gls"))
            (and (or sys/linuxp sys/cygwinp) (executable-find "ls")))
    (setq ls-lisp-use-insert-directory-program t)
    (setq dired-listing-switches "-alh --group-directories-first")))

;; From Centaur Emacs
(use-package dired-plus
  :ensure nil
  :config
  (let ((cmd (cond (sys/mac-x-p "open")
                   (sys/linux-x-p "xdg-open")
                   (sys/win32p "start")
                   (t ""))))
    (setq dired-guess-shell-alist-user
          `(("\\.pdf\\'" ,cmd)
            ("\\.docx\\'" ,cmd)
            ("\\.\\(?:djvu\\|eps\\)\\'" ,cmd)
            ("\\.\\(?:jpg\\|jpeg\\|png\\|gif\\|xpm\\)\\'" ,cmd)
            ("\\.\\(?:xcf\\)\\'" ,cmd)
            ("\\.csv\\'" ,cmd)
            ("\\.tex\\'" ,cmd)
            ("\\.\\(?:mp4\\|mkv\\|avi\\|flv\\|rm\\|rmvb\\|ogv\\)\\(?:\\.part\\)?\\'" ,cmd)
            ("\\.\\(?:mp3\\|flac\\)\\'" ,cmd)
            ("\\.html?\\'" ,cmd)
            ("\\.md\\'" ,cmd))))

  (setq dired-omit-files
        (concat dired-omit-files "\\|^.DS_Store$\\|^.projectile$\\|^.git*\\|^.svn$\\|^.vscode$\\|\\.js\\.meta$\\|\\.meta$\\|\\.elc$\\|^.emacs.*")))


;; Colourful dired-mode
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;; Show git informatio in dired mode
(use-package dired-git-info
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode)))

;; make icons available in dired mode
(use-package all-the-icons-dired
  :after all-the-icons
  :ensure t
  :hook (dired-mode . (lambda ()
                          (when (icon-displayable-p)
                            (all-the-icons-dired-mode))))
  :config (setq all-the-icons-dired-monochrome nil)) ;; nil means it is colourful in dired-mode

(when (executable-find "fd")
  (use-package fd-dired
    :ensure t))

;;----------------------------------------------------------------------------
(provide 'init-d-dired)
;;; init-d-dired.el ends here
