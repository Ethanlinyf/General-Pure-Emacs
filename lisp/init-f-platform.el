;;; init-f-platform.el --- IDE -*- lexical-binding: t; -*-
;;
; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Integrated Development Enviroment
;;--------------------------------------------------------------------
;;; Code:

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/awesome-tab"))
(require 'awesome-tab)

;;--------------------------------------------------------------------

;; (awesome-tab-mode t)

(add-hook 'prog-mode-hook #'awesome-tab-mode)

(add-hook 'eshell-mode-hook #'(lambda() (awesome-tab-mode -1)))
(add-hook 'Info-mode-hook (lambda() (awesome-tab-mode -1)))

;; (defun awesome-tab-buffer-groups ()
;; "`awesome-tab-buffer-groups' control buffers' group rules.
;; Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
;; All buffer name start with * will group to \"Emacs\".
;; Other buffer group by `awesome-tab-get-group-name' with project name."
;; (list
;; (cond
;;     ((or (string-equal "*" (substring (buffer-name) 0 1))
;; 	(memq major-mode '(magit-process-mode
;; 			    magit-status-mode
;; 			    magit-diff-mode
;; 			    magit-log-mode
;; 			    magit-file-mode
;; 			    magit-blob-mode
;; 			    magit-blame-mode)))
;;     "Emacs")
;;     ((derived-mode-p 'eshell-mode)
;;     "EShell")
;;     ((derived-mode-p 'dired-mode)
;;     "Dired")
;;     ((memq major-mode '(org-mode org-agenda-mode diary-mode))
;;     "OrgMode")
;;     ((derived-mode-p 'eaf-mode)
;;     "EAF")
;;     (t
;;     (awesome-tab-get-group-name (current-buffer))))))

;;-------------------------------------------------------------------- 

;; https://github.com/re5et/simp
;; (use-package simple
;;   :ensure nil
;;   :hook (after-init . size-indication-mode)
;;   :init
;;   (progn
;;     (setq column-number-mode t)
;;     ))

;;--------------------------------------------------------------------
(require 'winum)

(winum-mode)
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
           treemacs-deferred-git-apply-delay        0.5
           treemacs-directory-name-transformer      #'identity
           treemacs-display-in-side-window          t
           treemacs-eldoc-display                   'simple
           treemacs-file-event-delay                5000
           treemacs-file-extension-regex            treemacs-last-period-regex-value
           treemacs-file-follow-delay               0.2
           treemacs-file-name-transformer           #'identity
           treemacs-follow-after-init               t
           treemacs-expand-after-init               t
           treemacs-find-workspace-method           'find-for-file-or-pick-first
           treemacs-git-command-pipe                ""
           treemacs-goto-tag-strategy               'refetch-index
          ;; treemacs-header-scroll-indicators        '(nil . "^^^^^^")'
           treemacs-indentation                     2
           treemacs-indentation-string              " "
           treemacs-indent-guide-style              'line
           treemacs-is-never-other-window           nil
           treemacs-max-git-entries                 5000
           treemacs-missing-project-action          'ask
           treemacs-move-forward-on-expand          nil
           treemacs-no-png-images                   nil
           treemacs-no-delete-other-windows         t
           treemacs-project-follow-cleanup          nil
           treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
           treemacs-position                        'left
           treemacs-read-string-input               'from-child-frame
           treemacs-recenter-distance               0.1
           treemacs-recenter-after-file-follow      nil; treemacs-recenter-after-tag-follow       nil
           treemacs-recenter-after-project-jump     'always
           treemacs-recenter-after-project-expand   'on-distance
           treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
           treemacs-show-cursor                     nil
           treemacs-show-hidden-files               t
           treemacs-silent-filewatch                nil
           treemacs-silent-refresh                  nil
           treemacs-sorting                         'alphabetic-asc
           treemacs-select-when-already-in-treemacs 'move-back
           treemacs-space-between-root-nodes        t
           treemacs-tag-follow-cleanup              t
           treemacs-tag-follow-delay                1.5
           treemacs-text-scale                      nil
           treemacs-user-mode-line-format           nil
           treemacs-user-header-line-format         nil
           treemacs-wide-toggle-width               70
           treemacs-width                           30
           treemacs-width-increment                 1
           treemacs-width-is-initially-locked       t
           treemacs-workspace-switch-cleanup        nil
          )

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)
    (treemacs-indent-guide-mode t)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

;; (use-package treemacs-evil
;;   :after (treemacs evil)
;;   :ensure t)

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

;; (use-package treemacs-icons-dired
;;   :hook (dired-mode . treemacs-icons-dired-enable-once)
;;   :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

(use-package treemacs-persp ;;treemacs-perspective if you use perspective.el vs. persp-mode
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-tab-bar ;;treemacs-tab-bar if you use tab-bar-mode
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

;;--------------------------------------------------------------------

(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/aweshell"))
(require 'aweshell)
(global-set-key (kbd "s-1") 'aweshell-dedicated-toggle)

;;--------------------------------------------------------------------
(require 'epc)

(add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")

(require 'lsp-bridge)
(global-lsp-bridge-mode)
;;--------------------------------------------------------------------
(provide 'init-f-platform)
;;; init-f-platform.el ends here