;;; init-platform.el --- IDE -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Integrated Development Environment
;;--------------------------------------------------------------------
;;; Code:

;; magit for git/GitHub
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :init
  (setq magit-diff-refine-hunk t))

;;--------------------------------------------------------------------
;; add tabs for the open files
(use-package awesome-tab
  :ensure nil
  :load-path "~/.emacs.d/site-lisp/awesome-tab"
  ;; :hook
  ;; (after-init . awesome-tab-mode)
  :bind ("s-9" . awesome-tab-mode)
  :config
  (defun awesome-tab-buffer-groups ()
    "`awesome-tab-buffer-groups' control buffers' group rules.
  Group awesome-tab with mode if buffer is derived from `eshell-mode' `emacs-lisp-mode' `dired-mode' `org-mode' `magit-mode'.
  All buffer name start with * will group to \"Emacs\".
  Other buffer group by `awesome-tab-get-group-name' with project name."
    (list
     (cond
      ((or (string-equal "*" (substring (buffer-name) 0 1))
           (memq major-mode '(magit-process-mode
                              magit-status-mode
                              magit-diff-mode
                              magit-log-mode
                              magit-file-mode
                              magit-blob-mode
                              magit-blame-mode
                              )))
       "Emacs")
      ((derived-mode-p 'eshell-mode)
       "EShell")
      ((derived-mode-p 'emacs-lisp-mode)
       "Elisp")
      ((derived-mode-p 'dired-mode)
       "Dired")
      ((memq major-mode '(org-mode org-agenda-mode diary-mode))
       "OrgMode")
      (t
       (awesome-tab-get-group-name (current-buffer))))))

  (defun awesome-tab-hide-tab (x)
    (let ((name (format "%s" x)))
      (or
       (string-prefix-p "*epc" name)
       (string-prefix-p "*helm" name)
       (string-prefix-p "*Compile-Log*" name)
       (string-prefix-p "*lsp" name)
       (string-prefix-p "*Treemacs-" name)
       (string-prefix-p "*shell*" name)
       (string-prefix-p "*shell" name)
       (string-prefix-p "*dashboard*" name)
       (string-prefix-p "*info*" name)
       (string-prefix-p "*scratch*" name)
       (string-prefix-p "*Messages*" name)
       (and (string-prefix-p "magit" name)
            (not (file-name-extension name)))
       )))

  (when (display-graphic-p)
    (setq awesome-tab-display-icon t)))

;;--------------------------------------------------------------------
;; treemacs
(use-package treemacs
  :ensure t
  :defer t
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
          ;; treemacs-header-scroll-indicators        '(nil . "^^^^^^")
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
  :bind (:map global-map
              ("C-M-0"     . treemacs-select-window)
              ("C-x t 1"   . treemacs-delete-other-windows)
              ("C-x t t"   . treemacs)
              ("s-2"       . treemacs)
              ("C-x t d"   . treemacs-select-directory)
              ("C-x t B"   . treemacs-bookmark)
              ("C-x t C-t" . treemacs-find-file)
              ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package treemacs-magit
  :after (treemacs magit)
  :ensure t)

;;treemacs-perspective if you use perspective.el vs. persp-mode
(use-package treemacs-persp
  :after (treemacs persp-mode) ;;or perspective vs. persp-mode
  :ensure t
  :config (treemacs-set-scope-type 'Perspectives))

;;treemacs-tab-bar if you use tab-bar-mode
(use-package treemacs-tab-bar
  :after (treemacs)
  :ensure t
  :config (treemacs-set-scope-type 'Tabs))

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :ensure t
  :init
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons"))

;;--------------------------------------------------------------------
;; aweshell with company and company-shell
(use-package company
  :ensure t)

(use-package company-shell
  :ensure t
  :config
  (add-to-list 'company-backends '(company-shell company-shell-env company-fish-shell)))

(use-package aweshell
  :load-path "site-lisp/aweshell"
  ;; :ensure company
  :ensure nil
  :bind ("s-1" . aweshell-dedicated-toggle)
  :init (setq aweshell-auto-suggestion-p t))

;;--------------------------------------------------------------------
;; lsp-bridge
(use-package markdown-mode
  :ensure t)

(use-package epc
  :ensure t)

(use-package lsp-bridge
  :load-path "site-lisp/lsp-bridge"
  ;; :ensure-system-package
  ;; ((epc . "pip install epc")
  ;;  (orjson . "pip install orjson")
  ;;  (six . "pip install six"))
  :commands lsp-bridge-mode
  :ensure nil
  :config
  (setq lsp-bridge-enable-search-words t)
  ;; (setq lsp-bridge-enable-org-babel t) ; error popping up, need to be checked.
  :hook
  (after-init . global-lsp-bridge-mode))

;;--------------------------------------------------------------------
;; blink-search
(use-package blink-search
  :ensure nil
  :load-path "site-lisp/blink-search"
  :commands blink-search
  :bind ("s-8" . blink-search))

;;--------------------------------------------------------------------
;; popweb
(use-package dash
  :ensure t)

(use-package popweb
  :ensure nil
  :load-path "site-lisp/popweb")

(use-package popweb-dict
  :ensure nil
  :load-path "site-lisp/popweb/extension/dict"
  :config
  (require 'popweb-dict)
  :bind ("s-4" . popweb-dict-youdao-pointer))

(use-package popweb-url
  :ensure nil
  :load-path "site-lisp/popweb/extension/url-preview"
  ;; :custom
  ;; (setq popweb-url-web-window-size-use-absolute t)
  ;; (setq popweb-url-web-window-width-absolute 375)
  ;; (setq popweb-url-web-window-height-absolute 677)
  ;; (setq popweb-url-web-window-width-scale 0.8)
  ;; (setq popweb-url-web-window-height-scale 0.8)
  :config
  (require 'popweb-url)
  :bind ("s-5" . popweb-url-preview-pointer))

;; ;;--------------------------------------------------------------------
;; ;; ChatGPT
(use-package mind-wave
  :ensure nil
  :load-path "site-lisp/mind-wave"
  :commands mind-wave-chat-ask
  :bind ("s-`" . mind-wave-chat-ask))

;;--------------------------------------------------------------------
(provide 'init-j-platform)
;;; init-j-platform.el ends here
