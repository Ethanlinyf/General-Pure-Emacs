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

;; add tabs for the open files
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/awesome-tab"))
(require 'awesome-tab)
(awesome-tab-mode t)

;;--------------------------------------------------------------------
;; magit for git/GitHub
(use-package magit
  :init (setq magit-diff-refine-hunk t))

;;--------------------------------------------------------------------
;; awesome-tab
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
     (string-prefix-p "Aweshell" name)
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
  (setq awesome-tab-display-icon t))

;;--------------------------------------------------------------------
;; (require 'winum)
;; (use-package winum
;;   :ensure t
;;   :hook
;;   (after-init . winum-mode))

;;--------------------------------------------------------------------
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
        ("C-M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("s-2"       . treemacs)
                                        ;("s-3"       . awesome-tab-mode)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

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

(use-package treemacs-all-the-icons
  :after (treemacs all-the-icons)
  :ensure t
  :init
  (require 'treemacs-all-the-icons)
  (treemacs-load-theme "all-the-icons"))

;;--------------------------------------------------------------------
;; (use-package company
;;   :ensure t)
;; (add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/aweshell"))
;; (require 'aweshell)
;; (global-set-key (kbd "s-1") 'aweshell-dedicated-toggle)

(use-package aweshell
  :load-path "~/.emacs.d/site-lisp/aweshell"
  :ensure company
  :defer t
  :bind ("s-1" . aweshell-dedicated-toggle))


;;--------------------------------------------------------------------
;; (require 'epc)
(use-package markdown-mode
  :ensure t)

;; (use-package epc
;;   :ensure t)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/lsp-bridge")

;; (require 'lsp-bridge)
;; (global-lsp-bridge-mode)

(use-package lsp-bridge
  :load-path "~/.emacs.d/site-lisp/lsp-bridge"
  ;; :ensure-system-package
  ;; ((epc . "pip install epc")
  ;;  (orjson . "pip install orjson")
  ;;  (six . "pip install six"))
  :ensure epc
  :defer 2
  :hook (after-init . global-lsp-bridge-mode))



;;--------------------------------------------------------------------
;; blink-search
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/blink-search")
;; (require 'blink-search)

(use-package blink-search
  :load-path "~/.emacs.d/site-lisp/blink-search"
  :defer 2
  :autoload blink-search)

;;--------------------------------------------------------------------

(use-package org-transclusion
  :ensure t)

(use-package ivy
  :ensure t)

(use-package dash
  :ensure t)


;; (add-to-list 'load-path "~/.emacs.d/site-lisp/popweb") ; add popweb to your load-path

;; ;; Org-Roam ID link and footnote link previewer
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/extension/org-roam")
;; (require 'popweb-org-roam-link)

;; ;; LaTeX preview functionality
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/extension/latex")
;; (require 'popweb-latex)
;; (add-hook 'latex-mode-hook #'popweb-latex-mode)

;; ;; Chinese-English translation popup
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/extension/dict") ;
;; (require 'popweb-dict-bing) ; Translation using Bing
;; (require 'popweb-dict-youdao) ; Translation using Youdao

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/extension/url-preview")
;; (require 'popweb-url)


(use-package popweb
  :defer t
  :load-path "~/.emacs.d/site-lisp/popweb"
  :hook ((org-mode tex-mode) . popweb-mode)
  :init
  ;; Org-Roam ID link and footnote link previewer
  (add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/extension/org-roam")
  (require 'popweb-org-roam-link)

  ;; LaTeX preview functionality
  (add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/extension/latex")
  (require 'popweb-latex)
  (add-hook 'latex-mode-hook #'popweb-latex-mode)

  ;; Chinese-English translation popup
  (add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/extension/dict") ;
  (require 'popweb-dict-bing) ; Translation using Bing
  (require 'popweb-dict-youdao) ; Translation using Youdao

  (add-to-list 'load-path "~/.emacs.d/site-lisp/popweb/extension/url-preview")
  (require 'popweb-url)
  :pin manual)
;;--------------------------------------------------------------------
(provide 'init-f-platform)
;;; init-f-platform.el ends here
