;;; init-c-package.el --- Pligins for Emacs. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Add feature defined in the Lisp folder
;;--------------------------------------------------------------------
;;; Code:

(require 'cl)
(require 'package)

(when (>= emacs-major-version 24)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
  )

(package-initialize)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; Add whatever packages you want here
(defvar puremacs/packages '(
                            ;; wishlist:
                            ; avy

			    company
                            ;; company-box
			    ;; smartparens
			    exec-path-from-shell
                            doom-modeline
                            doom-themes
                            dashboard
                            page-break-lines
                            projectile
                            counsel-projectile
                            all-the-icons
                            all-the-icons-completion

                            undo-tree

                            ;; ;; lsp-mode
                            ;; flycheck
                            ;; 
                            treemacs
                            ;; highlight-indent-guides
                            multi-term
                            ;; ido
                            ;; ivy-rich
                            ;; all-the-icons-ivy-rich
                            ;; amx
                            all-the-icons-dired
                            diredfl
                            ;;;diredful
                            ;; maxframe
                            ns-auto-titlebar ;; Set the MacOS transparent titlebar to match theme
                            ;; 
                            ;; 
                            ;; 
                            ;; pangu-spacing ; Minor-mode to add space between Chinese and English characters.
                            all-the-icons-ibuffer ; Display icons for all buffers in ibuffer.
                            ;; all-the-icons-ivy-rich ;
                            ;; good-scroll ; This package implements smooth scrolling by pixel lines. It attempts to improve upon `pixel-scroll-mode' by adding variable speed.
                            ;; elisp-format
                            ;; 
                            ;; 
                            ;; org-download
                            ;; format-all
                            ;; htmlize ; from org-site
                            ;; ;;ox-twbs
                            
                            ;; 
                            treemacs-all-the-icons
                            corfu
                            orderless
                            ;; 
                            ;; eglot
                            epc
                            corfu-doc

                            ;;; D
                            which-key
                            window-numbering
                            magit
                            rainbow-delimiters
                            hungry-delete
                            ctrlf
                            popwin
                            minions
                            cape
                            ;; format-all
                            ;; crux

                            
                            ;;; G
                            yasnippet

                            ;;; H
                            posframe
                            markdown-mode

                            ;;; I
                            pyvenv

                            ;;org
                            org-superstar
                            org-noter
                            org-ql
                            org-download
                            pangu-spacing
                            ;;valign installed

                            ;;org-roam
                            org-roam
                            org-roam-ui
                            org-roam-bibtex
                            websocket

                            dash
                            f
                            s
                            emacsql
                            emacsql-sqlite
                            magit-section
                            ;; [filenotify-recursive][https://github.com/jethrokuan/filenotify-recursive]

                            ;; TeX
                            auctex
                            reftex
                            cdlatex
                            company-auctex
                            pdf-tools
                            latex-preview-pane ;;latex-preview-pane is a minor mode for Emacs that enables you to preview your LaTeX files directly in Emacs. It supports PDF previews, your choice of pdflatex or xelatex, and it highlights errors in your LaTeX buffer.
                            zotxt ; for reference

                            ;;; Website
                            htmlize

                            ;;; Optional
                            keycast
                            ef-themes

                            ;;; Programming
                            lua-mode 
                            
                                 )  "Default packages.")

(setq package-selected-packages puremacs/packages)

(defun puremacs/packages-installed-p ()
  "Looping all the packages."
  (cl-loop for pkg in puremacs/packages
	when (not (package-installed-p pkg)) do (cl-return nil)
	finally (cl-return t)))

(unless (puremacs/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg puremacs/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))

;;------------------------------------------------------------------------------
;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)
;;--------------------------------------------------------------------

(when (display-graphic-p)
  (require 'all-the-icons))


;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  ;; (setq vertico-cycle t)
  )

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
	      history-length 1000
	      savehist-additional-variables '(mark-ring
					      global-mark-ring
					      search-ring
					      regexp-search-ring
					      extended-command-history)
	      savehist-autosave-interval 300)
  )

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  ;; Add prompt indicator to `completing-read-multiple'.
  ;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
  (defun crm-indicator (args)
    (cons (format "[CRM%s] %s"
                  (replace-regexp-in-string
                   "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                   crm-separator)
                  (car args))
          (cdr args)))
  (advice-add #'completing-read-multiple :filter-args #'crm-indicator)

  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; Emacs 28: Hide commands in M-x which do not work in the current mode.
  ;; Vertico commands are hidden in normal buffers.
  ;; (setq read-extended-command-predicate
  ;;       #'command-completion-default-include-p)

  ;; Enable recursive minibuffers
  (setq enable-recursive-minibuffers t))

(use-package corfu
  :init
  (global-corfu-mode))

;; Optionally use the `orderless' completion style.
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;;--------------------------------------------------------------------
;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init configuration is always executed (Not lazy!)
  :init

  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (marginalia-mode))

;;--------------------------------------------------------------------
;; Example configuration for Consult
(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ;; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s D" . consult-locate)
         ("M-s g" . consult-grep)
         ("M-s G" . consult-git-grep)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ;; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history))                ;; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
)

;;--------------------------------------------------------------------
(use-package embark
  :ensure t

  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;;--------------------------------------------------------------------
;; (use-package simple
;;   :ensure nil
;;   :hook (after-init . size-indication-mode)
;;   :init
;;   (progn
;;     (setq column-number-mode t)
;;     ))

;;modeline上显示我的所有的按键和执行的命令
;; (require 'keycast)
;; (add-to-list 'global-mode-string '("" keycast-mode-line))
;; (keycast-mode t)

;; 这里的执行顺序非常重要，doom-modeline-mode 的激活时机一定要在设置global-mode-string 之后‘
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))
(add-hook 'after-init-hook #'doom-modeline-mode)
(setq doom-modeline-minor-modes t)
;; (require 'minions)
;; (add-hook 'after-doom-modeline-hook #'minions-mode)

(use-package all-the-icons
  :if (display-graphic-p))

(load-theme 'doom-one t)

;;--------------------------------------------------------------------
;; Hunbgry-delete
(require 'hungry-delete)
;(hungry-delete-mode 1)
(global-hungry-delete-mode 1)

(setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(help-mode minibuffer-mode minibuffer-inactive-mode calc-mode))
;;--------------------------------------------------------------------
(add-to-list 'load-path (expand-file-name "~/.emacs.d/site-lisp/awesome-tab"))

(require 'awesome-tab)

(awesome-tab-mode t)

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




(require 'epc)

;;--------------------------------------------------------------------
;; all-the-icons-ibuffer
(add-hook 'ibuffer-mode-hook #'all-the-icons-ibuffer-mode)

(setq all-the-icons-color-icons t)
(setq all-the-icons-ibuffer-color-icon t)
(setq all-the-icons-dired-monochrome nil) ;; nil means it is colourful in dired-mode

(all-the-icons-ibuffer-mode t)

;; Whether display the icons.
(setq all-the-icons-ibuffer-icon t)

;; Whether display the colorful icons.
;; It respects `all-the-icons-color-icons'.
(setq all-the-icons-ibuffer-color-icon t)

;; The default icon size in ibuffer.
(setq all-the-icons-ibuffer-icon-size 1.0)

;; The default vertical adjustment of the icon in ibuffer.
(setq all-the-icons-ibuffer-icon-v-adjust 0.0)

;; Use human readable file size in ibuffer.
(setq  all-the-icons-ibuffer-human-readable-size t)

;; A list of ways to display buffer lines with `all-the-icons'.
;; See `ibuffer-formats' for details.
all-the-icons-ibuffer-formats



;;--------------------------------------------------------------------
(provide 'init-c-package)
;;; init-c-package.el ends here
