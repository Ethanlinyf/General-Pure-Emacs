;;; init-t-minibuffer.el --- minibuffer -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Minibufer enhancement, M-x
;; 1. Vertico
;; 1.1 vertico-directory
;; 1.2 Savehist
;; 2. Orderless
;; 3. Marginalia
;; 4. Embark
;; 5. Consult
;; 6. Embark-Consult and Wgrep
;; 7. Fine tune Vertico with extensions. 
;;--------------------------------------------------------------------
;;; Code:

(require 'cl-lib)

;; Enable vertico
(use-package vertico
  :ensure t
  :bind (("M-P" . vertico-repeat)
         :map vertico-map
         ("<tab>" . vertico-insert)
         ("<escape>" . minibuffer-keyboard-quit)
         ("C-M-n" . vertico-next-group)
         ("C-M-p" . vertico-previous-group)
         ("M-RET" . minibuffer-force-complete-and-exit)
         ("M-TAB" . minibuffer-complete)
         :map minibuffer-local-map
         ("M-h" . backward-kill-word))
  :init
  (vertico-mode)
  :hook
  (minibuffer-setup . vertico-repeat-save)
  :custom
  (vertico-scroll-margin 0)  ;; Different scroll margin, the default is 2.
  (vertico-count 10)   ;; Show more candidates
  (vertico-resize t)  ;; Grow and shrink the Vertico minibuffer
  (vertico-cycle nil)  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (vertico-grid-separator "       ")
  (vertico-grid-lookahead 50)
  (vertico-buffer-display-action '(display-buffer-reuse-window))
  :config
  ;; (global-set-key "\M-R" #'vertico-repeat)
  ;; "» ", as an indicator infront of the candidate 
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))

  ;; Use `consult-completion-in-region' if consult is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  ;; (setq-default completion-in-region-function
  ;;               (lambda (&rest args)
  ;;                 (apply (if vertico-mode
  ;;                            #'consult-completion-in-region
  ;;                          #'completion-in-region)
  ;;                        args)))
  
  ;; Clean up the path when changing directories with shadowed paths syntax, such as "~".
  ;; This works with `file-name-shadow-mode'.  
  ;; (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)
  ;; (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)

  ;; some commands are problematic and automatically show the "*Completions*" buffer.
  ;; (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions) ;; tmm: text mode access to menu-bar
  ;; (advice-add #'ffap-menu-ask :around (lambda (&rest args)
  ;;                                       (cl-letf (((symbol-function #'minibuffer-completion-help)
  ;;                                                  #'ignore))
  ;;                                         (apply args))))
  
  ;; Alternative 1: Use the basic completion style
  ;; (setq org-refile-use-outline-path 'file
  ;;       org-outline-path-complete-in-steps t)
  ;; (advice-add #'org-olpath-completing-read :around
  ;;             (lambda (&rest args)
  ;;               (minibuffer-with-setup-hook
  ;;                   (lambda () (setq-local completion-styles '(basic)))
  ;;                 (apply args))))

  ;; Alternative 2: Complete full paths
  ;; (setq org-refile-use-outline-path 'file
  ;;       org-outline-path-complete-in-steps nil)

  ;; some key bindings
  ;; ("C-w" . vertico-directory-delete-word)
  ;; ("C-<backspace>" . vertico-directory-delete-word)
  )

;; Configure directory extension with more convenient directory navigation commands
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)) ;; to tidy shadowed file names

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("M-G" . vertico-multiform-grid)
              ("M-F" . vertico-multiform-flat)
              ("M-R" . vertico-multiform-reverse)
              ("M-U" . vertico-multiform-unobtrusive))
  :init
  (vertico-multiform-mode)
  :config
  (setq vertico-multiform-categories
        '((file grid indexed)
          (consult-location buffer)
          (consult-grep buffer)
          (minor-mode reverse)
          (library reverse indexed)
          (imenu buffer)
          (org-roam-node reverse indexed)
          (t unobtrusive)
          ))
  (setq vertico-multiform-commands
        '((consult-dir reverse)
          ("flyspell-correct-*" grid reverse)
          (execute-extended-command indexed)
          (org-refile grid reverse indexed)
          (embark-prefix-help-command flat)
          (consult-yank-pop indexed)
          (consult-flycheck)
          (completion-at-point reverse))))

(use-package vertico-quick
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("C-i" . vertico-quick-insert) ;; insert to excute
              ("C-o" . vertico-quick-exit) ;; excute
              ))

;; When resizing the minibuffer (e.g., via the mouse), adjust the number of visible candidates in Vertico automatically.
;; (defun vertico-resize--minibuffer ()
;;   (add-hook 'window-size-change-functions
;;             (lambda (win)
;;               (let ((height (window-height win)))
;;                 (when (/= (1- height) vertico-count)
;;                   (setq-local vertico-count (1- height))
;;                   (vertico--exhibit))))
;;             t t))

;; (advice-add #'vertico--setup :before #'vertico-resize--minibuffer)

;;--------------------------------------------------------------------
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
                                              extended-command-history
                                              vertico-repeat-history)
              savehist-autosave-interval 300))

;;--------------------------------------------------------------------
;; (use-package corfu
;;   :hook
;;   (after-init . global-corfu-mode))

;;--------------------------------------------------------------------
;; Optionally use the 'orderless' completion style.
(use-package orderless
  :demand t ;; it is better to be loaded immediately
  :config
  ;;   (advice-add +vertico--company-capf--candidates-a (&rest args)
  ;;     "Highlight company matches correctly, and try default completion styles before
  ;; orderless."
  ;;     :around #'company-capf--candidates
  ;;     (let ((orderless-match-faces [completions-common-part])
  ;;           (completion-styles +vertico-company-completion-styles))
  ;;       (apply args)))
  
  (defun +vertico-orderless-dispatch (pattern _index _total)
    (cond
     ;; Ensure $ works with Consult commands, which add disambiguation suffixes
     ((string-suffix-p "$" pattern)
      `(orderless-regexp . ,(concat (substring pattern 0 -1) "[\x200000-\x300000]*$")))
     ;; Ignore single !
     ((string= "!" pattern) `(orderless-literal . ""))
     ;; Without literal
     ((string-prefix-p "!" pattern) `(orderless-without-literal . ,(substring pattern 1)))
     ;; Character folding
     ((string-prefix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 1)))
     ((string-suffix-p "%" pattern) `(char-fold-to-regexp . ,(substring pattern 0 -1)))
     ;; Initialism matching
     ((string-prefix-p "`" pattern) `(orderless-initialism . ,(substring pattern 1)))
     ((string-suffix-p "`" pattern) `(orderless-initialism . ,(substring pattern 0 -1)))
     ;; Literal matching
     ((string-prefix-p "=" pattern) `(orderless-literal . ,(substring pattern 1)))
     ((string-suffix-p "=" pattern) `(orderless-literal . ,(substring pattern 0 -1)))
     ;; Flex matching
     ((string-prefix-p "~" pattern) `(orderless-flex . ,(substring pattern 1)))
     ((string-suffix-p "~" pattern) `(orderless-flex . ,(substring pattern 0 -1)))))

  ;;;autoload
  (defun +vertico-basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))

  ;;;autoload
  (defun +vertico-basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  
  (add-to-list
   'completion-styles-alist
   '(+vertico-basic-remote
     +vertico-basic-remote-try-completion
     +vertico-basic-remote-all-completions
     "Use basic completion on remote files only"))
  
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        ;; note that despite override in the name orderless can still be used in
        ;; find-file etc.
        completion-category-overrides '((file (styles +vertico-basic-remote orderless partial-completion)))
        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")
  
  ;; ...otherwise find-file gets different highlighting than other commands
  (set-face-attribute 'completions-first-difference nil :inherit nil))

;;--------------------------------------------------------------------
;; Enable richer annotations using the Marginalia package
(use-package marginalia
  ;; Either bind `marginalia-cycle` globally or only in the minibuffer
  :bind (("M-A" . marginalia-cycle) ; for globally
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle)); for locally

  ;; The :init configuration is always executed (Not lazy!)
  :hook 
  ;; Must be in the :init section of use-package such that the mode gets
  ;; enabled right away. Note that this forces loading the package.
  (after-init . marginalia-mode)
  (marginalia . all-the-icons-completion-marginalia-setup)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'center)
  :config
  (advice-add #'marginalia--project-root :override #'projectile-project-root)
  (cl-pushnew '(projectile-recentf . project-file) marginalia-command-categories) ;; The cl-pushnew does not work, but it could use with the custom.el
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

;;--------------------------------------------------------------------
;; Example configuration for Consult

;; (use-package counsel-projectile
;;   :ensure t)

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
         ;; ("C-x p b" . consult-project-buffer)      ;; orig. project-switch-to-buffer
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



;; Consult users will also want the emConsultbark-consult package.
(use-package embark-consult
  :ensure t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))


(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode))

;; (use-package embark-consult
;;   :after (embark consult)
;;   :config
;;   (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))


(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :config (setq wgrep-auto-save-buffer t))


(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (add-hook 'doom-after-reload-hook #'posframe-delete-all))


;;--------------------------------------------------------------------
(provide 'init-c-minibuffer)
;;; init-c-minibuffer.el ends here. 
