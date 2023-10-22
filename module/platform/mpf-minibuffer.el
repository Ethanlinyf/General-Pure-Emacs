;;; mpf-minibuffer.el --- Basic GPE settings -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; Minibufer enhancement, M-x
;; 1. Vertico
;; 1.1 vertico-directory
;; 1.2 vertico-multiform
;; 1.3 vertico-quick
;; 2.1 Savehist
;; 2.2 precient-vertico: sorting
;; 3. Orderless
;; 4. Marginalia
;; 5. Embark
;; 6. Consult
;; 7. Embark-Consult and Wgrep
;; 8. Fine tune Vertico with extensions.
;; 9. all-the-icons-completion
;; 10. popper
;; 11 avy and with Embark
;;--------------------------------------------------------------------
;;; Code:

(require 'cl-lib)

;;--------------------------------------------------------------------
;; completion by vertico
(use-package vertico
  :ensure t
  :bind (("M-P" . vertico-repeat) ; effective in the specific mode
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
  (minibuffer-setup .vertico-repeat-save)
  :custom
  (vertico-scroll-margin 0)
  (vertico-count 10)
  (vertico-resize t)
  (vertico-cycle nil)
  (vertico-buffer-display-action '(display-buffer-reuse-window)) ; need to be clear
  :config
  ;; "» ", as an indicator infront of the candidate
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))

  ;; Problematic completion commands: org-refile
  ;; Alternative 1: Use the basic completion style
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps t)
  (advice-add #'org-olpath-completing-read :around
              (lambda (&rest args)
                (minibuffer-with-setup-hook
                    (lambda () (setq-local completion-styles '(basic)))
                  (apply args))))

  ;; Alternative 2: Complete full paths
  (setq org-refile-use-outline-path 'file
        org-outline-path-complete-in-steps nil)

  ;; tmm-menubar
  (advice-add #'tmm-add-prompt :after #'minibuffer-hide-completions)

  ;; ffap-menu:
  (advice-add #'ffap-menu-ask :around (lambda (&rest args)
                                        (cl-letf (((symbol-function #'minibuffer-completion-help)
                                                   #'ignore))
                                          (apply args)))))

;; Configure directory extension with more convenient directory navigation commands
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word) ; Mac Keyboard
              ("C-<backspace>" . vertico-directory-delete-word)) ;; for different keyboard (c-w for all)
  :hook
  (rfn-eshadow-update-overlay . vertico-directory-tidy)) ; to tidy shadowed file names

(use-package vertico-multiform
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("M-G" . vertico-multiform-grid)
              ("M-F" . vertico-multiform-flat)
              ("M-R" . vertico-multiform-reverse)
              ("M-U" . vertico-multiform-unobtrusive))

  :custom
  (vertico-grid-separator "   |    ")
  (vertico-grid-lookahead 50)

  (vertico-multiform-categories
   '((file) ;; Defaul vertico display, (file grid indexed)
     (consult-location buffer)
     (consult-grep buffer)
     (minor-mode reverse)
     (library reverse indexed)
     (imenu buffer)
     (org-roam-node reverse indexed)
     (t reverse) ;; unobtrusive
     ))
  (vertico-multiform-commands
   '((consult-dir reverse)
     ("flyspell-correct-*" grid reverse)
     (execute-extended-command indexed)
     (org-refile grid reverse indexed)
     (embark-prefix-help-command flat)
     (consult-yank-pop indexed)
     (consult-flycheck)
     (completion-at-point reverse)))
  :init
  (vertico-multiform-mode)
  )

(use-package vertico-quick
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("C-i" . vertico-quick-insert) ; insert to excute
              ("C-o" . vertico-quick-exit) ; excute
              ))

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
;; Optionally use the orderless' completion style.
(use-package orderless
  :demand t ;; it is better to be loaded immediately to enable the macro: orderless-define-completion-style
  :config
  (defun +vertico-orderless-dispatch (pattern _index _total) ; from doom-emacs
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

  ;; only for remote files:
  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))

  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides
        '((file (styles basic-remote orderless partial-completion))
          (command (styles orderless+initialism))
          (symbol (styles orderless+initialism))
          (variable (styles orderless+initialism)))

        orderless-style-dispatchers '(+vertico-orderless-dispatch)
        orderless-component-separator "[ &]")

  ;; highlighting
  (set-face-attribute 'completions-first-difference nil :inherit nil))

;;--------------------------------------------------------------------
;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :ensure t
  :bind (:map minibuffer-local-map
              ("M-A" . marginalia-cycle))
  :hook
  (after-init . marginalia-mode)
  :custom
  (marginalia-max-relative-age 0)
  (marginalia-align 'center)
  :config
  (advice-add #'marginalia--project-root :override #'projectile-project-root)

  (cl-pushnew '(flycheck-error-list-set-filter . builtin) marginalia-command-categories)
  (add-to-list 'marginalia-command-categories '(projectile-switch-to-buffer . buffer))
  (add-to-list 'marginalia-command-categories '(projectile-find-file . project-file))
  (add-to-list 'marginalia-command-categories '(projectile-recentf . project-file))
  (add-to-list 'marginalia-command-categories '(projectile-switch-project . project-file)))

;;--------------------------------------------------------------------
(use-package embark
  :ensure t
  :bind
  (("s-." . embark-act)         ; pick some comfortable binding
   ("s-;" . embark-dwim)        ; good alternative: M-.
   ("C-h B" . embark-bindings)) ; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq which-key-use-C-h-commands nil
        prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none))))

  (eval-when-compile
    (defmacro my/embark-ace-action (fn)
      `(defun ,(intern (concat "my/embark-ace-" (symbol-name fn))) ()
         (interactive)
         (with-demoted-errors "%s"
           (require 'ace-window)
           (let ((aw-dispatch-always t))
             (aw-switch-to-window (aw-select nil))
             (call-interactively (symbol-function ',fn)))))))

  (define-key embark-file-map     (kbd "o") (my/embark-ace-action find-file))
  (define-key embark-buffer-map   (kbd "o") (my/embark-ace-action switch-to-buffer))
  (define-key embark-bookmark-map (kbd "o") (my/embark-ace-action bookmark-jump))

  (eval-when-compile
    (defmacro my/embark-split-action (fn split-type)
      `(defun ,(intern (concat "my/embark-"
                               (symbol-name fn)
                               "-"
                               (car (last  (split-string
                                            (symbol-name split-type) "-"))))) ()
         (interactive)
         (funcall #',split-type)
         (call-interactively #',fn))))

  (define-key embark-file-map     (kbd "2") (my/embark-split-action find-file split-window-below))
  (define-key embark-buffer-map   (kbd "2") (my/embark-split-action switch-to-buffer split-window-below))
  (define-key embark-bookmark-map (kbd "2") (my/embark-split-action bookmark-jump split-window-below))

  (define-key embark-file-map     (kbd "3") (my/embark-split-action find-file split-window-right))
  (define-key embark-buffer-map   (kbd "3") (my/embark-split-action switch-to-buffer split-window-right))
  (define-key embark-bookmark-map (kbd "3") (my/embark-split-action bookmark-jump split-window-right)))

;;--------------------------------------------------------------------
;; Example configuration for Consult
(use-package consult
  :ensure t
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ; orig. switch-to-buffer-other-frame
         ("C-x r b" . consult-bookmark)            ; orig. bookmark-jump
         ("C-x p b" . consult-project-buffer)      ; orig. project-switch-to-buffer
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ; orig. yank-pop
         ("<help> a" . consult-apropos)            ; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ; orig. goto-line
         ("M-g o" . consult-outline)               ; Alternative: consult-org-heading
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
         ("M-e" . consult-isearch-history)         ; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)            ; needed by consult-line to detect isearch
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ; orig. next-matching-history-element
         ("M-r" . consult-history))                ; orig. previous-matching-history-element

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)
  :init
  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

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
   :preview-key "M-.")

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;; There are multiple reasonable alternatives to chose from.
  ;;;; 1. project.el (project-roots)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. projectile.el (projectile-project-root)
  ;; (autoload 'projectile-project-root "projectile")
  ;; (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 3. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 4. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  )

;; Consult users will also want the emConsultbark package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :init
  (setq wgrep-auto-save-buffer t)
  (setq wgrep-change-readonly-file t))

;; if you like postframe, you could make it available
;; (use-package vertico-posframe
;;   :ensure t
;;   :hook (vertico-mode . vertico-posframe-mode)
;;   :config
;;   (add-hook 'doom-after-reload-hook #'posframe-delete-all))

(use-package consult-dir
  :ensure t
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-projectile
  :ensure t
  :after (projectile)
  :init
  (setq projectile-switch-project-action 'projectile-dired)) ; open directory in dired-mode from dashboard

(use-package 0x0
  :ensure t)

(use-package popper
  :ensure t
  :bind (("C-`"   . popper-toggle-latest)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :config
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; for echo area hints

(global-set-key (kbd "M-j") nil)

(use-package avy
  :ensure t
  :config
  (defun avy-action-embark (pt)
    (unwind-protect
    (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)
  (setf (alist-get ?e avy-dispatch-alist) 'avy-action-embark)
  :bind
  (("M-j C-SPC" . avy-goto-char-timer)))

(provide 'mpf-minibuffer)
;;; mpf-minibuffer.el ends here
