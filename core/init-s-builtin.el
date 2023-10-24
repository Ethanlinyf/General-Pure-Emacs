;;; init-s-builtin.el --- Basic GPE settings -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; init file for General Pure Emacs basic branch

;;; code:
(setq-default
 initial-scratch-message (concat
                          ";;--------------------------------------------------------
;; Welcome to G.P.E Basic Platform
;; Somethng Good as Indicated:\n\n\n")
 line-spacing 0.1
 truncate-lines nil
 word-wrap t)

(setq-default require-final-newline t)
;; (setq-default scroll-conservatively 1000)
(setq-default read-process-output-max (* 4 1024 1024))
(setq-default show-trailing-whitespace t)
(setq-default use-short-answers t)
(setq-default abbrev-mode t)
;; (fset 'yes-or-no-p 'y-or-n-p)
(global-set-key (kbd "C-c C-'") 'set-mark-command)

;; Disable the ring bell function
(setq ring-bell-function 'ignore)

(setq fast-but-imprecise-scrolling t)

;; auto revert
;; `global-auto-revert-mode' is provided by autorevert.el (builtin)
(add-hook 'after-init-hook 'global-auto-revert-mode)

;; auto save to the visited file (provided by `files.el')
(add-hook 'after-init-hook 'auto-save-visited-mode)

(add-hook 'prog-mode-hook (lambda() (setq split-width-threshold 80)))

;; Delete trailing white space
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Enable to delete the selection
(add-hook 'after-init-hook #'delete-selection-mode)

;; Electric-Pair
(add-hook 'after-init-hook #'electric-indent-mode)

;; Highlight line mode
(when (display-graphic-p)
  (add-hook 'after-init-hook #'global-hl-line-mode))

;; Recent Files
(add-hook 'after-init-hook (lambda ()
			                 (recentf-mode 1)
			                 (add-to-list 'recentf-exclude '("~\/.emacs.d\/elpa\/" "~\/.emacs.d\/.cache\/treemacs-persist\/"))))
(setq-default recentf-max-menu-items 20)
(global-set-key (kbd "C-x C-r") #'recentf-open-files)
(setq recentf-max-saved-items 300
      recentf-exclude
      '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
        "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
        "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
        "^/tmp/" "^/var/folders/.+$" "^/ssh:" "/persp-confs/"
        (lambda (file) (file-in-directory-p file package-user-dir))))
(add-to-list 'recentf-exclude
             (expand-file-name "company-statistics-cache.el" user-emacs-directory)
             (expand-file-name "elgrep-data.el" user-emacs-directory))
(when sys/macp
  (global-set-key (kbd "s-3") 'recentf-open-files))

;; Save Place
(add-hook 'after-init-hook 'save-place-mode)

(defun pulse-save-buffers (&rest args)
  (save-some-buffers t)
  (pulse-momentary-highlight-one-line (point)))
;; auto save when frame lose focus, Alt-Tab
(add-function :after after-focus-change-function #'pulse-save-buffers)
;; auto save when buffer changed
(dolist (command '(other-window
                   switch-to-buffer
                   next-buffer
                   previous-buffer))
  (advice-add command :after #'pulse-save-buffers))

(require 'which-key)
(add-hook 'after-init-hook #'which-key-mode)

;; enable hippie implementation
(setq hippie-expand-try-function-list '(try-expand-debbrev
					                    try-expand-debbrev-all-buffers
					                    try-expand-debbrev-from-kill
					                    try-complete-file-name-partially
					                    try-complete-file-name
					                    try-expand-all-abbrevs
					                    try-expand-list
					                    try-expand-line
					                    try-complete-lisp-symbol-partially
					                    try-complete-lisp-symbol))

(global-set-key (kbd "s-/") 'hippie-expand)

(when emacs/>=29p
  (setq pixel-scroll-precision-mode 1)
  ;; (setq visible-bell t)
  (setq ring-bell-function 'ignore)
  (global-set-key (kbd "<f1>") 'scratch-buffer))

;;----------------------- Dired Mode ---------------------------------
(with-eval-after-load "dired"
  (put 'dired-find-alternate-file 'disabled nil)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file)
  (define-key dired-mode-map (kbd "<mouse-2>") 'dired-find-alternate-file))

(when *is-mac*
  (setq insert-directory-program "gls" dired-use-ls-dired t)
  (setq dired-listing-switches "-al --group-directories-first"))

;;-------------------------------------------------------------------------------------------------
(provide 'init-s-builtin)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; init-s-builtin.el ends here.
