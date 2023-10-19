;;; init-e-enhance.el --- Enhancement. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;;; Commentary:
;; Some enhancement for editting
;;--------------------------------------------------------------------
;;; Code:

;; Session history, enhance package, desktop+, which extends `desktop'
;; by providing more features related to sessions persistance.
;; (use-package desktop
;;   :hook (after-init . desktop-save-mode))

;; Indent Region or Buffer
(use-package GPE-indent
  :ensure nil
  :commands (indent-region-or-buffer)
  :bind (("C-M-\\" . indent-region-or-buffer))
  :init
  (defun indent-buffer()
    "To indent the buffer."
    (interactive)
    (indent-region (point-min) (point-max)))

  (defun indent-region-or-buffer()
    "To indent the region or buffer."
    (interactive)
    (save-excursion
      (if (region-active-p)
          (progn
            (indent-region (region-beginning) (region-end))
            (message "Indent selected region."))
        (progn
          (indent-buffer)
          (message "Indent buffer."))))))

;;--------------------------------------------------------------------
;; abbrev settings
(use-package GPE-abbrev
  :ensure nil
  :init
  (setq-default abbrev-mode t)
  (setq save-abbrevs nil)

  (defconst abbrev-file (expand-file-name "abbrev_defs" user-emacs-directory))
  (unless (file-exists-p abbrev-file)
    (shell-command (concat "touch " abbrev-file)))
  (setq abbrev-file-name abbrev-file))

;;--------------------------------------------------------------------
;; Further Enhancement
(use-package GPE-enhancement
  :ensure nil
  :init
  (setq delete-by-moving-to-trash t) ; disable delete directly
  (add-hook 'after-init-hook 'electric-pair-mode)
  (add-hook 'after-init-hook 'winner-mode)
  (add-hook 'after-init-hook 'global-auto-revert-mode)
  (add-hook 'after-init-hook 'electric-indent-mode)
  (add-hook 'prog-mode-hook (lambda() (setq split-width-threshold 80)))

  (add-hook 'after-init-hook 'show-paren-mode)
  (define-advice show-paren-function (:around (fn) fix-show-paren-function)
    "Highlight enclosing parens."
    (cond ((looking-at-p "\\s(") (funcall fn))
          (t (save-excursion
               (ignore-errors (backward-up-list))
               (funcall fn))))))

;;--------------------------------------------------------------------
;;happie-expand
(use-package GPE-hippie-expand
  :ensure nil
  :commands (hippie-expand)
  :bind ("M-/" . hippie-expand)
  :init
  (setq hippie-expand-try-function-list '(try-expand-debbrev
                      try-expand-debbrev-all-buffers
                      try-expand-debbrev-from-kill
                      try-complete-file-name-partially
                      try-complete-file-name
                      try-expand-all-abbrevs
                      try-expand-list
                      try-expand-line
                      try-complete-lisp-symbol-partially
                      try-complete-lisp-symbol)))

;;--------------------------------------------------------------------
;; from centaur emacs
(use-package recentf
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init
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
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude)
  (add-to-list 'recentf-filename-handlers #'abbreviate-file-name)
  (when sys/macp
    (global-set-key (kbd "s-3") 'recentf-open-files))
  )

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; ALT + number to swith to the number of a specific window
(use-package window-numbering
  :hook (after-init . window-numbering-mode))

(use-package which-key
  :diminish
  :bind ("C-h M-m" . which-key-show-major-mode)
  :hook (after-init . which-key-mode)
  :config
  (setq which-key-max-description-length 30
        which-key-show-remaining-keys t))

;;--------------------------------------------------------------------
;; Hungry deletion
(use-package hungry-delete
  :diminish
  :hook (after-init . global-hungry-delete-mode)
  :init (setq hungry-delete-chars-to-skip " \t\f\v"
              hungry-delete-except-modes
              '(minibuffer-mode help-modeminibuffer-inactive-mode calc-mode)))

;;--------------------------------------------------------------------
;; spell checking
;; (use-package jinx
;;  :ensure t
;;  :hook
;;  (tex-mode . jinx-mode)
;;  (org-mode . jinx-mode)
;;  (conf-mode . jinx-mode)
;;  :hook
;;  (emacs-startup . global-jinx-mode))

;;--------------------------------------------------------------------
;; An intuitive and efficient solution for single-buffer text search
(use-package ctrlf
  :ensure t
  :init
  (ctrlf-mode +1))

;;--------------------------------------------------------------------
;; goto-line-preview
(use-package goto-line-preview
  :ensure t)

;;--------------------------------------------------------------------
;; golden-ratio
(use-package golden-ratio
  :ensure t
  ;;:hook
  ;;(after-init . golden-ratio-mode)
  :config
  (setq golden-ratio-auto-scale t))

;;--------------------------------------------------------------------
;; undo-tree
(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :init (setq undo-tree-visualizer-timestamps t
              undo-tree-visualizer-diff t
              undo-tree-enable-undo-in-region nil
              undo-tree-auto-save-history nil))
;;--------------------------------------------------------------------
;; Elisp API Demos
;; C-h x command RET (describe-command) displays the documentation of the named command, in a window.
(use-package elisp-demos
  :ensure t
  :init
  (advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1))

;;--------------------------------------------------------------------
;; ace-window
(use-package ace-window
  :ensure t
  :hook
  (emacs-startup . ace-window-display-mode)
  :bind
  ([remap other-window] . ace-window))

;;--------------------------------------------------------------------
(use-package multiple-cursors
  :bind
  ("C-s-<mouse-1>" . mc/toggle-cursor-on-click))


;; To be enhanced as follows:
(use-package multiple-cursors
  :ensure t
  :after hydra
  :bind
  (("C-x C-h m" . hydra-multiple-cursors/body)
   ("C-S-<mouse-1>" . mc/toggle-cursor-on-click))
  :hydra (hydra-multiple-cursors
      (:hint nil)
      "
Up^^             Down^^           Miscellaneous           % 2(mc/num-cursors) cursor%s(if (> (mc/num-cursors) 1) \"s\" \"\")
------------------------------------------------------------------
 [_p_]   Prev     [_n_]   Next     [_l_] Edit lines  [_0_] Insert numbers
 [_P_]   Skip     [_N_]   Skip     [_a_] Mark all    [_A_] Insert letters
 [_M-p_] Unmark   [_M-n_] Unmark   [_s_] Search      [_q_] Quit
 [_|_] Align with input CHAR       [Click] Cursor at point"
      ("l" mc/edit-lines :exit t)
      ("a" mc/mark-all-like-this :exit t)
      ("n" mc/mark-next-like-this)
      ("N" mc/skip-to-next-like-this)
      ("M-n" mc/unmark-next-like-this)
      ("p" mc/mark-previous-like-this)
      ("P" mc/skip-to-previous-like-this)
      ("M-p" mc/unmark-previous-like-this)
      ("|" mc/vertical-align)
      ("s" mc/mark-all-in-region-regexp :exit t)
      ("0" mc/insert-numbers :exit t)
      ("A" mc/insert-letters :exit t)
      ("<mouse-1>" mc/add-cursor-on-click)
      ;; Help with click recognition in this hydra
      ("<down-mouse-1>" ignore)
      ("<drag-mouse-1>" ignore)
      ("q" nil)))

;;--------------------------------------------------------------------
;; Rectangle from Centaur Emacs
(use-package rect
  :ensure nil
  :bind (:map text-mode-map
              ("<C-return>" . rect-hydra/body)
              :map prog-mode-map
              ("<C-return>" . rect-hydra/body))
  :init
  (with-eval-after-load 'org
    (bind-key "<s-return>" #'rect-hydra/body org-mode-map))
  (with-eval-after-load 'wgrep
    (bind-key "<C-return>" #'rect-hydra/body wgrep-mode-map))
  (with-eval-after-load 'wdired
    (bind-key "<C-return>" #'rect-hydra/body wdired-mode-map))
  :pretty-hydra
  ((:title (pretty-hydra-title "Rectangle" 'material "border_all" :height 1.2 :v-adjust -0.225)
           :color amaranth :body-pre (rectangle-mark-mode) :post (deactivate-mark) :quit-key ("q" "C-g"))
   ("Move"
    (("h" backward-char "←")
     ("j" next-line "↓")
     ("k" previous-line "↑")
     ("l" forward-char "→"))
    "Action"
    (("w" copy-rectangle-as-kill "copy") ; C-x r M-w
     ("y" yank-rectangle "yank")         ; C-x r y
     ("t" string-rectangle "string")     ; C-x r t
     ("d" kill-rectangle "kill")         ; C-x r d
     ("c" clear-rectangle "clear")       ; C-x r c
     ("o" open-rectangle "open"))        ; C-x r o
    "Misc"
    (("N" rectangle-number-lines "number lines")        ; C-x r N
     ("e" rectangle-exchange-point-and-mark "exchange") ; C-x C-x
     ("u" undo "undo")
     ("r" (if (region-active-p)
              (deactivate-mark)
            (rectangle-mark-mode 1))
      "reset")))))

;;--------------------------------------------------------------------
(use-package highlight-symbol
  :ensure t
  :bind ("<f7>" . highlight-symbol))

;;--------------------------------------------------------------------
;; enhance the helpful
(use-package helpful
  :ensure t
  :commands (helpful-callable helpful-variable helpful-command helpful-key helpful-mode)
  :bind (([remap describe-command] . helpful-command)
         ("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h s" . helpful-symbol)
         ("C-h S" . describe-syntax)
         ("C-h m" . describe-mode)
         ("C-h F" . describe-face)
         ([remap describe-key] . helpful-key))
  )

;;--------------------------------------------------------------------
(use-package mwim
  :ensure t
  :bind
  ("C-a" . mwim-beginning-of-code-or-line)
  ("C-e" . mwim-end-of-code-or-line))

;;--------------------------------------------------------------------
;; Markmacro
(use-package markmacro
  :ensure nil
  :load-path "site-lisp/markmacro"
  :bind (("s-/" . markmacro-mark-words)
         ("s-?" . markmacro-mark-lines)
         ("s-<" . markmacro-apply-all)
         ("s->" . markmacro-apply-all-except-first)
         ("s-M" . markmacro-rect-set)
         ("s-D" . markmacro-rect-delete)
         ("s-F" . markmacro-rect-replace)
         ("s-I" . markmacro-rect-insert)
         ("s-C" . markmacro-rect-mark-columns)
         ("s-S" . markmacro-rect-mark-symbols)))

;;-------------------------------------------------------------------------------------------------
(provide 'init-e-enhance)
;;; init-e-enhance.el ends here
