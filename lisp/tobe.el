
;;-------------------------------------------------------------------- 
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

;;---------------- Emacs-Application-Framework -----------------------
;; (add-to-list 'load-path "~/.emacs.d/site-lisp/emacs-application-framework/")
;; (require 'eaf)

    ;; (add-hook 'inferior-python-mode-hook
    ;;           #'(lambda nil
    ;;               (process-query-on-exit-flag
    ;;                (get-process "Python"))))
    ;; (eval-after-load 'python
    ;;   #'(lambda nil
    ;;       (progn
    ;;         (if
    ;;             (and
    ;;              (executable-find "python3")
    ;;              (string= python-shell-interpreter "python"))
    ;;             (progn
    ;;               (setq python-shell-interpreter "python3")))
    ;;         (eval-after-load 'exec-path-from-shell
    ;;           #'(lambda nil
    ;;               (exec-path-from-shell-copy-env "PYTHONPATH"))))))
;; (setq eaf--mac-enable-rosetta t)
;; (require 'eaf-demo)
;; (require 'eaf-file-sender)
;; (require 'eaf-music-player)
;; (require 'eaf-camera)
;; (require 'eaf-rss-reader)
;; (require 'eaf-terminal)
;; (require 'eaf-image-viewer)
;; ;; (require 'eaf-vue-demo)
;; (require 'eaf-pdf-viewer)
;; (require 'eaf-browser)
;; (require 'eaf-markdown-previewer)
;; (require 'eaf-file-browser)
;; (require 'eaf-mermaid)
;; (require 'eaf-file-manager)
;; (require 'eaf-mindmap)
;; (require 'eaf-video-player)
;; (require 'eaf-org-previewer)
;; (require 'eaf-airshare)
;; (require 'eaf-jupyter)
;; (require 'eaf-netease-cloud-music)
;;(require 'eaf-git)
;; (require 'eaf-system-monitor)


;--------------------------------------------------------------------
 ; A modern Packages Menu
;; (use-package paradox
;;   :custom-face
;;   (paradox-archive-face ((t (:inherit font-lock-doc-face))))
;;   (paradox-description-face ((t (:inherit completions-annotations))))
;;   :hook (after-init . paradox-enable)
;;   :init (setq paradox-execute-asynchronously t
;;               paradox-github-token t
;;               paradox-display-star-count nil
;;               paradox-status-face-alist ;
;;               '(("built-in"   . font-lock-builtin-face)
;;                 ("available"  . success)
;;                 ("new"        . (success bold))
;;                 ("held"       . font-lock-constant-face)
;;                 ("disabled"   . font-lock-warning-face)
;;                 ("avail-obso" . font-lock-comment-face)
;;                 ("installed"  . font-lock-comment-face)
;;                 ("dependency" . font-lock-comment-face)
;;                 ("incompat"   . font-lock-comment-face)
;;                 ("deleted"    . font-lock-comment-face)
;;                 ("unsigned"   . font-lock-warning-face)))
;;   :config
;;   (add-hook 'paradox-after-execute-functions
;;             (lambda (_)
;;               "Display `page-break-lines' in \"*Paradox Report*\" buffer."
;;               (when (fboundp 'page-break-lines-mode)
;;                 (let ((buf (get-buffer "*Paradox Report*"))
;;                       (inhibit-read-only t))
;;                   (when (buffer-live-p buf)
;;                     (with-current-buffer buf
;;                       (page-break-lines-mode 1))))))
;;             t))

;;--------------------------------------------------------------------
(setq inhibit-compacting-font-caches t)

;;--------------------------------------------------------------------
(setq load-prefer-newer t)
