
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

;;--------------------------------------------------------------------
(setq inhibit-compacting-font-caches t)

;;--------------------------------------------------------------------
(setq load-prefer-newer t)
