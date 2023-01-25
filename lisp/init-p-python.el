;;; init-python.el --- Settings for Python. -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;;
;; Author: Ethan YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org/
;;--------------------------------------------------------------------
;;; Commentary:
;; Configurations for Python programming
;;--------------------------------------------------------------------
;;; Code:
;; (with-eval-after-load 'python
;;   (defun python-shell-completion-native-try ()
;;     "Return non-nil if can trigger native completion."
;;     (let ((python-shell-completion-native-enable t)
;;           (python-shell-completion-native-output-timeout
;;            python-shell-completion-native-try-output-timeout))
;;       (python-shell-completion-native-get-completions
;;        (get-buffer-process (current-buffer))
;;        nil "_"))))

(setq python-shell-completion-native-enable nil)

(use-package python
  :ensure nil
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  ;; Disable readline based native completion
  (setq python-shell-completion-native-enable nil)
  :config
  ;; Default to Python 3. Prefer the versioned Python binaries since some
  ;; systems stupidly make the unversioned one point at Python 2.
  (setq python-indent-offset 4)
  (when (and (executable-find "python")
             (string= python-shell-interpreter "python"))
    (setq python-shell-interpreter "python"))

  ;; Env vars
  (with-eval-after-load 'exec-path-from-shell
    (exec-path-from-shell-copy-env "PYTHONPATH"))

  ;; Live Coding in Python
  (use-package live-py-mode))

;; (use-package pyvenv
;;   :ensure t
;;   :config
;;   (setenv "WORKON_HOME" "~/miniconda3/envs")
;;   (setq python-shell-interpreter "python3")
;;   (pyvenv-mode t))

;;----------------------------------------------------------------------------
(provide 'init-p-python)
;;; init-p-python.el ends here
