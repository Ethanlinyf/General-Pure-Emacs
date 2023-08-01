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

(use-package python
  :ensure nil
  :hook (inferior-python-mode . (lambda ()
                                  (process-query-on-exit-flag
                                   (get-process "Python"))))
  :init
  (setq python-shell-completion-native-enable t)
  :config
  ;; Default to python3, differ from python2. 
  (setq python-indent-offset 4)
  (when (and (executable-find "python3")
             (string= python-shell-interpreter "python3"))
    (setq python-shell-interpreter "python3")))

;; Live Coding in Python
(use-package live-py-mode
  :ensure t)

;;-------------------------------------------------------------------------------------------------
(provide 'init-p-python)
;;; init-p-python.el ends here
