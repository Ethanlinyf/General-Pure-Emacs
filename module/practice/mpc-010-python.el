;;; mpc-010-python.el --- Elisp Template . -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;;

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
(provide 'mpc-010-python)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; mpc-010-python.el ends here.
