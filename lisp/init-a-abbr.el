;;; init-abbr.el --- Abbreviations. -*- lexical-binding: t; -*-
;;
; Copyleft (CL) 2022-2032 YF Lin
;;
;; Something good as indicated, by Dr YF Lin <e.yflin@gmail.com>
;; URL: https://github.com/Ethanlinyf/General-Pure-Emacs
;; Under ThingsEngine Project: https://www.thethingsengine.org
;;--------------------------------------------------------------------
;; Commentary:
;; Some abbrevs to be used when editting
;;--------------------------------------------------------------------
;;; Code:

(setq-default abbrev-mode t)
(setq save-abbrevs nil)
(define-abbrev-table 'global-abbrev-table '(
					    ;; example 1
					    ("exp1" "xxx")
					    ;; Tudi
					    ("exp2" "AAA")
                                            ("eline" ";;--------------------------------------------------------------------")
					    ))
;;--------------------------------------------------------------------

;;--------------------------------------------------------------------
(provide 'init-a-abbr)
;;; init-abbr.el ends here
