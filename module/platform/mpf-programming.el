;;; mpf-programming.el --- Elisp Template . -*- lexical-binding: t; -*-
;;
;; Copyleft (CL) 2022-2032 Dr YF Lin
;; Under ThingsEngine Project: https://www.thethingsengine.org

;;; Commentary:
;; settings for programming.

;;; Code:

(require 'format-all)
(add-hook 'prog-mode-hook #'format-all-mode)
(global-set-key (kbd "C-c f") 'format-all-region-or-buffer)

;; only use spaces instead of TAB, use C-q TAB to input the TAB char
(setq-default indent-tabs-mode nil)  ; avoid to mixture the tabs and spaces in code
(setq-default tab-width 4)

(require 'flycheck)
(add-hook 'prog-mode-hook #'flycheck-mode)

(use-package flycheck-indicator
  :hook (flycheck-mode . flycheck-indicator-mode))

;;--------------------------------------------------------------------
;; treesit implementation
(require 'treesit)
;; M-x `treesit-install-language-grammar` to install language grammar.
(setq treesit-language-source-alist
      '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
        (c . ("https://github.com/tree-sitter/tree-sitter-c"))
        (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
        (css . ("https://github.com/tree-sitter/tree-sitter-css"))
        (cmake . ("https://github.com/uyha/tree-sitter-cmake"))
        (csharp     . ("https://github.com/tree-sitter/tree-sitter-c-sharp.git"))
        (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
        (elisp . ("https://github.com/Wilfred/tree-sitter-elisp"))
        (go . ("https://github.com/tree-sitter/tree-sitter-go"))
        (gomod      . ("https://github.com/camdencheek/tree-sitter-go-mod.git"))
        (html . ("https://github.com/tree-sitter/tree-sitter-html"))
        (java       . ("https://github.com/tree-sitter/tree-sitter-java.git"))
        (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
        (json . ("https://github.com/tree-sitter/tree-sitter-json"))
        (lua . ("https://github.com/Azganoth/tree-sitter-lua"))
        (make . ("https://github.com/alemuller/tree-sitter-make"))
        (markdown . ("https://github.com/MDeiml/tree-sitter-markdown" nil "tree-sitter-markdown/src"))
        (ocaml . ("https://github.com/tree-sitter/tree-sitter-ocaml" nil "ocaml/src"))
        (org . ("https://github.com/milisims/tree-sitter-org"))
        (python . ("https://github.com/tree-sitter/tree-sitter-python"))
        (php . ("https://github.com/tree-sitter/tree-sitter-php"))
        (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
        (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
        (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
        (rust . ("https://github.com/tree-sitter/tree-sitter-rust"))
        (sql . ("https://github.com/m-novikov/tree-sitter-sql"))
        (vue . ("https://github.com/merico-dev/tree-sitter-vue"))
        (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
        (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
        (zig . ("https://github.com/GrayJack/tree-sitter-zig"))))

(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (conf-toml-mode  . toml-ts-mode)
        (css-mode        . css-ts-mode)
        (js-mode         . js-ts-mode)
        (js-json-mode    . json-ts-mode)
        (python-mode     . python-ts-mode)
        (sh-mode         . bash-ts-mode)
        (typescript-mode . typescript-ts-mode)
        (rust-mode       . rust-ts-mode)
        (java-mode       . java-ts-mode)
        ))

(add-hook 'markdown-mode-hook #'(lambda () (treesit-parser-create 'markdown)))

(add-hook 'zig-mode-hook #'(lambda () (treesit-parser-create 'zig)))

(add-hook 'web-mode-hook #'(lambda ()
                             (let ((file-name (buffer-file-name)))
                               (when file-name
                                 (treesit-parser-create
                                  (pcase (file-name-extension file-name)
                                    ("vue" 'vue)
                                    ("html" 'html)
                                    ("php" 'php))))
                               )))

(add-hook 'emacs-lisp-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'ielm-mode-hook #'(lambda () (treesit-parser-create 'elisp)))
(add-hook 'json-mode-hook #'(lambda () (treesit-parser-create 'json)))
(add-hook 'go-mode-hook #'(lambda () (treesit-parser-create 'go)))
(add-hook 'java-mode-hook #'(lambda () (treesit-parser-create 'java)))
(add-hook 'java-ts-mode-hook #'(lambda () (treesit-parser-create 'java)))
(add-hook 'php-mode-hook #'(lambda () (treesit-parser-create 'php)))
(add-hook 'php-ts-mode-hook #'(lambda () (treesit-parser-create 'php)))
(add-hook 'java-ts-mode-hook #'(lambda () (treesit-parser-create 'java)))

;;-------------------------------------------------------------------------------------------------
(provide 'mpf-programming)

;; Local Variables:
;; byte-compile-warnings: (not free-vars unresolved)
;; End:
;;; mpf-programming.el ends here.
