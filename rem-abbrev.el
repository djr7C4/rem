;;; -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'mmt)
(require 'pcase)

(defalias 'db 'cl-destructuring-bind)
(defalias 'mvb 'cl-multiple-value-bind)
(defalias 'mvs 'cl-multiple-value-setq)
(defalias 'with-gensyms 'mmt-with-gensyms)
(defalias 'once-only 'mmt-once-only)
(defalias 'dflet 'noflet)
(defalias 'pclet 'pcase-let)
(defalias 'pclet* 'pcase-let*)
(defalias 'pcsetq* 'pcase-setq)
(defalias 'pcdolist 'pcase-dolist)
(defalias 'pclambda 'pcase-lambda)
(defalias 'pcdefmacro 'pcase-defmacro)
(defalias 'epcase 'pcase-exhaustive)

(provide 'rem-abbrev)
;;; rem-abbrev.el ends here
