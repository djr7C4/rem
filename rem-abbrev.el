;;; -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2025 David J. Rosenbaum <djr7c4@gmail.com>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of version 3 of the GNU General Public License, as
;; published by the Free Software Foundation.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'dash)
(require 'cl-lib)
(require 'pcase)

(defalias 'db 'cl-destructuring-bind)
(defalias 'mvb 'cl-multiple-value-bind)
(defalias 'mvs 'cl-multiple-value-setq)
(defalias 'with-gensyms 'cl-with-gensyms)
(defalias 'once-only 'cl-once-only)
(defalias 'dflet 'noflet)
(defalias 'plet 'pcase-let)
(defalias 'plet* 'pcase-let*)
(defalias 'psetq* 'pcase-setq)
(defalias 'pdolist 'pcase-dolist)
(defalias 'plambda 'pcase-lambda)
(defalias 'pdefmacro 'pcase-defmacro)
(defalias 'epcase 'pcase-exhaustive)
(defalias 'fn 'rem-fn)

(defmacro rem-define-fn-aliases ()
  `(progn
     ,@(mapcar (lambda (k)
                 `(defalias ',(intern (format "fn%d" k)) ',(intern (format "rem-fn%d" k))))
               (-iota 10 1))))

(rem-define-fn-aliases)

(provide 'rem-abbrev)
;;; rem-abbrev.el ends here
