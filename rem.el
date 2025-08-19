;;; rem.el --- Random elisp modules -*- lexical-binding: t; -*-
;; Copyright (C) 2025 David J. Rosenbaum <djr7c4@gmail.com>

;; Author: David J. Rosenbaum <djr7c4@gmail.com>
;; Keywords: utilities
;; URL: https://github.com/djr7C4/rem
;; Version: 0.7.9
;; Package-Requires: ((llama "1.0.0"))
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

(require 'anaphora)
(require 'cl-lib)
(require 'dash)
(require 'f)
(require 'llama)
(require 'map)
(require 'rem-abbrev)
(require 'url-parse)

;;; Algorithms
(defun rem-topological-sort (nodes edges)
  "Perform a topological sort on the list NODES and return the
result. EDGES should be a mapping that maps each node to a list
of the nodes that it is connected to. Each node must be
comparable using `equal'."
  (let ((edges (map-into edges (list 'hash-table :test #'equal :size (map-length edges))))
        (in-degrees (make-hash-table :test #'equal :size (map-length nodes)))
        ;; This is a list of nodes with in-degrees of 0 (i.e. they have no
        ;; incoming edges).
        open
        sorted)
    ;; Compute the in-degree of each node.
    (cl-dolist (node nodes)
      (puthash node 0 in-degrees))
    (map-do (lambda (_node adjacent)
              (cl-dolist (node2 adjacent)
                (cl-incf (gethash node2 in-degrees))))
            edges)
    ;; Initialize the open list.
    (map-do (lambda (node in-degree)
              (when (= in-degree 0)
                (push node open)))
            in-degrees)
    (while open
      (let* ((node (pop open))
             (adjacent (gethash node edges)))
        (push node sorted)
        ;; Since we removed node from the graph, update the in-degree of each
        ;; node that it connected to.
        (cl-dolist (node2 adjacent)
          (when (= (cl-decf (gethash node2 in-degrees)) 0)
            (push node2 open)))))
    (unless (= (length sorted) (length nodes))
      (error "Failed to topologically sort the graph because it is not acyclic"))
    (reverse sorted)))

;;; Buffers
(defun rem-buffer-contents (&optional buffer)
  (with-current-buffer buffer
    (buffer-string)))

(defun rem-buffer-same-p (buffer &optional buffer2)
  (setq buffer2 (or buffer2 (current-buffer)))
  (string= (rem-buffer-contents buffer) (rem-buffer-contents buffer2)))

(defun rem-replace-region (beg end new &optional pos)
  (unless '(before beg start beginning after end)
    (error "Invalid value for pos"))
  (save-excursion
    (delete-region beg end)
    (goto-char beg)
    (insert new))
  (when pos
    (goto-char beg)
    (when (memq pos '(after end))
      (forward-char (length new)))))

;;; Clipboard
(defun rem-clipboard-kill-ring-save-string (string)
  "Copy STRING to the `kill-ring' and system clipboard."
  (with-temp-buffer
    (insert string)
    (clipboard-kill-ring-save (point-min) (point-max))))

;;; Elisp
(defvar rem-load-blacklist (list "-pkg\\.\\(el\\|elc\\)$" "\\(^\\|/\\).cask/" "\\(^\\|/\\).eask/"))

(cl-defun rem-elisp-files-to-load (dir &key compressed recursive)
  (let* ((extensions (if compressed
                         '(".el" ".el.gz")
                       '(".el")))
         (files (-filter (lambda (path)
                           (and (f-file-p path)
                                (not (member (f-filename path) (rem-dir-locals-file-names)))))
                         (f-entries dir
                                    (lambda (path)
                                      (or (f-dir-p path)
                                          ;; Using `f-ext' doesn't work since
                                          ;; for .el.gz files it returns "gz"
                                          ;; instead of "el.gz".
                                          (and (cl-some (lambda (extension)
                                                          (s-ends-with-p extension path))
                                                        extensions)
                                               (not (cl-some (-rpartial #'string-match-p path) rem-load-blacklist)))))
                                    recursive))))
    (setq files (cl-remove-duplicates files :test #'equal))))

(defun rem-elisp-dependencies (path)
  (let (code dependencies eof)
    (with-temp-buffer
      (insert-file-contents path)
      (while (not eof)
        (condition-case nil
            (push (read (current-buffer)) code)
          (end-of-file (setq eof t)))))
    (setq code (reverse code))
    (rem-subtree-map (lambda (subtree)
                       (and (consp subtree)
                            (cdr subtree)
                            (listp (cdr subtree))
                            (cl-destructuring-bind (fun arg &rest args)
                                subtree
                              (or (and (eq fun 'require)
                                       (consp arg)
                                       (cdr arg)
                                       (listp (cdr arg))
                                       (eq (car arg) 'quote)
                                       (symbolp (cadr arg))
                                       (push (cadr arg) dependencies))
                                  (and (eq fun 'load)
                                       (stringp arg)
                                       (push (intern (f-filename arg)) dependencies))))))
                     code)
    (reverse dependencies)))

(defun rem-elisp-sort-dependencies (paths)
  "Sort the PATHS to elisp files by load order.

This ensures that each file is loaded after those it depends on."
  (unless (= (length (-uniq (mapcar (-compose #'f-no-ext #'f-filename) paths)))
             (length paths))
    (error "The filename components are not unique: sorting is not possible"))
  (let* ((orig-paths (map-into (mapcar (lambda (path)
                                         (cons (f-no-ext (f-filename path)) path))
                                       paths)
                               (list 'hash-table :test #'equal :size (length paths))))
         (dependencies (mapcar (lambda (path)
                                 (->> path
                                      rem-elisp-dependencies
                                      (mapcar (lambda (dependency)
                                                (gethash (symbol-name dependency) orig-paths)))
                                      (-filter #'identity)
                                      (cons path)))
                               paths)))
    (rem-topological-sort paths dependencies)))

;;; Files
(defun rem-ancestor-of-inclusive-p (path path2)
  (or (f-same-p path path2)
      (f-ancestor-of-p path path2)))

(defun rem-no-ext (path)
  "Remove all extensions from PATH. This is different from
`f-no-ext' which only removes the last one."
  (let (old-path)
    (while (not (equal path old-path))
      (setq old-path path
            path (f-no-ext path)))
    path))

(defun rem-dir-locals-file-names ()
  (save-match-data
    (let (files)
      ;; Copied from files.el.
      (when (string-match "\\.el\\'" dir-locals-file)
        (push (replace-match "-2.el" t nil dir-locals-file) files))
      ;; Ensure that `dir-locals-file' is first in the list.
      (push dir-locals-file files)
      files)))

(defun rem-slash (path)
  "Unconditionally add a slash to PATH.

This is different from `f-slash' which only adds a slash if PATH
points to a directory."
  (file-name-as-directory path))

(defun rem-no-slash (path)
  "Ensure that PATH does not end with a slash."
  (s-chop-suffix "/" path))

(defun rem-relative-path (path dir)
  (rem-slash (s-chop-prefix (rem-slash (f-canonical dir))
                            (f-canonical path))))

;;; Functions
(defmacro rem-fn (&rest body)
  (declare (indent 0))
  `(llama progn ,@body))

(cl-eval-when (compile load eval)
  (defvar rem-fn-vars '(%1 %2 %3 %4 %5 %6 %7 %8 %9 %10)))

(defmacro rem-define-fn (n)
  (cl-assert (>= n 2))
  `(defmacro ,(intern (format "rem-fn%d" n)) (&rest body)
     (declare (indent 0))
     `(rem-fn (progn ,@',(-take n rem-fn-vars)) ,@body)))

(defmacro rem-fn1 (&rest body)
  (declare (indent 0))
  `(rem-fn (let ((% %1))
             ,@body)))

(defmacro rem-define-fns ()
  `(progn ,@(mapcar (lambda (k) `(rem-define-fn ,k)) (-iota 9 2))))

(rem-define-fns)

(defun rem-maybe-args (&rest args)
  (unless (cl-evenp (length args))
    (error "An even number of arguments is required"))
  (let (args2 include-x-p x)
    (while args
      (setq x (car args)
            include-x-p (cadr args)
            args (cddr args))
      (when include-x-p
        (push x args2)))
    (reverse args2)))

(defmacro rem-maybe-kwd-args (&rest vars)
  (unless (cl-evenp (length vars))
    (error "An even number of variables is required"))
  `(append ,@(mapcar (lambda (pair)
                       (cl-destructuring-bind (x include-x-p)
                           pair
                         `(when ,include-x-p
                            (list ,(intern (format ":%s" (symbol-name x))) ,x))))
                     (-partition 2 vars))))

;;; Local variables
(defun rem-ensure-prop-line ()
  "Ensure that there is a prop line and move point into it."
  ;; `delete-file-local-variable-prop-line' requires an interned symbol so we
  ;; intern a gensym and then unintern it later.
  (let ((var (intern (symbol-name (gensym)))))
    (unwind-protect
        (progn
          (add-file-local-variable-prop-line var 'val)
          (delete-file-local-variable-prop-line var))
      (unintern var nil))))

;;; Quotes
(defun rem-quoted-p (form)
  "Determine if form is a quoted form."
  (eq (car-safe form) 'quote))

(defun rem-sharp-quoted-p (form)
  "Determine if form is a quoted form."
  (eq (car-safe form) 'function))

;;; Trees
(defun rem-subtree-map (fun tree)
  "Apply FUN to each subtree of TREE."
  (cl-labels ((rec (subtree)
                (funcall fun subtree)
                (when (consp subtree)
                  (rec (car subtree))
                  (rec (cdr subtree)))))
    (rec tree)))

(defun rem-tree-find-if (pred tree)
  "Find all subtrees in TREE satisfying PRED."
  (let (matches)
    (rem-subtree-map (lambda (subtree)
                       (when (funcall pred subtree)
                         (push subtree matches)))
                     tree)
    (reverse matches)))

;;; Movement and positions
(defun rem-goto-column (column)
  (beginning-of-line)
  ;; Columns are zero-indexed by `current-column'.
  (forward-char column))

(defun rem-goto-line (line)
  (goto-char (point-min))
  ;; Lines are one-indexed by `line-number-at-pos'.
  (forward-line (1- line)))

(defun rem-goto-line-column (line &optional column no-error)
  "Go to LINE and COLUMN in the current buffer.

If NO-ERROR is non-nil, go to the closest line and column instead
of signaling an error. Return a list containing \\='line if
moving to LINE was successful and \\'column if moving to COLUMN
as successful."
  (setq column (or column 0))
  (let (success)
    (if (<= line (count-lines (point-min) (point-max)))
        (progn
          (rem-goto-line line)
          (push 'line success))
      (if no-error
          (goto-char (point-max))
        (error "Line number %d does not exist" line)))
    (if (< column (- (line-end-position) (line-beginning-position)))
        (progn
          (rem-goto-column column)
          (push 'column success))
      (if no-error
          (end-of-line)
        (error "Column number %d does not exist on line %d" column (line-number-at-pos))))
    success))

(defun rem-window-line-number-at-pos (&optional pt)
  "Return the index of the line at PT in the selected window.

Indexing starts with 1 with the first line in the window. If PT
is nil, use the current point's position."
  (setq pt (or pt (point)))
  (let ((start (save-excursion
                 (move-to-window-line 0)
                 (beginning-of-visual-line)
                 (point)))
        (end (save-excursion
               (goto-char pt)
               (end-of-visual-line)
               (point))))
    ;; When we are on the first line and it is empty, start and end will be the
    ;; same. In this case, `count-screen-lines' would return 0 since the number
    ;; of lines between start and end is 0.
    (if (= start end)
        1
      (count-screen-lines start end))))

(cl-defun rem-move-current-window-line-to-pos (offset &optional (recenter t))
  "Move the current line to index OFFSET in the selected window.

Indexing is from 1. (`move-to-window-line' OFFSET) will have no
effect after running this function."
  (unless (<= 0 offset (truncate (window-screen-lines)))
    (error "Invalid index offset: %d" offset))
  ;; Convert to 0-indexing.
  (setq offset (1- offset))
  (let ((column (current-column)))
    (line-move-visual (- offset))
    (when recenter
      (recenter 0))
    (line-move-visual offset)
    (rem-goto-column column)))

;;; Default directories
(defmacro rem-with-directory (dir &rest body)
  "Execute BODY with `default-directory' temporarily changed to DIR.

This macro is useful for executing commands in a specific
directory."
  (declare (indent 1))
  (with-gensyms (buf cwd new-dir)
    `(let ((,buf (current-buffer))
           (,cwd default-directory)
           (,new-dir ,dir))
       (unwind-protect
           (progn
             (cd ,new-dir)
             ,@body)
         (with-current-buffer ,buf
           (cd ,cwd))))))

(def-edebug-spec rem-with-directory t)

;;; Whitespace
(defvar rem-newlines "\n\r"
  "String of newline characters.")

(defvar rem-newline-char-regexp (concat "[" rem-newlines "]")
  "Regexp for matching newline characters.")

;; This could use [[:blank:]] instead which would be better in theory. However,
;; then we would not be able to compute `rem-whitespace' from `rem-newlines' and
;; `rem-spaces'. Unfortunately, there isn't a character class for newlines or
;; all whitespace (including newlines) so there doesn't seem to be a good way
;; around this.
(defvar rem-spaces " \t"
  "String of space and tab characters.")

(defvar rem-space-char-regexp (concat "[" rem-spaces "]")
  "Regexp for matching space and tab characters.")

(defvar rem-whitespace (concat rem-newlines rem-spaces)
  "String of whitespace characters.")

(defvar rem-whitespace-char-regexp (concat "[" rem-whitespace "]")
  "Regexp for matching whitespace characters.")

(defvar rem-non-whitespace-char-regexp (concat "[^" rem-whitespace "]")
  "Regexp for matching non-whitespace characters.")

(defvar rem-positive-whitespace-regexp (concat "[" rem-whitespace "]+")
  "Regexp for matching one or more whitespace characters.")

(defvar rem-trailing-whitespace-regexp (concat "[" rem-whitespace "]*$")
  "Regexp for matching trailing whitespace at the end of a string.")

;;; Strings
(defun rem-copy-string (string)
  "Make a copy of the contents of STRING.

Text properties are ignored."
  (and string
       (stringp string)
       (let ((string2 (concat string)))
         string2)))

(defun rem-split-first (separator string)
  "Split string at the first occurrence of separator."
  (if-let ((start (cl-search separator string)))
      (list (substring string 0 start)
            (substring string (+ start (length separator))))
    string))

(defun rem-join-and (strings)
  (cond
   ((null strings)
    "")
   ((= (length strings) 1)
    (car strings))
   (t
    (concat (s-join ", " (butlast strings))
            " and "
            (car (last strings))))))

(defun rem-empty-nil (string &optional fun)
  "If STRING is nil or empty, return nil.

Otherwise, return STRING. If FUN is non-nil, apply it to string
before returning it. This function does not change the global
state (including the match data)."
  (save-match-data
    (unless (or (null string) (string= string ""))
      (if fun
          (funcall fun string)
        string))))

;;; Looking back
(defun rem-looking-back-p (regexp &optional limit greedy)
  "Check if text before point matches REGEXP.

Match data is saved. LIMIT and GREEDY have the same meaning as in
`looking-back'."
  (save-match-data
    (looking-back regexp limit greedy)))

;;; Text properties
(defun rem-strip-text-properties (string)
  "Create a copy of STRING with text properties removed."
  (setq string (rem-copy-string string))
  (set-text-properties 0 (length string) nil string)
  string)

;;; Thing at point
(defun rem-plain-thing-at-point (thing)
  "Return the thing at point without text properties."
  (awhen (thing-at-point thing)
    (rem-strip-text-properties it)))

(defun rem-plain-symbol-at-point ()
  "Return the symbol at point without text properties."
  (rem-plain-thing-at-point 'symbol))

;;; Binding keys
(defun rem-maybe-kbd (key)
               "If KEY is a string, apply `kbd' to it."
               (if (stringp key)
                   (kbd key)
                 key))

(defun rem-map-bindings (fun keymaps bindings)
  "Apply FUN to setup BINDINGS in each of the KEYMAPS."
  (when (keymapp keymaps)
    (setq keymaps (list keymaps)))
  (cl-dolist (keymap keymaps)
    (cl-dolist (binding bindings)
      (funcall fun keymap (rem-maybe-kbd (car binding)) (cdr binding)))))

(defun rem-define-keys (keymaps bindings)
  "Define BINDINGS for each of the KEYMAPS."
  (rem-map-bindings 'define-key keymaps bindings))

(defun rem-binding-list-to-alist (bindings)
  (mapcar (lambda (bind)
            (cons (car bind) (cadr bind)))
          (-partition 2 bindings)))

(defun rem-set-keys (keymaps &rest bindings)
  "This is the same as `rem-define-keys' but has the syntax of
`define-keymap'."
  (unless (cl-evenp (length bindings))
    (error "The length of bindings is odd"))
  (rem-define-keys keymaps (rem-binding-list-to-alist bindings)))

(defun rem-global-define-keys (bindings)
  "Globally define BINDINGS."
  (rem-define-keys (current-global-map) bindings))

(defun rem-global-set-keys (&rest bindings)
  "Globally define BINDINGS using the syntax of `define-keymap'."
  (apply #'rem-set-keys (current-global-map) bindings))

;;; Archives
(defun rem-extract-archive (path &optional subdir)
  "Extract the archive located at PATH to its containing directory.

When SUBDIR is non-nil, files are always extracted to a directory
even if the archive is a single compressed file."
  (let ((dir (f-dirname path))
        (archive (f-filename path)))
    (rem-with-directory dir
      (unless (= (call-process-shell-command (format "atool%s -x '%s'"
                                                     (if subdir
                                                         " -D"
                                                       "")
                                                     archive))
                 0)
        (error "Failed to extract %s" path)))))

;;; Lexicographic comparison
;; Compare using the shortlex ordering. See
;; https://en.wikipedia.org/wiki/Shortlex_order.
(defun rem-shortlex< (lex< s s2)
  "Return non-nil if S is shortlex-less than S2 using LEX<."
  (or (< (length s) (length s2))
      (and (= (length s) (length s2))
           (funcall lex< s s2))))

(defun rem-shortlex-string< (s s2)
  "Return non-nil if string S is shortlex-less than string S2."
  (rem-shortlex< 'string< s s2))

;;; Completion framework compatibility
(defvar rem-comp-preferred-framework nil
  "The preferred completion framework to use.")

(defun rem-comp-framework ()
  "Determine which completion framework should be used."
  (or rem-comp-preferred-framework
      (cond
       ((bound-and-true-p vertico-mode) 'vertico)
       ((bound-and-true-p selectrum-mode) 'selectrum)
       ((bound-and-true-p ivy-mode) 'ivy)
       ((bound-and-true-p helm-mode) 'helm)
       ((bound-and-true-p ido-mode) 'ido)
       ((bound-and-true-p fido-mode) 'fido)
       (t 'default))))

(defun rem-collection-with-metadata (collection new-metadata &optional override)
  (lambda (string predicate action)
    (if (eq action 'metadata)
        (progn
          (when (functionp new-metadata)
            (setq new-metadata (funcall new-metadata collection)))
          (if override
              `(metadata ,@new-metadata)
            (let ((current-metadata (cdr (completion-metadata (minibuffer-contents)
                                                              collection
                                                              minibuffer-completion-predicate))))
              `(metadata
                ,@(map-merge 'alist current-metadata new-metadata)))))
      (complete-with-action action collection string predicate))))

(defun rem-collection-with-sort-fun (collection sort-fun &optional override)
  (rem-collection-with-metadata collection
                                `((display-sort-function . ,sort-fun)
                                  (cycle-sort-function . ,sort-fun))
                                override))

(cl-defun rem-comp-read (prompt collection &key predicate require-match initial-input history default sort-fun metadata override-metadata keymap multiple)
  "Perform completing read with keyword arguments and some extensions.

Most of the arguments are the same as for `completing-read' but
are keyword arguments instead. HISTORY must be a symbol.
INITIAL-INPUT should not be used as it is better to use DEFAULT
so that the user can pull in the default value with M-n if they
wish. See `completing-read' for more details on why INITIAL-INPUT
is considered obsolete.

INHERIT-INPUT-METHOD is not supported because several frameworks
do not support it. If SORT-FUN is non-nil, it will be used to
sort collection before completion is performed. It should not
modify collection. If METADATA is non-nil, it should be an alist
that is merged with the completion metadata. It can also be a
function that is called with the completion table as an argument
to produce the metadata to merge. If OVERRIDE-METADATA is
non-nil, then METADATA will replace the completion metadata
instead of being merged with it. If KEYMAP is non-nil, it will be
used to create temporary key bindings that will be available
during this call to `completing-read'. (throw \\='rem-comp-read
return-value) can be used to create commands that can return from
the `completing-read' call. If MULTIPLE is non-nil, multiple
candidates will be allowed using the completion framework's
facilities for it or `completing-read-multiple'."
  ;; Some completion frameworks (e.g. helm) cannot handle the other options for
  ;; history that `completing-read' allows.
  (cl-assert (symbolp history))
  (when metadata
    (setq collection (rem-collection-with-metadata collection metadata override-metadata)))
  (defvar vertico-preselect)
  ;; Give user-defined functions in the keymap a way to short-circuit the
  ;; `completing-read' call.
  (catch 'rem-comp-read
    (cl-flet (;; Preselection has to be done differently for vertico. Other
              ;; completion frameworks already do this.
              (funcall-vertico (fun &rest args)
                (let ((vertico-preselect (if default 'first vertico-preselect)))
                  (apply fun args))))
      (let* ((framework (rem-comp-framework))
             (funcall-with-env (pcase framework
                                 ('vertico #'funcall-vertico)
                                 (_ #'funcall))))
        (pcase framework
          ('helm
           (declare-function helm-comp-read "helm-mode")
           (defvar helm-comp-read-map)
           ;; `helm-comp-read' already supports multiple candidates.
           (helm-comp-read prompt
                           collection
                           :predicate predicate
                           :must-match require-match
                           :initial-input initial-input
                           :history history
                           :default default
                           :preselect default
                           :keymap (make-composed-keymap (list keymap) helm-comp-read-map)
                           :fc-transformer sort-fun))
          ;; Disabled because it is broken. ivy does not seem to be using
          ;; `ivy-sort-functions-alist'.
          ;; ('ivy
          ;;  (declare-function ivy-read "ivy")
          ;;  (defvar ivy--all-candidates)
          ;;  (defvar ivy-sort-functions-alist)
          ;;  (defvar ivy-sort-matches-functions-alist)
          ;;  ;; Since `ivy-sort-functions-alist' expects a comparison function
          ;;  ;; rather than a sorting function, some tricks are required in
          ;;  ;; order to use sort-fun to determine the order.
          ;;  (let (indexes)
          ;;    (cl-flet ((our< (x y)
          ;;                (unless indexes
          ;;                  ;; Save the hash table that maps each candidate to
          ;;                  ;; its index as it is expensive to compute.
          ;;                  (setq indexes (make-hash-table))
          ;;                  (let ((k 0)
          ;;                        (sorted-candidates (funcall sort-fun ivy--all-candidates)))
          ;;                    (cl-dolist (candidate sorted-candidates)
          ;;                      (setf (gethash candidate indexes) k)
          ;;                      (cl-incf k))))
          ;;                (when (consp x)
          ;;                  (setq x (car x)))
          ;;                (when (consp y)
          ;;                  (setq y (car y)))
          ;;                (< (gethash x indexes) (gethash y indexes))))
          ;;      (let (;; This comparison function is used only once when the full
          ;;            ;; list of candidates is computed.
          ;;            (ivy-sort-functions-alist `((t . ,#'our<)))
          ;;            ;; This is used to sort the matching candidates whenever
          ;;            ;; the list of matching candidates changes. We disable any
          ;;            ;; additional sorting so that the initial order will be
          ;;            ;; maintained.
          ;;            (ivy-sort-matches-functions-alist '((t . nil))))
          ;;        (ivy-read prompt
          ;;                  collection
          ;;                  :predicate predicate
          ;;                  :require-match require-match
          ;;                  :initial-input initial-input
          ;;                  :history history
          ;;                  :def default
          ;;                  :preselect default
          ;;                  :keymap keymap)))))
          (_
           (cl-flet ((setup-keymap ()
                       (when keymap
                         (use-local-map (make-composed-keymap (list keymap) (current-local-map))))))
             ;; Put the hook at the very end so that any hooks added by the
             ;; completion framework will run first. This is important when they
             ;; setup the local map using `use-local-map'. This allows bindings
             ;; to be set in a generic way across different frameworks.
             (minibuffer-with-setup-hook (:append #'setup-keymap)
               (funcall funcall-with-env
                        (if multiple #'completing-read-multiple #'completing-read)
                        prompt
                        (if sort-fun
                            (rem-collection-with-sort-fun collection sort-fun)
                          collection)
                        predicate
                        require-match
                        initial-input
                        history
                        default)))))))))

(cl-defun rem-read-from-mini (prompt &key initial-contents keymap read history default inherit-input-method)
  "This is `read-from-minibuffer' with keyword arguments.

As in `read-from-minibuffer', INITIAL-CONTENTS should not be used
as it is better to use DEFAULT so that the user can pull in the
default value with M-n if they wish."
  (read-from-minibuffer prompt initial-contents keymap read history default inherit-input-method))

;;; Symbols
(defun rem-find-symbols (start &optional end)
  "Return all symbols starting with START and ending with END."
  (when (not end)
    (setq end ""))
  (let (syms name)
    (cl-do-all-symbols (sym syms)
      (setq name (symbol-name sym))
      (when (and (s-starts-with-p start name)
                 (s-ends-with-p end name))
        (push sym syms)))))

;;; Shell commands
(defvar rem-shell-file-name (executable-find "bash"))

(defvar rem-shell-command-switch "-c")

(defun rem-as-shell-command (command)
  "Convert COMMAND from a list to a shell command string.

If COMMAND is already a string then it is returned with no
changed."
  (if (stringp command)
      command
    (s-join " " (cons (car command) (mapcar #'shell-quote-argument (cdr command))))))

(defun rem--call-process-shell-command-no-rc (command &optional infile buffer display)
  (apply #'call-process
         (append (list rem-shell-file-name infile buffer display)
                 ;; This makes things much faster since loading the rc file can
                 ;; be slow.
                 (and (string= (f-filename rem-shell-file-name) "bash")
                      (list "--norc" "--noprofile"))
                 (list rem-shell-command-switch command))))

(defun rem--process-file-shell-command-no-rc (command &optional infile buffer display)
  (with-connection-local-variables
   (apply #'process-file
          (append (list rem-shell-file-name infile buffer display)
                  ;; This makes things much faster since loading the rc file can
                  ;; be slow.
                  (and (string= (f-filename rem-shell-file-name) "bash")
                       (list "--norc" "--noprofile"))
                  (list rem-shell-command-switch command)))))

;; Here command is a list consisting of a command and its arguments.
(defun rem--call-process (command &optional infile destination display)
  (apply #'call-process (executable-find (car command)) infile destination display (cdr command)))

(defun rem--process-file (command &optional infile destination display)
  (apply #'process-file (executable-find (car command)) infile destination display (cdr command)))

(cl-defun rem-run-command (command &key allow-remote (trim-output t) (validate t validate-supplied-p) error (return 'output return-supplied-p) nostderr buffer)
  "Execute COMMAND and return its exit code and output as a list.

This function is a combination of `call-process-shell-command',
`process-file-shell-command', `call-process' and `process-file'
depending on the keyword arguments passed. It is more versatile,
supports validation and can return the exit code as well as the
output.

When COMMAND is a string, it is passed to `rem-shell-file-name'
and the behavior is similar to `call-process-shell-command'. When
it is a list, the head is treated as the command and the tail as
the arguments to pass to it. If the head of COMMAND is not an
absolute path, it will be resolved by calling `executable-find'.

By default COMMAND is run on the localhost as with
`call-process-shell-command'. When ALLOW-REMOTE is non-nil,
command will be run on a remote host if `default-directory'
points to one (as with `process-file-shell-command').

When TRIM-OUTPUT is non-nil, whitespace will be removed from the
ends of the output string before it is returned. When
PROCESS-OUTPUT is non-nil, it should be a function to apply to
the output. This occurs after trimming (if applicable).

When VALIDATE is non-nil, it describes how the exit code and
output should be validated. When VALIDATE is a natural number or
a string, validation succeeds when the exit code is equal to
validate. When it is a function, it succeeds when the function
applied to the exit code and the output is non-nil. When VALIDATE
is any other non-nil value, validation succeeds if the exit code
is non-zero. When VALIDATE is nil, validation always succeeds.

When validation fails, the return value is determined by ERROR.
If it is a function, the function is applied to the exit code and
output and the result is returned. ERROR is allowed to signal an
error. When ERROR is nil, nil is returned. Otherwise, when ERROR
is non-nil, an error is signaled instead of returning nil.

When validation succeeds, the return value is determined by
RETURN. If RETURN is \\='both, validation is skipped and a list
of the form (EXIT-CODE OUTPUT) is returned. EXIT-CODE can be
either a numeric exit code or a string describing a signal. When
RETURN is \\='output, the output from COMMAND is returned. When
it is \\='exit-code, the exit-code is returned. Validation is
skipped in this case unless VALIDATE was passed explicitly. When
it is a function, it is applied to the exit-code and output and
the result is returned. When RETURN is any other value, that
value is returned.

By default, stdout and stderr are mixed in the output. When
NOSTDERR is non-nil, stderr is discarded instead.

When BUFFER is non-nil, it indicates the buffer that the output
should be sent to. In this case, the default value of RETURN will
be ignored and it will be treated as if it were \\='exit-code
unless RETURN was passed explicitly."
  (when (and buffer (not return-supplied-p))
    (setq return 'exit-code))
  (when (and (eq return 'exit-code) (not validate-supplied-p))
    (setq validate nil))
  (with-temp-buffer
    (let* ((shell (stringp command))
           ;; It would be easier to just use `rem-as-shell-command' and always
           ;; use a shell but that would create extra processes.
           (run (cond
                 ((and (not shell) (not allow-remote))
                  #'rem--call-process)
                 ((and (not shell) allow-remote)
                  #'rem--process-file)
                 ((and shell (not allow-remote))
                  #'rem--call-process-shell-command-no-rc)
                 ((and shell allow-remote)
                  #'rem--process-file-shell-command-no-rc)))
           (dest (if nostderr
                     ;; The second element is the file to send stderr to.
                     (list (or buffer (current-buffer)) "/dev/null")
                   (or buffer (current-buffer))))
           (exit-code (funcall run command nil dest))
           (raw-output (buffer-substring-no-properties 1 (point-max)))
           (output (if trim-output (s-trim raw-output) raw-output))
           (success (or (eq return 'both)
                        (cond
                         ((null validate)
                          t)
                         ((or (natnump validate) (stringp validate))
                          (equal exit-code validate))
                         ((functionp validate)
                          (funcall validate exit-code output))
                         (t
                          (eql exit-code 0))))))
      (if success
          (cond
           ((eq return 'both)
            (list exit-code output))
           ((eq return 'exit-code)
            exit-code)
           ((eq return 'output)
            output)
           ((functionp return)
            (funcall return exit-code output))
           (t
            return))
        (cond
         ((functionp error)
          (funcall error exit-code output))
         (error
          (error "The command \"%s\" in \"%s\" failed with exit code %s and output \"%s\""
                 (rem-as-shell-command command)
                 default-directory
                 (if (stringp exit-code)
                     (format "\"%s\"" exit-code)
                   exit-code)
                 output)))))))

(defun rem-call-process-shell-command (command)
  (rem-run-command command :validate nil :return 'both))

(defun rem-process-file-shell-command (command)
  (rem-run-command command :allow-remote t :validate nil :return 'both))

;;; Email
(defvar rem-email-address-regexp "[a-zA-Z0-9_!#$%&'*+-/=?]+@[a-zA-Z0-9.-]+\\.[a-zA-Z0-9.-]+")

;;; Paths
(defun rem-path-length (path)
  (setq path (f-canonical path))
  (length (f-split path)))

;;; URLs
;; Based on the erc-button-url-regexp variable.
(defvar rem-strict-url-regexp "\\(www\\.\\|\\(s?https?\\|ftp\\|file\\|gopher\\|news\\|telnet\\|wais\\|mailto\\):\\)\\(//[-a-zA-Z0-9_.]+:[0-9]*\\)?[-a-zA-Z0-9_=!?#$@~`%&*+\\/:;.,()]+[-a-zA-Z0-9_=#$@~`%&*+\\/()]"
  "This regexp matches only things that are clearly URLs.

It does not match ambiguous things such as abc.xyz.")

(defun rem-url-dirname (url)
  "Return the directory component of the URL."
  ;; Parse the URL if necessary or create a copy of it.
  (setq url (url-generic-parse-url (if (url-p url)
                                       (url-recreate-url url)
                                     url)))
  (setf (url-filename url) (f-dirname (url-filename url)))
  (file-name-as-directory (url-recreate-url url)))

(provide 'rem)
;;; rem.el ends here
