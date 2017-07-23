;;; js99-mode.el --- Retarded JavaScript editing mode                   -*- lexical-binding: t; -*-

;; Copyright (C) 2017  futpib

;; Author: futpib <futpib@gmail.com>
;; URL: https://github.com/futpib/js99-mode
;; Package-Requires: ((emacs "25") (promise "1.0"))
;; Version: 1.0
;; Keywords: languages es es6 es7 es2015 es2016 es2017 javascript js jsx

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; TODO

;;; Code:

(require 'cl)
(require 'promise)
(require 'nodejs-slave)


(defvar js99-debug-overlays nil
  "Show overlay boundaries.")
(defvar js99-debug-pause-updates nil
  "Stop updates.")


(defconst js99-ast-subtyping-hash (make-hash-table))

(defun js99-define-ast-type (type &optional parents)
  "Define ESTree AST node TYPE that inherits from PARENTS."
  (puthash type parents js99-ast-subtyping-hash))

(defun js99-ast-parent-types (type)
  "Get (immediate) parent-types of the TYPE."
  (gethash type js99-ast-subtyping-hash))

(defun js99-ast-ancestor-types (type)
  "Get (recursive) ancestor-types of the TYPE."
  (let ((parent-types (js99-ast-parent-types type)))
    (delete-dups (append parent-types (mapcan #'js99-ast-ancestor-types parent-types)))))

(defun js99-ast-subtypeof (subtype supertype)
  "Check if SUBTYPE is subtype of SUPERTYPE."
  (let ((supertypes (js99-ast-ancestor-types subtype)))
    (or (eq subtype supertype)
        (member supertype supertypes)
        (cl-some (lambda (p) (js99-ast-subtypeof p supertype)) supertypes))))


(defconst js99-ast-face-hash (make-hash-table))

(defun js99-define-ast-face-new (type-sym facespec doc rest)
  (let ((name (intern (format "js99-estree-%s" (symbol-name type-sym))))
        (doc (or doc (format "Face for %s ESTree nodes." (symbol-name type-sym)))))
    (puthash type-sym name js99-ast-face-hash)
    `(defface ,name ,facespec ,doc
       :group 'js99-mode
       ,@rest)))

(defun js99-define-ast-face-existing (type-sym name)
  (puthash type-sym name js99-ast-face-hash))

(defmacro js99-define-ast-face (type facespec &optional doc &rest rest)
  "Define faceing for AST nodes that are subtype of TYPE to be the FACESPEC."
  (let ((type-sym (second type))
        (face-sym (second facespec)))
    (if (symbolp face-sym)
        (js99-define-ast-face-existing type-sym face-sym)
      (js99-define-ast-face-new type-sym facespec doc rest))))

(defun js99-face-for-node-type (type)
  "Choose a face for the node TYPE."
  (seq-some (lambda (x) (gethash x js99-ast-face-hash))
            (cons type (js99-ast-ancestor-types type))))


(js99-define-ast-face 'Comment
  '((t :inherit font-lock-comment-face)))

(js99-define-ast-face 'ThisExpression
  '((((background light)) :foreground "DarkSlateBlue" :weight normal)
    (((background dark))  :foreground "LightStaleBlue" :weight normal)))
(js99-define-ast-face 'Super
  '((((background light)) :foreground "DarkSlateBlue" :weight normal)
    (((background dark)) :foreground "LightStaleBlue" :weight normal)))

(js99-define-ast-face 'Declaration
  '((((background light)) :foreground "Purple" :weight bold)
    (((background dark))  :foreground "Cyan1" :weight bold)))
(js99-define-ast-face 'Statement
  '((((background light)) :foreground "Purple" :weight bold)
    (((background dark))  :foreground "Cyan1" :weight bold)))

(js99-define-ast-face 'SyntaxError
  '((t :background "red")))

(js99-define-ast-face 'Unparsed
  '((t :background "green")))

(js99-define-ast-face 'Literal
  '((t :foreground "DarkOrange"
       :weight normal)))

(js99-define-ast-face 'ModuleDeclaration
  '((t :foreground "SeaGreen3"
       :weight normal)))

(js99-define-ast-face 'Expression
  '((t :inherit default
       :weight normal)))

(js99-define-ast-face 'Type
  '((t :foreground "CornflowerBlue")
    (t :weight normal)))

(js99-define-ast-face 'Identifier
  '((t :inherit default
       :weight normal)))


;; cd ~/code/estree
;; find -iname '*.md' | grep -v deprecated | ack -x '^interface (\w+)(?:\s*?<:\s*?)?(\w+)?(?:\s*?,\s*?)?(\w+)?\s*{' --output '(js99-define-ast-type `$1 `($2 $3))' --no-filename | perl -pe 's/ \)/\)/g' | perl -pe 's/ `\(\)//g' | sort -u
(js99-define-ast-type `ArrayExpression `(Expression))
(js99-define-ast-type `ArrayPattern `(Pattern))
(js99-define-ast-type `ArrowFunctionExpression `(Function Expression))
(js99-define-ast-type `AssignmentExpression `(Expression))
(js99-define-ast-type `AssignmentPattern `(Pattern))
(js99-define-ast-type `AssignmentProperty `(Property))
(js99-define-ast-type `AwaitExpression `(Expression))
(js99-define-ast-type `BinaryExpression `(Expression))
(js99-define-ast-type `BlockStatement `(Statement))
(js99-define-ast-type `BreakStatement `(Statement))
(js99-define-ast-type `CallExpression `(Expression))
(js99-define-ast-type `CatchClause `(Node))
(js99-define-ast-type `ClassBody `(Node))
(js99-define-ast-type `ClassDeclaration `(Class Declaration))
(js99-define-ast-type `ClassExpression `(Class Expression))
(js99-define-ast-type `Class `(Node))
(js99-define-ast-type `ConditionalExpression `(Expression))
(js99-define-ast-type `ContinueStatement `(Statement))
(js99-define-ast-type `DebuggerStatement `(Statement))
(js99-define-ast-type `Declaration `(Statement))
(js99-define-ast-type `Decorator `(Node))
(js99-define-ast-type `DoWhileStatement `(Statement))
(js99-define-ast-type `EmptyStatement `(Statement))
(js99-define-ast-type `ExportAllDeclaration `(ModuleDeclaration))
(js99-define-ast-type `ExportDefaultDeclaration `(ModuleDeclaration))
(js99-define-ast-type `ExportNamedDeclaration `(ModuleDeclaration))
(js99-define-ast-type `ExportSpecifier `(ModuleSpecifier))
(js99-define-ast-type `Expression `(Node))
(js99-define-ast-type `ExpressionStatement `(Statement))
(js99-define-ast-type `ForInStatement `(Statement))
(js99-define-ast-type `ForOfStatement `(ForInStatement))
(js99-define-ast-type `ForStatement `(Statement))
(js99-define-ast-type `FunctionDeclaration `(Function Declaration))
(js99-define-ast-type `FunctionExpression `(Function Expression))
(js99-define-ast-type `Function `(Node))
(js99-define-ast-type `Identifier `(Expression Pattern))
(js99-define-ast-type `JSXIdentifier `(Identifier))
(js99-define-ast-type `IfStatement `(Statement))
(js99-define-ast-type `ImportDeclaration `(ModuleDeclaration))
(js99-define-ast-type `ImportDefaultSpecifier `(ModuleSpecifier))
(js99-define-ast-type `ImportNamespaceSpecifier `(ModuleSpecifier))
(js99-define-ast-type `Import `(Node))
(js99-define-ast-type `ImportSpecifier `(ModuleSpecifier))
(js99-define-ast-type `LabeledStatement `(Statement))
(js99-define-ast-type `Literal `(Expression))
(js99-define-ast-type `LogicalExpression `(Expression))
(js99-define-ast-type `MemberExpression `(Expression Pattern))
(js99-define-ast-type `MetaProperty `(Expression))
(js99-define-ast-type `MethodDefinition `(Node))
(js99-define-ast-type `ModuleDeclaration `(Node))
(js99-define-ast-type `ModuleSpecifier `(Node))
(js99-define-ast-type `NewExpression `(Expression))
(js99-define-ast-type `Node)
(js99-define-ast-type `ObjectExpression `(Expression))
(js99-define-ast-type `ObjectPattern `(Pattern))
(js99-define-ast-type `Pattern `(Node))
(js99-define-ast-type `Position)
(js99-define-ast-type `Program `(Node))
(js99-define-ast-type `Property `(Node))
(js99-define-ast-type `RegExpLiteral `(Literal))
(js99-define-ast-type `RestElement `(Pattern))
(js99-define-ast-type `ReturnStatement `(Statement))
(js99-define-ast-type `SequenceExpression `(Expression))
(js99-define-ast-type `SourceLocation)
(js99-define-ast-type `SpreadElement `(Node))
(js99-define-ast-type `Statement `(Node))
(js99-define-ast-type `Super `(Node))
(js99-define-ast-type `SwitchCase `(Node))
(js99-define-ast-type `SwitchStatement `(Statement))
(js99-define-ast-type `TaggedTemplateExpression `(Expression))
(js99-define-ast-type `TemplateElement `(Literal))
(js99-define-ast-type `TemplateLiteral `(Expression))
(js99-define-ast-type `ThisExpression `(Expression))
(js99-define-ast-type `ThrowStatement `(Statement))
(js99-define-ast-type `TryStatement `(Statement))
(js99-define-ast-type `TypeAnnotation `(Node))
(js99-define-ast-type `UnaryExpression `(Expression))
(js99-define-ast-type `UpdateExpression `(Expression))
(js99-define-ast-type `VariableDeclaration `(Declaration))
(js99-define-ast-type `VariableDeclarator `(Node))
(js99-define-ast-type `WhileStatement `(Statement))
(js99-define-ast-type `WithStatement `(Statement))
(js99-define-ast-type `YieldExpression `(Expression))

(js99-define-ast-type `Unparsed `(Node))
(js99-define-ast-type `SyntaxError `(Node))

(js99-define-ast-type `Comment `(Node))
(js99-define-ast-type `CommentLine `(Comment))

(js99-define-ast-type `Type `(Node))
(js99-define-ast-type `TypeAnnotation `(Type Node))
(js99-define-ast-type `TypeDeclaration `(Type Declaration))
(js99-define-ast-type `InterfaceDeclaration `(Type Declaration))
(js99-define-ast-type `TypeAlias `(TypeDeclaration))


(defvar js99-overlays nil)

(defun js99-delete-overlays ()
  "Delete all js99 overlays."
  (dolist (overlay js99-overlays)
    (delete-overlay overlay))
  (setq js99-overlays nil))

(defun js99-make-overlay (type depth parent-overlay start end buffer)
  "Make overlay for AST node with type TYPE at depth DEPTH at START and END."
  (let ((face (js99-face-for-node-type type))
        (overlay (make-overlay start end buffer t t)))
    (when face
      (push overlay js99-overlays)

      (overlay-put overlay 'priority depth)
      (overlay-put overlay 'face face)
      (overlay-put overlay 'evaporate t)

      (overlay-put overlay 'js99-overlay t)
      (overlay-put overlay 'js99-ast-type type)
      (overlay-put overlay 'js99-parent-overlay parent-overlay)

      (when js99-debug-overlays
        (overlay-put overlay 'before-string (format "<%s depth=%s>" type depth))
        (overlay-put overlay 'after-string (format "</%s>" type)))

      overlay)))

(defun js99-is-overlay (overlay)
  "Check if OVERLAY was created by js99-mode."
  (when (overlay-get overlay 'js99-overlay) overlay))


(defgroup js99-mode nil
  "Retarded JavaScript editing mode."
  :group 'languages)


(defun js99-debounce (func wait)
  "Delay FUNC application by WAIT seconds since last call of the debounced FUNC."
  (let ((timer nil))
    (lambda (&rest args)
      (when timer (cancel-timer timer))
      (setq timer (run-at-time wait nil (lambda () (apply func args)))))))

(defmacro js99-debounced-defun (name arglist wait doc &rest body)
  "Define debounced function."
  `(let ((js99-debounced-defun--debounced (js99-debounce (lambda ,arglist ,@body) ,wait)))
     (defun ,name (&rest js99-debounced-defun--args)
       ,@(and doc (list doc))
       (apply js99-debounced-defun--debounced js99-debounced-defun--args))))

(defun js99-parse (source)
  (nodejs-slave-run
   '(lambda (source)
      (let ((js99 (require "nodejs-js99-mode")))
        (js99.parse source)))
   source))


(defun js99-overlay-buffer (ast buffer)
  "Create overlays based on AST in the BUFFER."
  (cl-labels ((highlight
               (node depth parent-overlay)
               (let ((type (alist-get 'type node))
                     (start-raw (alist-get 'start node))
                     (end-raw (alist-get 'end node)))
                 (when type
                   (let ((type (if (stringp type) (intern type) type)))
                     (when (and start-raw end-raw)
                       (let* ((zero (point-min))
                              (start (+ zero start-raw))
                              (end (+ zero end-raw)))
                         (setq
                          parent-overlay
                          (js99-make-overlay type depth parent-overlay start end buffer)))))))
               (highlight-children node (+ 1 depth) parent-overlay))

              (highlight-all
               (nodes depth parent-overlay)
               (seq-do (lambda (node)
                         (when (listp node)
                           (highlight node (+ 1 depth) parent-overlay)))
                       nodes))

              (highlight-children
               (node depth parent-overlay)
               (when (listp node)
                 (mapcar (lambda (cons)
                           (let ((val (cdr-safe cons)))
                             (when (and (listp val) (alist-get 'type val))
                               (highlight val (+ 1 depth) parent-overlay))
                             (when (arrayp val)
                               (highlight-all val (+ 1 depth) parent-overlay))))
                         node))))

    (js99-delete-overlays)
    (highlight-children ast 0 nil)))

(defun js99-overlay-buffer-error (err buffer)
  "Replace AST overlays with something from ERR in the BUFFER."
  (with-current-buffer buffer
    (let* ((zero (point-min))

           (pos (+ zero (alist-get 'pos err)))

           (overlay (cl-some #'js99-is-overlay (reverse (overlays-at pos t))))
           (start (overlay-start overlay))
           (end (overlay-end overlay))
           (priority (overlay-get overlay 'priority))
           (parent (overlay-get overlay 'js99-parent-overlay))

           (message (alist-get 'message err)))
      (delete-overlay overlay)
      (print `(js99-make-overlay 'SyntaxError ,priority ,parent ,start ,end ,buffer))
      (js99-make-overlay 'SyntaxError priority parent start end buffer))))


(defun pprint (form &optional output-stream)
  (princ (with-temp-buffer
           (cl-prettyprint form)
           (buffer-string))
         output-stream))

(js99-debounced-defun js99-onchange (&rest _ignored) 1
  "Handle buffer change by queueing parsing and overlays update."
  (when (not js99-debug-pause-updates)
    (let ((buffer (current-buffer))
          (buffer-state (buffer-chars-modified-tick)))
      (promise-done
       (promise-chain
           (js99-parse (buffer-string))

         (then
          (lambda (x)
            (let ((new-buffer-state (buffer-chars-modified-tick)))
              (when (eq new-buffer-state buffer-state)
                (js99-overlay-buffer x buffer) x))))

         (promise-catch
          (lambda (e)
            (let ((new-buffer-state (buffer-chars-modified-tick))
                  (error-name (alist-get 'name e)))
              (when (not (equal error-name "SyntaxError"))
                (error e))
              (when (eq new-buffer-state buffer-state)
                (js99-overlay-buffer-error e buffer)))))

         (promise-catch
          (lambda (e)
            (if (and (listp e) (alist-get 'code e))
                (print "Please run `npm install --global nodejs-js99-mode` before using js99-mode")
              (error e)))))))))

(define-derived-mode js99-mode fundamental-mode "js99"
  "Major mode for editing JavaScript code."

  (run-hooks 'js99-init-hook)
  (add-hook 'change-major-mode-hook #'js99-mode-exit nil t)

  (add-hook 'after-change-functions #'js99-onchange nil t)
  (add-hook 'js99-exit-hook
            (lambda ()
              (js99-delete-overlays)
              (remove-hook 'after-change-functions #'js99-onchange t))
            nil t)

  (js99-onchange)

  nil)

(defun js99-mode-exit ()
  (run-hooks 'js99-exit-hook))

(provide 'js99-mode)
;;; js99-mode.el ends here
