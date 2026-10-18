;;; bash-ts-mode.el --- Tree-sitter mode for Bash  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026 konomanoasa
;;
;; Author: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Maintainer: konomanoasa <238482287+konomanoasa@users.noreply.github.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "31.1"))
;; Keywords: languages
;; URL: https://github.com/konomanoasa/bash-ts-mode
;;
;; Permission is hereby granted, free of charge, to any person obtaining
;; a copy of this software and associated documentation files (the
;; "Software"), to deal in the Software without restriction, including
;; without limitation the rights to use, copy, modify, merge, publish,
;; distribute, sublicense, and/or sell copies of the Software, and to
;; permit persons to whom the Software is furnished to do so, subject to
;; the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
;; LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
;; OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
;; WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

;;; Commentary:
;;
;; Tree-sitter major mode for Bash.

;;; Code:

(require 'editorconfig)
(require 'elec-pair)
(require 'newcomment)
(require 'sh-script)
(require 'treesit)

;;; Built-in mode redirection

(advice-remove 'bash-ts-mode 'sh--redirect-bash-ts-mode)

(defgroup bash-ts nil
  "Tree-sitter mode for Bash."
  :group 'languages)

;;;; Grammar

(defconst bash-ts-mode--grammar-sources
  '((bash "https://github.com/konomanoasa/tree-sitter-bash"
          :revision "v0.4.0")
    (bash_extglob "https://github.com/konomanoasa/tree-sitter-bash"
                  :revision "v0.4.0"
                  :source-dir "bash_extglob/src"))
  "Tree-sitter grammar sources for Bash.")

(defun bash-ts-mode--ensure-grammar (language)
  "Ensure that the grammar for LANGUAGE is installed."
  (let ((treesit-language-source-alist
         (if (assq language treesit-language-source-alist)
             treesit-language-source-alist
           (cons (assq language bash-ts-mode--grammar-sources)
                 treesit-language-source-alist))))
    (or (treesit-ensure-installed language)
        (user-error "Tree-sitter grammar `%s' is unavailable" language))))

(defun bash-ts-mode--editorconfig-option (properties property default)
  "Return boolean PROPERTY from PROPERTIES, or DEFAULT when unspecified."
  (let ((value (and properties (gethash property properties))))
    (pcase value
      ('nil default)
      ("true" t)
      ("false" nil)
      (_ (user-error "Invalid EditorConfig %s: %S (expected true or false)"
                     property value)))))

(defun bash-ts-mode--language ()
  "Return the Tree-sitter language selected by EditorConfig."
  (let* ((properties (when buffer-file-name
                       (editorconfig-call-get-properties-function
                        buffer-file-name)))
         (extglob (bash-ts-mode--editorconfig-option
                   properties 'extglob nil)))
    (if extglob 'bash_extglob 'bash)))

;;;; Incremental Updates

(defconst bash-ts-mode-update--containers
  '("list")
  "Container nodes traversed to find an edited source unit.")

(defvar-local bash-ts-mode-update--state nil
  "Bounds and recovery state captured before a buffer edit.")

(defvar-local bash-ts-mode-update--pending-range nil
  "Edited owner range awaiting syntax propertization.")

(defun bash-ts-mode-update--bounds (beg end)
  "Return enclosing source-unit bounds around BEG and END."
  (let ((start beg)
        (finish end)
        (root (treesit-parser-root-node treesit-primary-parser)))
    (dolist (position (list (max (point-min) (1- beg))
                            (min (point-max) end)))
      (let ((node (treesit-node-at position treesit-primary-parser))
            owner)
        (while (and node (not (treesit-node-eq node root)))
          (unless (member (treesit-node-type node)
                          bash-ts-mode-update--containers)
            (setq owner node))
          (setq node (treesit-node-parent node)))
        (when owner
          (setq start (min start (treesit-node-start owner))
                finish (max finish (treesit-node-end owner))))))
    (cons start finish)))

(defun bash-ts-mode-update--before-change (beg end)
  "Save owner bounds and old-tree recovery state before changing BEG to END."
  (when treesit-primary-parser
    (save-restriction
      (widen)
      (setq bash-ts-mode-update--state
            (cons (bash-ts-mode-update--bounds beg end)
                  (treesit-node-check
                   (treesit-parser-root-node treesit-primary-parser)
                   'has-error))))))

(defun bash-ts-mode-update--reset-parser ()
  "Replace the primary parser without reusing its old tree."
  (let* ((parser treesit-primary-parser)
         (language (treesit-parser-language parser))
         (tag (treesit-parser-tag parser))
         (ranges (treesit-parser-included-ranges parser))
         (notifiers (treesit-parser-notifiers parser))
         (level (treesit-parser-embed-level parser)))
    (treesit-parser-delete parser)
    (setq treesit-primary-parser (treesit-parser-create language nil nil tag))
    (treesit-parser-set-included-ranges treesit-primary-parser ranges)
    (treesit-parser-set-embed-level treesit-primary-parser level)
    (dolist (notifier notifiers)
      (treesit-parser-add-notifier treesit-primary-parser notifier))))

(defun bash-ts-mode-update--after-change (beg end old-length)
  "Refresh edited owners and avoid stale trees after BEG, END and OLD-LENGTH."
  (when treesit-primary-parser
    (let ((narrowed (buffer-narrowed-p)))
      (save-restriction
        (widen)
        (let* ((state bash-ts-mode-update--state)
               (reset (or (cdr state)
                          (and (= beg 1) (> old-length 0)
                               (eq (char-after 1) #xfeff)))))
          (setq bash-ts-mode-update--state nil)
          (when reset (bash-ts-mode-update--reset-parser))
          (let* ((bounds (if reset (cons (point-min) (point-max))
                           (bash-ts-mode-update--bounds beg end)))
                 (start (min (car bounds) (or (caar state) beg)))
                 (finish (min (point-max)
                              (max (cdr bounds)
                                   (+ (or (cdar state) (+ beg old-length))
                                      (- end beg old-length)))))
                 (pending bash-ts-mode-update--pending-range)
                 (pending-end (cdr pending)))
            (when (and pending-end (>= pending-end beg))
              (setq pending-end (max end (+ pending-end (- end beg old-length)))))
            (setq bash-ts-mode-update--pending-range
                  (cons (min start (or (car pending) start))
                        (min (point-max) (max finish (or pending-end finish)))))
            (syntax-ppss-flush-cache start)
            (font-lock-flush start finish)
            (when narrowed (syntax-propertize finish))))))))

(defun bash-ts-mode-update--extend-region (start end)
  "Include edited owners when propertizing START to END."
  (when bash-ts-mode-update--pending-range
    (let ((bounds bash-ts-mode-update--pending-range))
      (setq bash-ts-mode-update--pending-range nil)
      (cons (max (point-min) (min start (car bounds)))
            (min (point-max) (max end (cdr bounds)))))))

(defun bash-ts-mode-update--setup ()
  "Configure incremental updates for the current buffer."
  (add-hook 'before-change-functions #'bash-ts-mode-update--before-change nil t)
  (add-hook 'after-change-functions #'bash-ts-mode-update--after-change nil t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'bash-ts-mode-update--extend-region nil t))

;;;; Context

(defconst bash-ts-mode--pattern-context-types
  '("pattern_list" "parameter_pattern" "conditional_pattern"
    "extglob_alternative" "regex_operand")
  "Node types that enter shell patterns or static EREs.")

(defconst bash-ts-mode--pattern-boundary-types
  '("command_substitution_body" "backquote_substitution_body" "list"
    "here_document_sequence")
  "Node types that start command lists outside a pattern scope.")

(defun bash-ts-mode--pathname-word-p (node)
  "Return non-nil when NODE owns source eligible for pathname expansion."
  (let ((parent (treesit-node-parent node)))
    (or (equal (treesit-node-type node) "brace_alternative")
        (and (equal (treesit-node-type node) "word")
             (pcase (treesit-node-type parent)
               ("simple_command"
                (member (treesit-node-field-name node) '("name" "argument")))
               ((or "for_clause" "select_clause")
                (equal (treesit-node-field-name node) "word"))
               ("io_file" (equal (treesit-node-field-name node) "filename"))
               ("array_assignment" t))))))

(defconst bash-ts-mode--pathname-pattern-regexp
  (rx string-start
      (or "pattern_bracket_source" "pattern_question_source"
          "pattern_star_source" "extglob_pattern")
      string-end)
  "Regexp matching pathname pattern candidates.")

(defconst bash-ts-mode--pattern-scope-types
  (append bash-ts-mode--pattern-context-types
          bash-ts-mode--pattern-boundary-types
          '("tilde_expansion"))
  "Node types whose nested patterns belong to another pattern scope.")

(defun bash-ts-mode--continued-lexical-token-p (node)
  "Return non-nil when NODE's parent has only continuation leaves."
  (let* ((parent (treesit-node-parent node))
         (index 0)
         (count (treesit-node-child-count parent)))
    (while (and (< index count)
                (equal (treesit-node-type (treesit-node-child parent index))
                       "line_continuation"))
      (setq index (1+ index)))
    (= index count)))

(defun bash-ts-mode--pathname-pattern-word-p (word)
  "Return non-nil when WORD contains a pathname pattern."
  (treesit-search-subtree
   word
   `(and ,bash-ts-mode--pathname-pattern-regexp
         ,(lambda (node)
            (let ((parent (treesit-node-parent node)))
              (while (and parent
                          (not (treesit-node-eq parent word))
                          (not (member (treesit-node-type parent)
                                       bash-ts-mode--pattern-scope-types)))
                (setq parent (treesit-node-parent parent)))
              (treesit-node-eq parent word))))))

(defun bash-ts-mode--syntax-structure-p (node)
  "Return non-nil when NODE's structure is independent of subscript candidates."
  (not (or (member (treesit-node-type node)
                   '("parenthesized_arithmetic_candidate"
                     "parenthesized_arithmetic_candidate_source"))
           (and (equal (treesit-node-type node) "array_subscript")
                (member (treesit-node-type (treesit-node-parent node))
                        '("arithmetic_candidate_array_reference"
                          "arithmetic_candidate_source_expression"
                          "parenthesized_arithmetic_candidate_source"))))))

(defun bash-ts-mode--syntax-delimiter-p (node)
  "Return non-nil when NODE delimits a confirmed syntax structure."
  (bash-ts-mode--syntax-structure-p (treesit-node-parent node)))

(defun bash-ts-mode--pattern-interior-p (node)
  "Return non-nil when NODE is inside a shell pattern or static ERE."
  (let (result)
    (while node
      (let ((parent (treesit-node-parent node))
            (type (treesit-node-type node)))
        (cond
         ((member type bash-ts-mode--pattern-context-types)
          (setq result t node nil))
         ((or (member type bash-ts-mode--pattern-boundary-types)
              (member type '("simple_command" "for_clause" "select_clause" "case_clause"
                             "io_file" "io_here")))
          (setq node nil))
         ((bash-ts-mode--pathname-word-p node)
          (setq result (bash-ts-mode--pathname-pattern-word-p node)
                node nil))
         (t (setq node parent)))))
    result))

(defun bash-ts-mode--outside-pattern-interior-p (node)
  "Return non-nil when NODE is outside shell patterns and static EREs."
  (not (bash-ts-mode--pattern-interior-p node)))

(defun bash-ts-mode--pattern-source-owner (node)
  "Return the owner outside the pattern source structure of NODE."
  (let ((parent (treesit-node-parent node)))
    (while (and parent
                (member (treesit-node-type parent)
                        '("pattern_bracket_source"
                          "pattern_bracket_negation_source"
                          "pattern_bracket_members_source"
                          "pattern_bracket_range_source"
                          "pattern_character_class_source"
                          "pattern_collating_symbol_source"
                          "pattern_equivalence_class_source")))
      (setq parent (treesit-node-parent parent)))
    parent))

(defun bash-ts-mode--tilde-pattern-source-p (node)
  "Return non-nil when NODE is pattern source owned by a tilde prefix."
  (let ((owner (bash-ts-mode--pattern-source-owner node)))
    (and owner (equal (treesit-node-type owner) "tilde_user"))))

(defun bash-ts-mode--plain-pattern-source-p (node)
  "Return non-nil when NODE is pattern source treated as plain text."
  (let* ((owner (bash-ts-mode--pattern-source-owner node))
         (type (and owner (treesit-node-type owner))))
    (and (not (equal type "tilde_user"))
         (not (member type bash-ts-mode--pattern-context-types))
         (not (bash-ts-mode--pathname-word-p owner))
         (bash-ts-mode--outside-pattern-interior-p owner))))

(defun bash-ts-mode--shell-pattern-source-p (node)
  "Return non-nil when NODE is pattern source outside a tilde prefix."
  (and (bash-ts-mode--pattern-interior-p node)
       (not (bash-ts-mode--tilde-pattern-source-p node))))

(defun bash-ts-mode--string-literal-p (node)
  "Return non-nil when NODE has a string literal owner."
  (let* ((parent (treesit-node-parent node))
         (owner (and parent (treesit-node-parent parent))))
    (and (not (and parent
                   (equal (treesit-node-type parent) "tilde_user")))
         (not (and parent owner
                   (equal (treesit-node-type parent) "word")
                   (or (and (equal (treesit-node-type owner) "simple_command")
                            (equal (treesit-node-field-name parent) "name"))
                       (and (equal (treesit-node-type owner) "coproc")
                            (equal (treesit-node-field-name parent) "name"))))))))

;;;; Syntax

(defvar bash-ts-mode-syntax--text-table
  (let ((table (make-syntax-table prog-mode-syntax-table)))
    (dolist (character '(?# ?$ ?' ?` ?\" ?\\ ?\( ?\) ?\[ ?\] ?{ ?}))
      (modify-syntax-entry character "." table))
    (modify-syntax-entry ?\n ">" table)
    table)
  "Syntax table for text without a CST syntax classification.")

(defvar bash-ts-mode-syntax-table
  (let ((table (copy-syntax-table bash-ts-mode-syntax--text-table)))
    (dolist (entry '((?\( . "()") (?\) . ")(")
                     (?\[ . "(]") (?\] . ")[")
                     (?{ . "(}") (?} . "){")))
      (modify-syntax-entry (car entry) (cdr entry) table))
    table)
  "Syntax table for `bash-ts-mode'.")

;;;;; Syntax Queries

(defconst bash-ts-mode-syntax--queries
  (mapcar
   (lambda (source)
     (let ((language (car source)))
       (cons language
             (treesit-query-compile
              language
              '((comment) @comment
                (parameter_expansion ["{" "}"] @delimiter)
                (command_substitution ["(" ")"] @delimiter)
                (process_substitution ["(" ")"] @delimiter)
                (current_shell_substitution ["{" "}"] @delimiter)
                (reply_substitution ["{" "}"] @delimiter)
                (arithmetic_expansion ["(" ")" "[" "]"] @delimiter)
                (arithmetic_command ["(" ")"] @delimiter)
                (arithmetic_for_clause (arithmetic_for_open) @compound-delimiter
                                       ")" @delimiter)
                ([(conditional_open) (conditional_close)] @compound-delimiter)
                (conditional_group ["(" ")"] @delimiter)
                (function_definition ["(" ")"] @delimiter)
                (case_item patterns: (pattern_list "(" @delimiter) ")" @delimiter)
                (brace_group ["{" "}"] @delimiter)
                (brace_expansion ["{" "}"] @delimiter)
                (subshell ["(" ")"] @delimiter)
                (array_assignment ["(" ")"] @delimiter)
                ((array_subscript ["[" "]"] @delimiter)
                 (:pred bash-ts-mode--syntax-delimiter-p @delimiter))
                (io_variable ["{" "}"] @delimiter)
                (extglob_pattern ["(" ")"] @delimiter)
                (extglob_group ["(" ")"] @delimiter)
                (parenthesized_arithmetic ["(" ")"] @delimiter)
                (parenthesized_arithmetic_source ["(" ")"] @delimiter))))))
   bash-ts-mode--grammar-sources)
  "Compiled syntax queries for Bash.")

;;;;; Propertization

(defun bash-ts-mode-syntax--delimiter-syntax (position)
  "Return the syntax descriptor for the delimiter at POSITION."
  (pcase (char-after position)
    (?\( (string-to-syntax "()"))
    (?\) (string-to-syntax ")("))
    (?\[ (string-to-syntax "(]"))
    (?\] (string-to-syntax ")["))
    (?{ (string-to-syntax "(}"))
    (?} (string-to-syntax "){"))))

(defvar-local bash-ts-mode-syntax--changed-start nil
  "Earliest pending syntax change reported by the parser.")

(defun bash-ts-mode-syntax--changed (ranges _parser)
  "Invalidate syntax properties for the parser's changed RANGES."
  (dolist (range ranges)
    (setq bash-ts-mode-syntax--changed-start
          (min (or bash-ts-mode-syntax--changed-start (car range))
               (car range))))
  (when bash-ts-mode-syntax--changed-start
    (syntax-ppss-flush-cache bash-ts-mode-syntax--changed-start)))

(defun bash-ts-mode-syntax--extend-region (start end)
  "Extend START and END to include pending structural changes."
  (treesit-parser-root-node treesit-primary-parser)
  (let ((begin bash-ts-mode-syntax--changed-start))
    (setq bash-ts-mode-syntax--changed-start nil)
    (when (and begin (< begin start))
      (cons (max (point-min) begin) end))))

(defun bash-ts-mode-syntax--propertize (start end)
  "Apply syntax properties between START and END."
  (let ((accessible-start (point-min)))
    (save-restriction
      (widen)
      (when (and (= start accessible-start)
                 (> accessible-start (point-min)))
        (setq start (point-min))
        (syntax-ppss-flush-cache start))
      (put-text-property start end 'syntax-table bash-ts-mode-syntax--text-table)
      (dolist (capture (treesit-query-capture
                        (treesit-parser-root-node treesit-primary-parser)
                        (alist-get (treesit-parser-language treesit-primary-parser)
                                   bash-ts-mode-syntax--queries)
                        start end))
        (let* ((name (car capture))
               (node (cdr capture))
               (position (if (memq name '(comment compound-delimiter))
                             (treesit-node-start node)
                           (1- (treesit-node-end node)))))
          (if (eq name 'comment)
              (let ((end (treesit-node-end node)))
                (put-text-property position (1+ position) 'syntax-table
                                   (string-to-syntax "< b"))
                (when (< end (point-max))
                  (put-text-property end (1+ end) 'syntax-table
                                     (string-to-syntax "> b"))))
            (put-text-property
             position (1+ position) 'syntax-table
             (bash-ts-mode-syntax--delimiter-syntax position))
            (when (eq name 'compound-delimiter)
              (let ((closing (1- (treesit-node-end node))))
                (put-text-property
                 closing (1+ closing) 'syntax-table
                 (bash-ts-mode-syntax--delimiter-syntax closing))))))))))

;;;;; Setup

(defun bash-ts-mode-syntax--setup ()
  "Configure syntax handling for the current buffer."
  (treesit-parser-add-notifier treesit-primary-parser #'bash-ts-mode-syntax--changed)
  (setq-local syntax-propertize-function
              #'bash-ts-mode-syntax--propertize)
  (add-hook 'syntax-propertize-extend-region-functions
            #'syntax-propertize-wholelines nil t)
  (add-hook 'syntax-propertize-extend-region-functions
            #'bash-ts-mode-syntax--extend-region t t))

;;;; Comment Commands

(defun bash-ts-mode-comment--uncomment-region (beg end &optional arg)
  "Uncomment BEG through END using syntax classified before editing.
Pass ARG to `uncomment-region-default'."
  (syntax-propertize end)
  (unwind-protect
      (let ((syntax-propertize-function nil))
        (uncomment-region-default beg end arg))
    (syntax-ppss-flush-cache beg)))

(defun bash-ts-mode-comment--setup ()
  "Configure comment commands for the current buffer."
  (setq-local comment-start "# ")
  (setq-local comment-end "")
  (setq-local comment-start-skip "#[[:blank:]]*")
  (setq-local comment-use-syntax t)
  (setq-local uncomment-region-function #'bash-ts-mode-comment--uncomment-region))

;;;; Electric Pair

(defun bash-ts-mode-electric-pair--newline-context-p ()
  "Return non-nil for a multiline CST delimiter pair around the newline."
  (when (and (eq (char-before) ?\n)
             (>= (- (point) 2) (point-min))
             (< (point) (point-max)))
    (let* ((opening (treesit-node-at (- (point) 2) treesit-primary-parser))
           (closing (treesit-node-at (point) treesit-primary-parser))
           (owner (treesit-node-parent opening)))
      (and (= (treesit-node-start opening) (- (point) 2))
           (= (treesit-node-end opening) (1- (point)))
           (= (treesit-node-start closing) (point))
           (= (treesit-node-end closing) (1+ (point)))
           (treesit-node-eq owner (treesit-node-parent closing))
           (member (treesit-node-type owner)
                   '("brace_group" "subshell" "command_substitution"
                     "parenthesized_arithmetic"
                     "parenthesized_arithmetic_source" "process_substitution"
                     "current_shell_substitution" "reply_substitution"
                     "array_assignment"))))))

(defun bash-ts-mode-electric-pair--setup ()
  "Configure electric pairing for the current buffer."
  (let ((pairs '((?\( . ?\)) (?\[ . ?\]) (?{ . ?})
                 (?\" . ?\") (?\' . ?\') (?` . ?`)))
        (table (copy-syntax-table (syntax-table))))
    (setq-local electric-pair-pairs (append electric-pair-pairs pairs))
    (dolist (pair pairs)
      (unless (eq (cdr (assq (car pair) electric-pair-pairs)) (cdr pair))
        (modify-syntax-entry (car pair) "." table)))
    (set-syntax-table table))
  (let ((setting electric-pair-open-newline-between-pairs))
    (setq-local electric-pair-open-newline-between-pairs
                (lambda ()
                  (and (if (functionp setting) (funcall setting) setting)
                       (bash-ts-mode-electric-pair--newline-context-p))))))

;;;; Font Lock

;;;;; Features

(defconst bash-ts-mode-font-lock--feature-list
  '((comment)
    (keyword function command string)
    (number constant variable escape)
    (pattern regexp operator punctuation bracket))
  "Font-lock features by decoration level.")

;;;;; Settings

(defun bash-ts-mode-font-lock--pattern-source-query (face predicate &optional scope)
  "Return leaf queries using FACE when PREDICATE and SCOPE match."
  (mapcar
   (lambda (pattern)
     (append (list pattern (list :pred predicate face))
             (when scope (list (list :pred scope face)))))
   `(([(pattern_star_source) (pattern_question_source)
       (pattern_bracket_character_source) (pattern_bracket_hyphen_source)
       (pattern_bracket_range_operator_source)
       (pattern_character_class_content_source)
       (pattern_collating_symbol_character_source)
       (pattern_equivalence_class_character_source)] ,face)
     (pattern_bracket_source ["[" "]"] ,face)
     ((pattern_bracket_negation_source) ,face)
     (pattern_character_class_source ["[" ":" "]"] ,face)
     (pattern_collating_symbol_source ["[" "." "]"] ,face)
     (pattern_equivalence_class_source ["[" "=" "]"] ,face))))

(defun bash-ts-mode-font-lock--settings (language)
  "Return font-lock settings for LANGUAGE."
  (treesit-font-lock-rules
   :default-language language

   :feature 'comment
   '((comment_text) @font-lock-comment-face)

   :feature 'keyword
   '([(case_keyword)
      (do_keyword)
      (done_keyword)
      (elif_keyword)
      (else_keyword)
      (esac_keyword)
      (fi_keyword)
      (for_keyword)
      (if_keyword)
      (in_keyword)
      (then_keyword)
      (until_keyword)
      (while_keyword) (function_keyword) (select_keyword)
      (coproc_keyword) (time_keyword)] @font-lock-keyword-face)

   :feature 'function
   '((function_definition
      name: (function_name) @font-lock-function-name-face))

   :feature 'command
   '((declaration_name) @font-lock-function-call-face
     ((simple_command name: (word (literal) @font-lock-function-call-face))
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-function-call-face)))

   :feature 'string
   '(((literal) @font-lock-string-face
      (:pred bash-ts-mode--string-literal-p @font-lock-string-face)
      (:pred bash-ts-mode--outside-pattern-interior-p @font-lock-string-face))
     ((single_quoted
       "'" @font-lock-string-face)
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-string-face))
     ((double_quoted
       "\"" @font-lock-string-face)
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-string-face))
     ((locale_quoted ["$" "\""] @font-lock-string-face)
      (:pred bash-ts-mode--outside-pattern-interior-p @font-lock-string-face))
     ((dollar_single_quoted
       ["$" "'"] @font-lock-string-face)
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-string-face))
     ((backquote_substitution
       "`" @font-lock-string-face)
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-string-face))
     ([(single_quote_content)
       (double_quote_text)
       (dollar_single_quote_text)
       (here_document_text)
       (quoted_here_document_text)] @font-lock-string-face
       (:pred bash-ts-mode--outside-pattern-interior-p
              @font-lock-string-face))
     (here_document_end_text) @font-lock-string-face)

   :feature 'string
   (bash-ts-mode-font-lock--pattern-source-query
    '@font-lock-string-face #'bash-ts-mode--plain-pattern-source-p)

   :feature 'number
   '(([(arithmetic_number) (io_number)] @font-lock-number-face
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-number-face)))

   :feature 'constant
   (bash-ts-mode-font-lock--pattern-source-query
    '@font-lock-constant-face #'bash-ts-mode--tilde-pattern-source-p
    #'bash-ts-mode--outside-pattern-interior-p)

   :feature 'constant
   '(((parameter_expansion
       parameter: [(positional_parameter)
                   (special_parameter)] @font-lock-constant-face)
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-constant-face))
     ((tilde_expansion
       "~" @font-lock-constant-face)
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-constant-face))
     ((tilde_expansion
       user: (tilde_user
              (literal) @font-lock-constant-face))
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-constant-face)))

   :feature 'variable
   '(((assignment_word
       name: (variable_name) @font-lock-variable-name-face)
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-variable-name-face))
     ((for_clause
       name: (name) @font-lock-variable-name-face)
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-variable-name-face))
     ((parameter_expansion
       "$" @font-lock-variable-use-face
       parameter: (_))
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-variable-use-face))
     ((parameter_expansion
       parameter: (variable_name) @font-lock-variable-use-face)
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-variable-use-face))
     ((arithmetic_variable) @font-lock-variable-use-face
      (:pred bash-ts-mode--outside-pattern-interior-p
             @font-lock-variable-use-face)))

   :feature 'escape
   '(([(escaped_character)
       (dollar_single_quote_escape)
       (double_quote_escape)
       (here_document_escape)] @font-lock-escape-face
       (:pred bash-ts-mode--outside-pattern-interior-p
              @font-lock-escape-face)))

   :feature 'pattern
   (bash-ts-mode-font-lock--pattern-source-query
    '@font-lock-constant-face #'bash-ts-mode--tilde-pattern-source-p
    #'bash-ts-mode--pattern-interior-p)

   :feature 'pattern
   '(([(pattern_bracket_character_source)
       (pattern_bracket_hyphen_source)
       (pattern_character_class_content_source)
       (pattern_collating_symbol_character_source)
       (pattern_equivalence_class_character_source)
       (pattern_question_source)
       (pattern_star_source)] @font-lock-constant-face
       (:pred bash-ts-mode--shell-pattern-source-p @font-lock-constant-face))
     ((pattern_bracket_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred bash-ts-mode--shell-pattern-source-p @font-lock-bracket-face))
     ((pattern_bracket_negation_source) @font-lock-negation-char-face
      (:pred bash-ts-mode--shell-pattern-source-p @font-lock-negation-char-face))
     ((pattern_bracket_range_operator_source) @font-lock-operator-face
      (:pred bash-ts-mode--shell-pattern-source-p @font-lock-operator-face))
     ((pattern_character_class_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred bash-ts-mode--shell-pattern-source-p @font-lock-bracket-face))
     ((pattern_character_class_source
       ":" @font-lock-punctuation-face)
      (:pred bash-ts-mode--shell-pattern-source-p @font-lock-punctuation-face))
     ((pattern_collating_symbol_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred bash-ts-mode--shell-pattern-source-p @font-lock-bracket-face))
     ((pattern_collating_symbol_source
       "." @font-lock-punctuation-face)
      (:pred bash-ts-mode--shell-pattern-source-p @font-lock-punctuation-face))
     ((pattern_equivalence_class_source
       ["[" "]"] @font-lock-bracket-face)
      (:pred bash-ts-mode--shell-pattern-source-p @font-lock-bracket-face))
     ((pattern_equivalence_class_source
       "=" @font-lock-punctuation-face)
      (:pred bash-ts-mode--shell-pattern-source-p @font-lock-punctuation-face))
     (pattern_list "|" @font-lock-operator-face))

   :feature 'pattern
   '(((simple_command name: (word (literal) @font-lock-function-call-face))
      (:pred bash-ts-mode--pattern-interior-p @font-lock-function-call-face))
     ((literal) @font-lock-string-face
      (:pred bash-ts-mode--string-literal-p @font-lock-string-face)
      (:pred bash-ts-mode--pattern-interior-p @font-lock-string-face))
     ((single_quoted
       "'" @font-lock-string-face)
      (:pred bash-ts-mode--pattern-interior-p @font-lock-string-face))
     ((double_quoted
       "\"" @font-lock-string-face)
      (:pred bash-ts-mode--pattern-interior-p @font-lock-string-face))
     ((dollar_single_quoted
       ["$" "'"] @font-lock-string-face)
      (:pred bash-ts-mode--pattern-interior-p @font-lock-string-face))
     ((backquote_substitution
       "`" @font-lock-string-face)
      (:pred bash-ts-mode--pattern-interior-p @font-lock-string-face))
     ([(single_quote_content)
       (double_quote_text)
       (dollar_single_quote_text)] @font-lock-string-face
       (:pred bash-ts-mode--pattern-interior-p @font-lock-string-face))
     ((tilde_expansion
       "~" @font-lock-constant-face)
      (:pred bash-ts-mode--pattern-interior-p @font-lock-constant-face))
     ((tilde_expansion
       user: (tilde_user
              (literal) @font-lock-constant-face))
      (:pred bash-ts-mode--pattern-interior-p @font-lock-constant-face))
     ([(arithmetic_number) (io_number)] @font-lock-number-face
      (:pred bash-ts-mode--pattern-interior-p @font-lock-number-face))
     ((parameter_expansion
       parameter: [(positional_parameter)
                   (special_parameter)] @font-lock-constant-face)
      (:pred bash-ts-mode--pattern-interior-p @font-lock-constant-face))
     ((parameter_expansion
       "$" @font-lock-variable-use-face
       parameter: (_))
      (:pred bash-ts-mode--pattern-interior-p
             @font-lock-variable-use-face))
     ((parameter_expansion
       parameter: (variable_name) @font-lock-variable-use-face)
      (:pred bash-ts-mode--pattern-interior-p
             @font-lock-variable-use-face))
     ((arithmetic_variable) @font-lock-variable-use-face
      (:pred bash-ts-mode--pattern-interior-p
             @font-lock-variable-use-face))
     ([(escaped_character)
       (dollar_single_quote_escape)
       (double_quote_escape)
       (here_document_escape)] @font-lock-escape-face
       (:pred bash-ts-mode--pattern-interior-p @font-lock-escape-face)))

   :feature 'pattern
   '(((locale_quoted ["$" "\""] @font-lock-string-face)
      (:pred bash-ts-mode--pattern-interior-p @font-lock-string-face))
     ((brace_endpoint) @font-lock-string-face
      (:pred bash-ts-mode--pattern-interior-p @font-lock-string-face))
     ((brace_increment) @font-lock-number-face
      (:pred bash-ts-mode--pattern-interior-p @font-lock-number-face))
     ((array_reference name: (variable_name) @font-lock-variable-use-face)
      (:pred bash-ts-mode--pattern-interior-p @font-lock-variable-use-face))
     ((parameter_transformation_code) @font-lock-constant-face
      (:pred bash-ts-mode--pattern-interior-p @font-lock-constant-face)))

   :feature 'string
   '(((brace_endpoint) @font-lock-string-face
      (:pred bash-ts-mode--outside-pattern-interior-p @font-lock-string-face)))

   :feature 'number
   '(((brace_increment) @font-lock-number-face
      (:pred bash-ts-mode--outside-pattern-interior-p @font-lock-number-face)))

   :feature 'constant
   '((time_option) @font-lock-constant-face
     (coproc name: (word (literal) @font-lock-constant-face))
     ((parameter_transformation_code) @font-lock-constant-face
      (:pred bash-ts-mode--outside-pattern-interior-p @font-lock-constant-face)))

   :feature 'variable
   '((select_clause name: (name) @font-lock-variable-name-face)
     (io_variable name: (variable_name) @font-lock-variable-name-face)
     ((array_reference name: (variable_name) @font-lock-variable-use-face)
      (:pred bash-ts-mode--outside-pattern-interior-p @font-lock-variable-use-face)))

   :feature 'regexp
   '((ordinary_character) @font-lock-regexp-face
     [(collating_element_single) (collating_element_multi)] @font-lock-constant-face
     (one_char_or_coll_elem_ere "." @font-lock-constant-face)
     (class_name) @font-lock-constant-face
     (meta_character) @font-lock-constant-face
     (duplication_count) @font-lock-number-face
     (start_range "-" @font-lock-operator-face)
     (range_expression "-" @font-lock-constant-face)
     (bracket_list "-" @font-lock-constant-face)
     (bracket_expression
      ["[" "]"] @font-lock-bracket-face)
     (nonmatching_list "^" @font-lock-negation-char-face)
     (collating_symbol
      ["[" "]"] @font-lock-bracket-face)
     (collating_symbol
      "." @font-lock-punctuation-face)
     (equivalence_class
      ["[" "]"] @font-lock-bracket-face)
     (equivalence_class
      "=" @font-lock-punctuation-face)
     (character_class
      ["[" "]"] @font-lock-bracket-face)
     (character_class
      ":" @font-lock-punctuation-face)
     (ere_expression
      ["(" ")"] @font-lock-bracket-face)
     (extended_reg_exp
      "|" @font-lock-operator-face)
     (ere_expression ["^" "$"] @font-lock-operator-face)
     (ere_dupl_symbol
      ["*" "+" "?"] @font-lock-operator-face)
     (ere_dupl_symbol
      ["{" "}"] @font-lock-bracket-face)
     (ere_dupl_symbol
      "," @font-lock-punctuation-face)
     (repetition_modifier) @font-lock-operator-face)

   :feature 'operator
   '([(and_if) (arithmetic_operator) (bang) (clobber) (dgreat)
      (dless) (dlessdash) (dsemi) (greatand) (lessand) (lessgreat)
      (or_if) (parameter_length_operator) (parameter_pattern_operator)
      (parameter_value_operator) (semi_and) (assignment_append)
      (conditional_unary_operator) (conditional_binary_operator)
      (conditional_match_operator) (conditional_regex_operator) (conditional_not)
      (pipe_and) (dsemi_and) (andgreat) (anddgreat) (here_string_operator)
      (parameter_indirection_operator) (parameter_names_operator)
      (parameter_substring_operator) (parameter_replacement_operator)
      (parameter_case_operator) (extglob_operator)] @font-lock-operator-face
      (assignment_word "=" @font-lock-operator-face)
      (array_element_assignment "=" @font-lock-operator-face)
      (separator_op "&" @font-lock-operator-face)
      (pipeline "|" @font-lock-operator-face)
      (reply_substitution "|" @font-lock-operator-face)
      (extglob_pattern "|" @font-lock-operator-face)
      (extglob_group "|" @font-lock-operator-face)
      (io_file operator: ["<" ">"] @font-lock-operator-face)
      (process_substitution direction: ["<" ">"] @font-lock-operator-face)
      (parameter_transformation_operator) @font-lock-operator-face)

   :feature 'punctuation
   '(";" @font-lock-punctuation-face
     (line_continuation) @font-lock-punctuation-face
     (command_substitution "$" @font-lock-punctuation-face)
     (current_shell_substitution "$" @font-lock-punctuation-face)
     (reply_substitution "$" @font-lock-punctuation-face)
     (arithmetic_expansion "$" @font-lock-punctuation-face)
     (brace_expansion "," @font-lock-punctuation-face)
     (brace_sequence_separator) @font-lock-punctuation-face
     (parameter_substring_separator) @font-lock-punctuation-face
     (parameter_replacement_separator) @font-lock-punctuation-face)

   :feature 'bracket
   '((["(" ")" "{" "}"] @font-lock-bracket-face
      (:pred bash-ts-mode--syntax-delimiter-p @font-lock-bracket-face))
     ((array_subscript ["[" "]"] @font-lock-bracket-face)
      (:pred bash-ts-mode--syntax-delimiter-p @font-lock-bracket-face))
     (arithmetic_expansion ["[" "]"] @font-lock-bracket-face)
     [(conditional_open) (conditional_close)] @font-lock-bracket-face
     (arithmetic_for_open) @font-lock-bracket-face)

   :feature 'punctuation
   :override t
   '(((line_continuation) @font-lock-punctuation-face
      (:pred bash-ts-mode--continued-lexical-token-p
             @font-lock-punctuation-face)))))

;;;;; Setup

(defun bash-ts-mode-font-lock--setup ()
  "Configure font lock for the current buffer."
  (setq-local treesit-font-lock-feature-list
              bash-ts-mode-font-lock--feature-list)
  (setq-local treesit-font-lock-settings
              (bash-ts-mode-font-lock--settings
               (treesit-parser-language treesit-primary-parser))))

;;;; Navigation

(defconst bash-ts-mode-navigation--function-definition-regexp
  "^function_definition$"
  "Regexp matching Bash 5.3 function definitions.")

(defconst bash-ts-mode-navigation--settings
  (let ((things
         `((sexp (and ,(rx string-start
                           (or
                            "word" "assignment_word" "name"
                            "function_name" "single_quoted" "double_quoted"
                            "dollar_single_quoted" "locale_quoted" "parameter_expansion"
                            "arithmetic_expansion" "command_substitution" "backquote_substitution"
                            "process_substitution" "current_shell_substitution" "reply_substitution"
                            "array_assignment" "array_element_assignment" "array_reference"
                            "array_subscript" "brace_expansion" "extglob_pattern"
                            "conditional_pattern" "regex_operand" "brace_group"
                            "subshell" "for_clause" "select_clause"
                            "arithmetic_for_clause" "case_clause" "if_clause"
                            "while_clause" "until_clause" "arithmetic_command"
                            "conditional_command" "coproc" "function_definition")
                           string-end)
                      bash-ts-mode--syntax-structure-p))
           (defun ,bash-ts-mode-navigation--function-definition-regexp))))
    (mapcar (lambda (source) (cons (car source) things))
            bash-ts-mode--grammar-sources))
  "Tree-sitter thing definitions for Bash.")

(defun bash-ts-mode-navigation--setup ()
  "Configure navigation for the current buffer."
  (setq-local treesit-thing-settings
              bash-ts-mode-navigation--settings))

;;;; Imenu

(defun bash-ts-mode-imenu--name (node)
  "Return the source name of NODE, or nil if it has no name."
  (when (treesit-node-match-p
         node bash-ts-mode-navigation--function-definition-regexp)
    (let ((name (treesit-node-child-by-field-name node "name")))
      (when (and name (equal (treesit-node-type name) "function_name"))
        (treesit-node-text name t)))))

(defconst bash-ts-mode-imenu--settings
  `(("Function" ,bash-ts-mode-navigation--function-definition-regexp nil nil))
  "Tree-sitter Imenu settings for Bash.")

(defun bash-ts-mode-imenu--setup ()
  "Configure Imenu for the current buffer."
  (setq-local treesit-defun-name-function
              #'bash-ts-mode-imenu--name)
  (setq-local treesit-simple-imenu-settings
              bash-ts-mode-imenu--settings))

;;;; Indentation

(defcustom bash-ts-mode-indent-offset 2
  "Number of spaces for each indentation level."
  :type 'natnum
  :group 'bash-ts)

(defun bash-ts-mode-indent--continued-line (_node _parent bol)
  "Preserve indentation at BOL after a continuation leaf."
  (when (and (> bol (1+ (point-min)))
             (equal (treesit-node-type
                     (treesit-node-on (- bol 2) (1- bol) treesit-primary-parser))
                    "line_continuation"))
    (cons bol 0)))

(defun bash-ts-mode-indent--closing-delimiter-p (node _parent _bol)
  "Return non-nil when NODE closes a confirmed syntax structure."
  (and (member (treesit-node-type node) '("}" ")" "conditional_close" "]"))
       (bash-ts-mode--syntax-delimiter-p node)))

(defun bash-ts-mode-indent--body (_node parent _bol)
  "Return the indentation for a command list in PARENT."
  (when (equal (treesit-node-type parent) "separator")
    (setq parent (treesit-node-parent parent)))
  (when (member (treesit-node-type parent)
                '("list" "command_substitution_body" "backquote_substitution_body"))
    (let ((owner (treesit-node-parent parent)))
      (when (member (treesit-node-type owner)
                    '("command_substitution_body" "backquote_substitution_body"))
        (setq owner (treesit-node-parent owner)))
      (if (equal (treesit-node-type owner) "program")
          (cons (point-min) 0)
        (cons (save-excursion
                (goto-char (treesit-node-start owner))
                (back-to-indentation)
                (point))
              bash-ts-mode-indent-offset)))))

(defconst bash-ts-mode-indent--rules
  (let ((rules
         '(bash-ts-mode-indent--continued-line
           (bash-ts-mode-indent--closing-delimiter-p standalone-parent 0)
           ((node-is "then_keyword") parent-bol 0)
           ((node-is "else_part") parent-bol 0)
           ((node-is "fi_keyword") parent-bol 0)
           ((node-is "do_group") standalone-parent 0)
           ((node-is "done_keyword") parent-bol 0)
           ((node-is "esac_keyword") parent-bol 0)
           ((node-is "case_item") parent-bol bash-ts-mode-indent-offset)
           ((field-is "terminator") parent-bol bash-ts-mode-indent-offset)
           bash-ts-mode-indent--body
           ((or (parent-is "array_assignment") (parent-is "conditional_command")
                (parent-is "arithmetic_command") (parent-is "arithmetic_expansion")
                (parent-is "arithmetic_for_clause"))
            parent-bol bash-ts-mode-indent-offset)
           ((parent-is "and_or") parent-bol bash-ts-mode-indent-offset)
           ((parent-is "pipeline") parent-bol bash-ts-mode-indent-offset)
           ((parent-is "program") column-0 0))))
    (mapcar (lambda (source) (cons (car source) rules))
            bash-ts-mode--grammar-sources))
  "Tree-sitter indentation rules for Bash.")

(defun bash-ts-mode-indent--setup ()
  "Configure indentation for the current buffer."
  (setq-local treesit-simple-indent-rules
              bash-ts-mode-indent--rules))

;;;; Mode

(defun bash-ts-mode--setup ()
  "Configure `bash-ts-mode' in the current buffer."
  (let ((language (bash-ts-mode--language)))
    (bash-ts-mode--ensure-grammar language)
    (setq-local treesit-primary-parser (treesit-parser-create language)))
  (bash-ts-mode-syntax--setup)
  (bash-ts-mode-comment--setup)
  (bash-ts-mode-electric-pair--setup)
  (bash-ts-mode-font-lock--setup)
  (bash-ts-mode-navigation--setup)
  (bash-ts-mode-imenu--setup)
  (bash-ts-mode-indent--setup)
  (treesit-major-mode-setup)
  (bash-ts-mode-update--setup))

;;;###autoload
(define-derived-mode bash-ts-mode prog-mode "Bash-TS"
  "Major mode for editing Bash 5.3."
  :syntax-table bash-ts-mode-syntax-table
  :group 'bash-ts
  (bash-ts-mode--setup))

;;;###autoload
(defun bash-ts-mode-auto-mode ()
  "Select a major mode for a Bash file, respecting its shebang."
  (if (save-excursion
        (goto-char (point-min))
        (looking-at-p "#!"))
      (let ((auto-mode-alist
             (rassq-delete-all #'bash-ts-mode-auto-mode
                               (copy-alist auto-mode-alist))))
        (set-auto-mode))
    (bash-ts-mode)))

;;;###autoload
(add-to-list 'auto-mode-alist `(,(rx (or ".bash" (seq "/.bash" (or "rc" "_profile" "_login" "_logout")))
                                     string-end)
                                . bash-ts-mode-auto-mode))

;;;###autoload
(add-to-list 'interpreter-mode-alist '("bash" . bash-ts-mode))

(provide 'bash-ts-mode)

;;; bash-ts-mode.el ends here
