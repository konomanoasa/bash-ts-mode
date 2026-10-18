;;; bash-ts-mode-test.el --- Tests for bash-ts-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 konomanoasa
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

;;; Code:

(require 'ert)
(require 'imenu)
(require 'newcomment)
(require 'bash-ts-mode)

(dolist (language '(bash bash_extglob))
  (unless (treesit-ready-p language t)
    (error "The %s grammar is required to run the tests" language)))

;;;; Helpers

(defmacro bash-ts-mode-test--with-editorconfig (properties &rest body)
  "Evaluate BODY with temporary EditorConfig PROPERTIES."
  (declare (indent 1) (debug (form body)))
  `(let ((default-directory
          (file-name-as-directory (make-temp-file "bash-ts-mode-" t))))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name ".editorconfig")
             (insert "root = true\n[*.bash]\n" ,properties))
           ,@body)
       (delete-directory default-directory t))))

(defmacro bash-ts-mode-test--with-script (properties source &rest body)
  "Evaluate BODY in SOURCE with EditorConfig PROPERTIES."
  (declare (indent 2) (debug (form form body)))
  `(bash-ts-mode-test--with-editorconfig ,properties
     (with-temp-buffer
       (setq buffer-file-name (expand-file-name "script.bash"))
       (insert ,source)
       (bash-ts-mode)
       (font-lock-ensure)
       ,@body)))

(defun bash-ts-mode-test--position (fragment &optional line)
  "Return the position of FRAGMENT, optionally within LINE."
  (save-excursion
    (goto-char (point-min))
    (when line
      (let ((found nil))
        (while (and (not found) (not (eobp)))
          (if (equal line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
              (setq found t)
            (forward-line 1)))
        (unless found (ert-fail (format "Missing fixture line: %S" line)))))
    (unless (search-forward fragment (and line (line-end-position)) t)
      (ert-fail (format "Missing fixture fragment: %S" fragment)))
    (- (point) (length fragment))))

(defun bash-ts-mode-test--face (fragment &optional offset line)
  "Return the face at FRAGMENT plus OFFSET, optionally within LINE."
  (get-text-property (+ (bash-ts-mode-test--position fragment line)
                        (or offset 0)) 'face))

(defun bash-ts-mode-test--comment-p (fragment &optional offset line)
  "Return comment state at FRAGMENT plus OFFSET, optionally within LINE."
  (syntax-propertize (point-max))
  (nth 4 (syntax-ppss (+ (bash-ts-mode-test--position fragment line)
                         (or offset 0)))))

(defun bash-ts-mode-test--syntax-class (fragment &optional offset line)
  "Return syntax class at FRAGMENT plus OFFSET, optionally within LINE."
  (syntax-propertize (point-max))
  (syntax-class (syntax-after (+ (bash-ts-mode-test--position fragment line)
                                 (or offset 0)))))

(defun bash-ts-mode-test--indent (source &optional offset)
  "Indent SOURCE with OFFSET and verify that repeating it changes nothing."
  (with-temp-buffer
    (insert source)
    (bash-ts-mode)
    (setq-local indent-tabs-mode nil)
    (when offset
      (setq-local bash-ts-mode-indent-offset offset))
    (indent-region (point-min) (point-max))
    (let ((indented (buffer-string)))
      (indent-region (point-min) (point-max))
      (should (equal (buffer-string) indented))
      indented)))

(defun bash-ts-mode-test--buffer-state ()
  "Return fontification and syntax properties for the current buffer."
  (font-lock-ensure)
  (syntax-propertize (point-max))
  (let (state)
    (dotimes (offset (- (point-max) (point-min)))
      (let ((position (+ (point-min) offset)))
        (push (list (get-text-property position 'face) (syntax-after position)) state)))
    (nreverse state)))

(defun bash-ts-mode-test--should-match-fresh-buffer (&optional level compare-tree)
  "Compare classifications at LEVEL and optionally COMPARE-TREE with fresh parsing."
  (let ((source (buffer-substring-no-properties (point-min) (point-max)))
        (state (bash-ts-mode-test--buffer-state))
        (tree (when compare-tree
                (treesit-node-string (treesit-parser-root-node treesit-primary-parser))))
        (file buffer-file-name)
        (level (or level treesit-font-lock-level)))
    (with-temp-buffer
      (setq buffer-file-name file)
      (insert source)
      (let ((treesit-font-lock-level level)) (bash-ts-mode))
      (should (equal state (bash-ts-mode-test--buffer-state)))
      (when compare-tree
        (should (equal tree (treesit-node-string
                             (treesit-parser-root-node treesit-primary-parser))))))))

;;;; Mode Selection

(ert-deftest bash-ts-mode-selects-editorconfig-conditions-on-file-visits ()
  (dolist (enabled '(nil t))
    (pcase-dolist
        (`(,properties ,language)
         '(("" bash)
           ("extglob = false\n" bash)
           ("extglob = true\n" bash_extglob)
           ("extglob = unset\n" bash)
           ("extquote = false\n" bash)
           ("extglob = true\nextquote = false\n" bash_extglob)
           ("extquote = invalid\n" bash)))
      (ert-info ((format "EditorConfig mode %S, properties %S" enabled properties))
        (bash-ts-mode-test--with-editorconfig properties
          (with-temp-file (expand-file-name "script.bash") (insert "echo hello\n"))
          (let ((previous editorconfig-mode))
            (unwind-protect
                (progn
                  (editorconfig-mode (if enabled 1 -1))
                  (with-current-buffer (find-file-noselect
                                        (expand-file-name "script.bash"))
                    (unwind-protect
                        (progn
                          (should (eq major-mode 'bash-ts-mode))
                          (should (eq (treesit-parser-language treesit-primary-parser)
                                      language))
                          (should (eq editorconfig-mode enabled)))
                      (kill-buffer))))
              (editorconfig-mode (if previous 1 -1)))))))))

(ert-deftest bash-ts-mode-defaults-without-a-file-name ()
  (bash-ts-mode-test--with-editorconfig
      "extglob = true\n"
    (with-temp-buffer
      (bash-ts-mode)
      (should-not buffer-file-name)
      (should (eq (treesit-parser-language treesit-primary-parser) 'bash)))))

(ert-deftest bash-ts-mode-rejects-invalid-editorconfig-conditions ()
  (dolist (value '("TRUE" "False" "on" "off" "1" "invalid"))
    (bash-ts-mode-test--with-editorconfig
        (format "extglob = %s\n" value)
      (with-temp-buffer
        (setq buffer-file-name (expand-file-name "script.bash"))
        (let ((error-data (should-error (bash-ts-mode) :type 'user-error)))
          (should (equal (error-message-string error-data)
                         (format "Invalid EditorConfig extglob: %S (expected true or false)"
                                 value))))
        (should-not (treesit-parser-list nil nil t))))))

(ert-deftest bash-ts-mode-reloads-editorconfig-only-on-mode-restart ()
  (bash-ts-mode-test--with-editorconfig ""
    (with-temp-buffer
      (setq buffer-file-name (expand-file-name "script.bash"))
      (insert "echo hello\n")
      (bash-ts-mode)
      (pcase-dolist (`(,extglob ,language)
                     '(("true" bash_extglob)
                       ("false" bash)))
        (let ((parser treesit-primary-parser))
          (with-temp-file (expand-file-name ".editorconfig")
            (insert "root = true\n[*.bash]\nextglob = " extglob
                    "\n"))
          (goto-char (point-max))
          (insert "# edited\n")
          (delete-region (line-beginning-position 0) (point-max))
          (font-lock-flush)
          (font-lock-ensure)
          (syntax-propertize (point-max))
          (should (eq treesit-primary-parser parser))
          (bash-ts-mode)
          (font-lock-ensure)
          (should (eq (treesit-parser-language treesit-primary-parser) language))
          (should (equal (treesit-parser-list) (list treesit-primary-parser)))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         "echo hello\n")))))))

(ert-deftest bash-ts-mode-selects-files-and-interpreters ()
  (let ((original (copy-tree auto-mode-alist)))
    (pcase-dolist (`(,file ,prefix ,expected)
                   '(("/tmp/script.bash" "" t)
                     ("/tmp/script.bash.gz" "" t)
                     ("/tmp/.bashrc" "" t)
                     ("/tmp/.bash_profile" "" t)
                     ("/tmp/.bash_login" "" t)
                     ("/tmp/.bash_logout" "" t)
                     ("/tmp/script.sh" "" nil)
                     ("/tmp/.profile" "" nil)
                     ("/tmp/script.bash.extra" "" nil)
                     ("/tmp/script.bash" "# comment\n" t)
                     ("/tmp/script.bash" "#!/bin/sh\n" nil)
                     ("/tmp/.bashrc" "#!/bin/zsh\n" nil)
                     ("/tmp/script.bash" "#!/usr/bin/unknown\n" nil)
                     ("/tmp/script.bash" "#!\n" nil)
                     ("/tmp/script.sh" "#!/bin/bash\n" t)
                     ("/tmp/script" "#!/bin/bash -e\n" t)
                     ("/tmp/script" "#!/usr/bin/env bash\n" t)
                     ("/tmp/script" "#!/usr/bin/env -S bash -e\n" t)))
      (ert-info ((format "%s: %S" file prefix))
        (with-temp-buffer
          (setq buffer-file-name file)
          (insert prefix "printf hello\n")
          (set-auto-mode)
          (should (eq (eq major-mode 'bash-ts-mode) expected)))))
    (should (equal auto-mode-alist original))))

(ert-deftest bash-ts-mode-keeps-bash-grammar-after-loading-sh-mode ()
  (with-temp-buffer (sh-mode))
  (with-temp-buffer
    (insert "echo hi\n")
    (bash-ts-mode)
    (should (eq major-mode 'bash-ts-mode))
    (should (eq (derived-mode-p 'prog-mode) 'prog-mode))
    (should (eq (keymap-parent (current-local-map)) prog-mode-map))
    (should (equal (treesit-node-type (treesit-node-at 1 'bash)) "literal"))))

;;;; Syntax

(ert-deftest bash-ts-mode-preserves-confirmed-syntax-in-subscript-candidates ()
  (let ((treesit-font-lock-level 4))
    (bash-ts-mode-test--with-script ""
        "echo ${a[key${name}$(printf x)$((2+3))]} ${a['quoted']} ${a[\\&]} ${a[ind\\\nex+1]}\n"
      (pcase-dolist (`(,fragment ,face)
                     '(("key" nil)
                       ("name" font-lock-variable-use-face)
                       ("printf" font-lock-function-call-face)
                       ("2" font-lock-number-face)
                       ("+3" font-lock-operator-face)
                       ("quoted" font-lock-string-face)
                       ("\\&" font-lock-escape-face)
                       ("ind" nil)
                       ("ex+1" nil)
                       ("\\\n" font-lock-punctuation-face)))
        (ert-info (fragment)
          (should (eq (bash-ts-mode-test--face fragment) face))))
      (goto-char (bash-ts-mode-test--position "$("))
      (forward-sexp)
      (should (eq (char-before) ?\)))
      (backward-sexp)
      (should (= (point) (bash-ts-mode-test--position "$("))))))

(ert-deftest bash-ts-mode-classifies-continued-compound-delimiters ()
  (with-temp-buffer
    (insert "[\\\n[ x == y ]\\\n]\nfor (\\\n(i=0; i<2; i++)); do :; done\n")
    (bash-ts-mode)
    (should-not (treesit-node-check (treesit-parser-root-node treesit-primary-parser) 'has-error))
    (pcase-dolist (`(,fragment ,offset ,class)
                   '(("[\\\n[" 0 4) ("[\\\n[" 3 4)
                     ("]\\\n]" 0 5) ("]\\\n]" 3 5)
                     ("(\\\n(" 0 4) ("(\\\n(" 3 4)))
      (should (= (bash-ts-mode-test--syntax-class fragment offset) class)))))

;;;; Comment Commands

(ert-deftest bash-ts-mode-uncomments-multiline-source ()
  (dolist (case
           '(("cat <<EOF\nhello\nEOF\n" "# cat <<EOF\n# hello\n# EOF\n")
             ("printf 'one\n# two'\n" "# printf 'one\n# # two'\n")))
    (with-temp-buffer
      (insert (car case))
      (bash-ts-mode)
      (let ((state (bash-ts-mode-test--buffer-state)))
        (comment-or-uncomment-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (cadr case)))
        (comment-or-uncomment-region (point-min) (point-max))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (car case)))
        (should (equal (bash-ts-mode-test--buffer-state) state))))))

;;;; Electric Pair

(ert-deftest bash-ts-mode-restricts-pair-newlines-to-cst-contexts ()
  (pcase-dolist (`(,before ,after ,expected)
                 '(("{" "}" "{\n\n}")
                   ("func() {" "}" "func() {\n\n}")
                   ("(" ")" "(\n\n)")
                   ("printf $(" ")" "printf $(\n\n)")
                   ("printf ${name:-{" "}}" "printf ${name:-{\n}}")
                   ("printf ${" "name}" "printf ${\nname}")
                   ("func(" ") { :; }" "func(\n) { :; }")
                   ("case x in (" ") :;; esac" "case x in (\n) :;; esac")
                   ("printf [" "]" "printf [\n]")
                   ("printf '{" "}'" "printf '{\n}'")
                   ("printf \"(" ")\"" "printf \"(\n)\"")
                   ("# {" "}" "# {\n}")
                   ("cat <<EOF\n{" "}\nEOF\n" "cat <<EOF\n{\n}\nEOF\n")))
    (ert-info ((format "%S / %S" before after))
      (let ((electric-pair-open-newline-between-pairs t))
        (with-temp-buffer
          (insert before after)
          (bash-ts-mode)
          (electric-indent-local-mode -1)
          (electric-pair-local-mode 1)
          (goto-char (1+ (length before)))
          (call-interactively (key-binding (kbd "RET")))
          (should (equal (buffer-substring-no-properties (point-min) (point-max))
                         expected)))))))

(ert-deftest bash-ts-mode-restricts-bash-pair-newlines ()
  (pcase-dolist (`(,prefix ,suffix ,extra)
                 '(("cat <(" ")" t)
                   ("values=(" ")" t)
                   ("value=${" "}" t)
                   ("value=${| " "}" nil)
                   ("echo ${value:-" "}" nil)
                   ("echo {a," "b}" nil)
                   ("values[" "]=x" nil)
                   ("[[ (" ") ]]" nil)))
    (with-temp-buffer
      (insert prefix suffix)
      (bash-ts-mode)
      (setq-local indent-tabs-mode nil)
      (let ((electric-pair-open-newline-between-pairs t))
        (bash-ts-mode-electric-pair--setup)
        (electric-indent-local-mode -1)
        (electric-pair-local-mode 1)
        (goto-char (1+ (length prefix)))
        (call-interactively (key-binding (kbd "RET")))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       (concat prefix "\n" (if extra "\n" "") suffix)))))))

;;;; Navigation

(ert-deftest bash-ts-mode-keeps-candidate-groups-out-of-structural-editing ()
  (bash-ts-mode-test--with-script "" "echo ${a[(i+1)]}\n"
    (goto-char (bash-ts-mode-test--position "(i"))
    (forward-sexp)
    (should (= (point) (bash-ts-mode-test--position "}\n"))))
  (let ((electric-pair-open-newline-between-pairs t))
    (bash-ts-mode-test--with-script "" "echo ${a[()]}\n"
      (electric-indent-local-mode -1)
      (electric-pair-local-mode 1)
      (goto-char (1+ (bash-ts-mode-test--position "()")))
      (call-interactively (key-binding (kbd "RET")))
      (should (equal (buffer-string) "echo ${a[(\n)]}\n")))))

(ert-deftest bash-ts-mode-navigates-structures ()
  (with-temp-buffer
    (insert "printf before\n"
            "first() {\n  :\n}\n"
            "printf between\n"
            "second() (\n  :\n)\n"
            "printf after\n")
    (bash-ts-mode)
    (goto-char (point-max))
    (beginning-of-defun)
    (should (looking-at-p "second()"))
    (beginning-of-defun)
    (should (looking-at-p "first()"))
    (end-of-defun)
    (should (equal (buffer-substring-no-properties
                    (line-beginning-position 0) (point))
                   "}\n"))))

(ert-deftest bash-ts-mode-navigates-nested-sexp-boundaries ()
  (pcase-dolist (`(,source ,start ,direction ,expected)
                 '(("printf pre${value}post\n" 8 1 23)
                   ("printf pre${value}post\n" 23 -1 8)
                   ("printf pre${value}post\n" 11 1 19)
                   ("printf pre${value}post\n" 19 -1 11)
                   ("printf pre${value}post\n" 15 1 19)
                   ("printf pre${value}post\n" 15 -1 11)
                   ("printf pre${value}\n" 11 1 19)
                   ("printf pre${value}\n" 19 -1 8)))
    (ert-info ((format "%S from %s by %s" source start direction))
      (with-temp-buffer
        (insert source)
        (bash-ts-mode)
        (goto-char start)
        (forward-sexp direction)
        (should (= (point) expected))))))

;;;; Imenu

(ert-deftest bash-ts-mode-keeps-imenu-source-spelling-and-duplicate-names ()
  (with-temp-buffer
    (insert "same() { :; }\nsa\\\nme() { :; }\nsame() { :; }\n")
    (bash-ts-mode)
    (let ((entries (cdr (assoc "Function" (funcall imenu-create-index-function)))))
      (should (equal (mapcar #'car entries) '("same" "sa\\\nme" "same")))
      (should (equal (mapcar (lambda (entry) (marker-position (cdr entry))) entries)
                     '(1 15 31))))))

(ert-deftest bash-ts-mode-indexes-bash-function-names ()
  (with-temp-buffer
    (insert "function build-app { :; }\nfunction build-app() { :; }\nplain() { :; }\n")
    (bash-ts-mode)
    (should (equal (mapcar #'car (cdr (assoc "Function" (funcall imenu-create-index-function))))
                   '("build-app" "build-app" "plain")))))

;;;; Indentation

(ert-deftest bash-ts-mode-preserves-subscript-candidate-indentation ()
  (bash-ts-mode-test--with-script ""
      "echo ${a[(i+1\n    )]}\necho ${a[$(\nprintf x\n)]}\n"
    (setq-local indent-tabs-mode nil)
    (indent-region (point-min) (point-max))
    (should (equal (buffer-string)
                   "echo ${a[(i+1\n    )]}\necho ${a[$(\n  printf x\n)]}\n"))))

(ert-deftest bash-ts-mode-indents-consecutive-returns ()
  (pcase-dolist (`(,prefix ,suffix ,column)
                 '(("f() {\n  echo ok" "\n}" 2)
                   ("if :; then\n  if :; then\n    echo ok" "\n  fi\nfi" 4)
                   ("if :; then\n  if :; then\n    echo ok\n  fi" "\nfi" 2)
                   ("x=$(\n  echo ok" "\n)" 2)
                   ("x=$(echo ok" "\necho more)" 2)
                   ("x=`\n  echo ok" "\n`" 2)
                   ("f() {\n  x=$(\n    echo ok" "\n  )\n}" 4)
                   ("x=$(\n  echo $(\n    echo ok" "\n  )\n)" 4)
                   ("f() {\n  echo a |" "\n    cat\n}" 4)
                   ("f() {\n  echo a &&" "\n    echo b\n}" 4)
                   ("f() {\n  echo a ||" "\n    echo b\n}" 4)
                   ("echo a |" "\n  cat\n" 2)
                   ("echo ok" "" 0)))
    (ert-info ((format "%S / %S" prefix suffix))
      (with-temp-buffer
        (insert prefix suffix)
        (bash-ts-mode)
        (setq-local indent-tabs-mode nil)
        (electric-indent-local-mode 1)
        (goto-char (1+ (length prefix)))
        (dotimes (_ 3)
          (call-interactively (key-binding (kbd "RET")))
          (should (= (current-column) column)))))))

(ert-deftest bash-ts-mode-preserves-expansion-boundaries-during-indentation ()
  (pcase-dolist (`(,prefix ,suffix)
                 '(("if :; then\n  " "\nfi\n")
                   ("worker() {\n  " "\n}\n")))
    (pcase-dolist (`(,left ,right)
                   '(("value=$((count + 1)" ")")
                     ("value=${name" "}")
                     ("value=${#name" "}")
                     ("value=${name:-fallback" "}")
                     ("value=${name:-" "}")
                     ("value=${name%pat" "}")
                     ("value=\"${name:-fallback" "}\"")
                     ("value=${name:-fallback" "  }")))
      (dolist (operation '(newline-and-indent indent-region))
        (ert-info ((format "%s: %S" operation (concat prefix left right suffix)))
          (with-temp-buffer
            (insert prefix left right suffix)
            (bash-ts-mode)
            (goto-char (+ 1 (length prefix) (length left)))
            (insert "\\")
            (if (eq operation 'newline-and-indent)
                (newline-and-indent)
              (newline)
              (indent-region (point-min) (point-max)))
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           (concat prefix left "\\\n" right suffix)))
            (should-not (treesit-node-check (treesit-parser-root-node treesit-primary-parser)
                                            'has-error))))))))

(ert-deftest bash-ts-mode-keeps-here-document-bodies-unindented ()
  (should
   (equal
    (bash-ts-mode-test--indent
     "{\ncat <<EOF\nbody text\n  spaced\nEOF\n: after\n}\n")
    "{\n  cat <<EOF\nbody text\n  spaced\nEOF\n  : after\n}\n")))

(ert-deftest bash-ts-mode-indents-bash-structures ()
  (pcase-dolist (`(,source ,expected)
                 '(("values=(\none\n[2]=two\n)\n" "values=(\n  one\n  [2]=two\n)\n")
                   ("select x in a b; do\necho x\ndone\n" "select x in a b; do\n  echo x\ndone\n")
                   ("for ((i=0; i<2; i++)); do\necho x\ndone\n" "for ((i=0; i<2; i++)); do\n  echo x\ndone\n")
                   ("coproc worker {\necho x\n}\n" "coproc worker {\n  echo x\n}\n")
                   ("cat <(\necho x\n)\n" "cat <(\n  echo x\n)\n")
                   ("value=${\necho x\n}\n" "value=${\n  echo x\n}\n")
                   ("value=${|\nREPLY=x\n}\n" "value=${|\n  REPLY=x\n}\n")
                   ("[[\n$x == y\n]]\n" "[[\n  $x == y\n]]\n")
                   ("((\nx + 1\n))\n" "((\n  x + 1\n))\n")))
    (ert-info ((format "%S" source))
      (should (equal (bash-ts-mode-test--indent source) expected)))))

;;;; Updates

(ert-deftest bash-ts-mode-reparses-old-recovery-even-for-distant-edits ()
  (pcase-dolist (`(,language ,properties)
                 '((bash "")
                   (bash_extglob "extglob = true\n")))
    (let ((treesit-font-lock-level 4))
      (bash-ts-mode-test--with-script properties "f() { if true; then :; }\ng() { :; }\n"
        (bash-ts-mode-test--buffer-state)
        (let* ((parser treesit-primary-parser)
               (notifiers (treesit-parser-notifiers parser)))
          (should (treesit-node-check (treesit-parser-root-node parser) 'has-error))
          (treesit-parser-set-embed-level parser 3)
          (goto-char (point-min))
          (search-forward "g")
          (save-restriction
            (narrow-to-region (line-beginning-position) (line-end-position))
            (insert "x"))
          (should-not (eq parser treesit-primary-parser))
          (should (eq (treesit-parser-language treesit-primary-parser) language))
          (should (= (treesit-parser-embed-level treesit-primary-parser) 3))
          (should (equal (treesit-parser-notifiers treesit-primary-parser) notifiers))
          (should (equal (treesit-parser-list) (list treesit-primary-parser))))
        (goto-char (point-min))
        (search-forward "}")
        (backward-char 1)
        (insert "fi; ")
        (should-not (treesit-node-check
                     (treesit-parser-root-node treesit-primary-parser) 'has-error))
        (bash-ts-mode-test--should-match-fresh-buffer 4 t)))))

(ert-deftest bash-ts-mode-refreshes-edited-owners-without-parser-notifications ()
  (pcase-dolist (`(,language ,properties ,source ,fragment ,owner-start)
                 '((bash "" "f() { printf \"%s\\n\" ok; }\n" "ok" 1)
                   (bash_extglob "extglob = true\n" "f() { printf \"%s\\n\" ok; }\n" "ok" 1)))
    (dolist (narrow '(nil t))
      (ert-info ((format "%s, narrowed: %s" language narrow))
        (let ((treesit-font-lock-level 4))
          (bash-ts-mode-test--with-script properties source
            (rename-buffer "bash-ts-mode-test" t)
            (let ((noninteractive nil)) (font-lock-mode 1))
            (bash-ts-mode-test--buffer-state)
            (should (get-text-property owner-start 'fontified))
            (let ((parser treesit-primary-parser))
              (should-not (treesit-node-check (treesit-parser-root-node parser) 'has-error))
              (dolist (notifier (treesit-parser-notifiers parser))
                (treesit-parser-remove-notifier parser notifier))
              (goto-char (bash-ts-mode-test--position fragment))
              (when narrow
                (narrow-to-region (line-beginning-position) (line-end-position)))
              (insert "x")
              (widen)
              (should (eq parser treesit-primary-parser))
              (should-not (get-text-property owner-start 'fontified))
              (bash-ts-mode-test--should-match-fresh-buffer 4 t))))))))

(ert-deftest bash-ts-mode-reparses-a-bom-moved-to-buffer-start ()
  (pcase-dolist (`(,language ,properties ,prefix ,body ,fragment ,face)
                 '((bash "extglob = false\n" "echo first\n" "# note\n" "note" font-lock-comment-face)
                   (bash_extglob "extglob = true\n" "echo first\n" "# note\n" "note" font-lock-comment-face)))
    (dolist (narrow '(nil t))
      (ert-info ((format "%s, narrowed: %s" language narrow))
        (let ((treesit-font-lock-level 4))
          (bash-ts-mode-test--with-script properties (concat prefix "\ufeff" body)
            (bash-ts-mode-test--buffer-state)
            (when narrow (narrow-to-region 1 (+ 2 (length prefix))))
            (delete-region 1 (1+ (length prefix)))
            (widen)
            (should (eq (treesit-parser-language treesit-primary-parser) language))
            (should (equal (treesit-parser-list) (list treesit-primary-parser)))
            (should (equal (buffer-substring-no-properties (point-min) (point-max))
                           (concat "\ufeff" body)))
            (bash-ts-mode-test--buffer-state)
            (should-not (get-text-property 1 'face))
            (goto-char (point-min))
            (search-forward fragment)
            (should (eq (get-text-property (1- (point)) 'face) face))
            (bash-ts-mode-test--should-match-fresh-buffer 4 t)))))))

(ert-deftest bash-ts-mode-updates-neutral-candidates-like-fresh-parsing ()
  (dolist (properties '("" "extglob = true\n"))
    (pcase-dolist (`(,source ,old ,replacement)
                   '((": $[a[(]]\n" "(" "1+2")
                     ("echo ${a[(i+1)]}\n" "i+1" "i+")
                     ("echo ${a[(i+1)]}\n" "(i+1)" "(i+1")
                     ("echo ${a[(]}\n" "]" "")
                     ("echo ${a[(}\n" "(}" "(]}")
                     ("echo ${a[i+1]}\n" "i+1" "$(printf x)+2")
                     ("echo ${a[i+1]}\n" "i+1" "\"i+1\"")
                     ("echo ${a[i+1]}\n" "i+1" "b[i+1]")
                     ("echo ${x/a/pre&post}\n" "&" "\\&")
                     ("echo ${x/a/pre&post}\n" "pre&post" "\"pre&post\"")))
      (let ((treesit-font-lock-level 4))
        (bash-ts-mode-test--with-script properties source
          (bash-ts-mode-test--buffer-state)
          (goto-char (bash-ts-mode-test--position old))
          (delete-char (length old))
          (insert replacement)
          (bash-ts-mode-test--should-match-fresh-buffer 4))))))

(ert-deftest bash-ts-mode-refreshes-syntax-before-multiline-errors ()
  (dolist (narrow '(nil t))
    (with-temp-buffer
      (insert "f() {\n  echo ok\n}\n")
      (let ((treesit-font-lock-level 4)) (bash-ts-mode))
      (syntax-propertize (point-max))
      (should (= (syntax-class (syntax-after 2)) 4))
      (goto-char (point-min))
      (search-forward "}")
      (backward-char 1)
      (let ((position (point)) (removed (char-after)))
        (save-restriction
          (when narrow (narrow-to-region (line-beginning-position) (point-max)))
          (delete-char 1)
          (syntax-propertize (point-max)))
        (bash-ts-mode-test--should-match-fresh-buffer 4)
        (should (= (syntax-class (syntax-after 2)) 1))
        (goto-char position)
        (insert removed)
        (bash-ts-mode-test--should-match-fresh-buffer 4)
        (should (= (syntax-class (syntax-after 2)) 4))))))

(ert-deftest bash-ts-mode-recomputes-comment-syntax-after-edits ()
  (with-temp-buffer
    (insert "printf value#suffix\n")
    (bash-ts-mode)
    (should-not
     (bash-ts-mode-test--comment-p "suffix" nil "printf value#suffix"))
    (goto-char
     (bash-ts-mode-test--position "#" "printf value#suffix"))
    (insert " ")
    (should
     (bash-ts-mode-test--comment-p "suffix" nil "printf value #suffix"))
    (goto-char
     (bash-ts-mode-test--position " #" "printf value #suffix"))
    (delete-char 1)
    (should-not
     (bash-ts-mode-test--comment-p "suffix" nil "printf value#suffix")))
  (with-temp-buffer
    (insert "echo \"`a # head\nbody\n`\"\n: tail\n")
    (let ((treesit-font-lock-level 4)) (bash-ts-mode))
    (dolist (continued '(t nil t))
      (goto-char (point-min))
      (search-forward "# head")
      (if continued (insert "\\") (delete-char 1))
      (narrow-to-region
       (bash-ts-mode-test--position "body" "body") (point-max))
      (syntax-propertize (point-max))
      (widen)
      (should (eq (bash-ts-mode-test--comment-p "body" nil "body") continued))
      (should (bash-ts-mode-test--comment-p "head" nil (if continued "echo \"`a # head\\" "echo \"`a # head")))
      (should-not (bash-ts-mode-test--comment-p "tail" nil ": tail"))
      (font-lock-flush)
      (font-lock-ensure)
      (bash-ts-mode-test--should-match-fresh-buffer 4))))

(ert-deftest bash-ts-mode-updates-pattern-context-after-edits ()
  (with-temp-buffer
    (let ((pattern-line "value=${x##*}")
          (value-line "value=${x:-*}"))
      (insert pattern-line "\n")
      (let ((treesit-font-lock-level 4)) (bash-ts-mode))
      (font-lock-ensure)
      (should (eq (bash-ts-mode-test--face "*" nil pattern-line)
                  'font-lock-constant-face))
      (goto-char (point-min))
      (search-forward "##")
      (replace-match ":-")
      (font-lock-flush (point-min) (point-max))
      (font-lock-ensure)
      (should (eq (bash-ts-mode-test--face "*" nil value-line)
                  'font-lock-string-face)))))

(ert-deftest bash-ts-mode-reclassifies-backquote-words-after-enclosing-quote-edits ()
  (with-temp-buffer
    (insert "echo `printf %s \\\"a b\\\"`\n")
    (let ((treesit-font-lock-level 4)) (bash-ts-mode))
    (dolist (quoted '(nil t nil))
      (goto-char 6)
      (when (eq (char-after) ?\")
        (delete-char 1)
        (goto-char (1- (point-max)))
        (delete-char -1))
      (when quoted
        (goto-char 6)
        (insert "\"")
        (goto-char (1- (point-max)))
        (insert "\""))
      (font-lock-flush)
      (font-lock-ensure)
      (goto-char (point-min))
      (search-forward "\\")
      (should (eq (get-text-property (1- (point)) 'face)
                  (unless quoted 'font-lock-escape-face)))
      (should (eq (get-text-property (point) 'face)
                  (if quoted 'font-lock-string-face 'font-lock-escape-face)))
      (search-forward "a b")
      (should (eq (get-text-property (- (point) 2) 'face)
                  (and quoted 'font-lock-string-face)))
      (bash-ts-mode-test--should-match-fresh-buffer 4))))

(ert-deftest bash-ts-mode-fontifies-continued-constructs-after-completion ()
  (pcase-dolist (`(,incomplete ,completion)
                 '(("i\\\nf\n" "true; then :; fi\n")
                   ("w\\\nhile\n" "true; do :; done\n")
                   ("f\\\nor item in one\n" "do :; done\n")
                   ("c\\\nase x in\nx)" " :;; esac\n")
                   ("f\\\n() {\n" ":;\n}\n")
                   ("e\\\ncho inner |\n" "cat\n")
                   ("e\\\ncho ${value:-" "inner}\n")
                   ("e\\\ncho $(" "echo inner)\n")))
    (with-temp-buffer
      (insert "#!/bin/sh\n\nprintf before\n\n")
      (let ((treesit-font-lock-level 4)) (bash-ts-mode))
      (font-lock-ensure)
      (let ((start (point)))
        (insert incomplete)
        (font-lock-ensure)
        (insert completion)
        (font-lock-ensure)
        (should-not (treesit-node-check (treesit-parser-root-node treesit-primary-parser) 'has-error))
        (bash-ts-mode-test--should-match-fresh-buffer 4)
        (goto-char start)
        (search-forward "\\")
        (should (eq (get-text-property (1- (point)) 'face)
                    'font-lock-punctuation-face))
        (delete-region start (point-max))
        (font-lock-ensure)
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       "#!/bin/sh\n\nprintf before\n\n"))
        (bash-ts-mode-test--should-match-fresh-buffer 4)))))

(ert-deftest bash-ts-mode-refreshes-ere-and-array-source-after-edits ()
  (pcase-dolist (`(,source ,old ,replacement)
                 '(("[[ $x =~ ^(ab|[0-9])+$ ]]\n" "[0-9]" "[[:alpha:]]")
                   ("[[ $x =~ ^\"ab\"$ ]]\n" "\"ab\"" "${value}")
                   ("values=([key]=one)\n" "key" "${name}")
                   ("echo ${value/a/b}\n" "a" "[[:digit:]]")))
    (with-temp-buffer
      (insert source)
      (let ((treesit-font-lock-level 4)) (bash-ts-mode))
      (bash-ts-mode-test--buffer-state)
      (goto-char (point-min))
      (search-forward old)
      (replace-match replacement t t)
      (bash-ts-mode-test--should-match-fresh-buffer 4))))

(provide 'bash-ts-mode-test)

;;; bash-ts-mode-test.el ends here
