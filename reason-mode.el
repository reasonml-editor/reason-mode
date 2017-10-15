;;; reason-mode.el --- A major mode for editing ReasonML -*-lexical-binding: t-*-
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;; Version: 0.4.0
;; Author: Mozilla
;; Url: https://github.com/reasonml-editor/reason-mode
;; Keywords: languages, ocaml
;; Package-Requires: ((emacs "24.3"))

;; This file is NOT part of GNU Emacs.

;; This file is distributed under the terms of both the MIT license and the
;; Apache License (version 2.0).

;;; Commentary:
;; This project provides useful functions and helpers for developing code
;; using the Reason programming language (https://facebook.github.io/reason).
;;
;; Reason is an umbrella project that provides a curated layer for OCaml.
;;
;; It offers:
;;  - A new, familiar syntax for the battle-tested language that is OCaml.
;;  - A workflow for compiling to JavaScript and native code.
;;  - A set of friendly documentations, libraries and utilities.
;;
;; See the README.md for more details.

;;; Code:

(require 'reason-indent)
(require 'refmt)
(require 'reason-interaction)

(eval-when-compile (require 'rx)
                   (require 'compile)
                   (require 'url-vars))

;; Syntax definitions and helpers
(defvar reason-mode-syntax-table
  (let ((table (make-syntax-table)))

    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?& ?| ?^ ?! ?< ?> ?~ ?@))
      (modify-syntax-entry i "." table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" table)
    (modify-syntax-entry ?\\ "\\" table)
    (modify-syntax-entry ?\' "_"  table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" table)
    (modify-syntax-entry ?*  ". 23n"  table)
    (modify-syntax-entry ?\n "> b"    table)
    (modify-syntax-entry ?\^m "> b"   table)

    table))

(defgroup reason nil
  "Support for Reason code."
  :link '(url-link "http://facebook.github.io/reason/")
  :group 'languages)

(defcustom reason-mode-hook nil
  "Hook called by `reason-mode'."
  :type 'hook
  :group 'reason)

;; Font-locking definitions and helpers
(defconst reason-mode-keywords
  '("and" "as"
    "else" "external"
    "fun" "for"
    "if" "impl" "in" "include"
    "let"
    "module" "match" "mod" "move" "mutable"
    "open"
    "priv" "pub"
    "rec" "ref" "return"
    "self" "static" "switch" "struct" "super"
    "trait" "type"
    "use"
    "virtual"
    "where" "when" "while"))

(defconst reason-mode-consts
  '("true" "false"))

(defconst reason-special-types
  '("int" "float" "string" "char"
    "bool" "unit" "list" "array" "exn"
    "option" "ref"))

(defconst reason-camel-case
  (rx symbol-start
      (group upper (0+ (any word nonascii digit "_")))
      symbol-end))

(eval-and-compile
  (defconst reason--char-literal-rx
    (rx (seq (group "'")
             (or (seq "\\" anything)
                 (not (any "'\\")))
             (group "'")))))

(defun reason-re-word (inner)
  "Build a word regexp given INNER."
  (concat "\\<" inner "\\>"))

(defun reason-re-grab (inner)
  "Build a grab regexp given INNER."
  (concat "\\(" inner "\\)"))

(defun reason-regexp-opt-symbols (words)
  "Like `(regexp-opt words 'symbols)`, but will work on Emacs 23.
See rust-mode PR #42.
Argument WORDS argument to pass to `regexp-opt`."
  (concat "\\_<" (regexp-opt words t) "\\_>"))

;;; Syntax highlighting for Reason
(defvar reason-font-lock-keywords
  `((,(reason-regexp-opt-symbols reason-mode-keywords) . font-lock-keyword-face)
    (,(reason-regexp-opt-symbols reason-special-types) . font-lock-builtin-face)
    (,(reason-regexp-opt-symbols reason-mode-consts) . font-lock-constant-face)

    (,reason-camel-case 1 font-lock-type-face)

    ;; Field names like `foo:`, highlight excluding the :
    (,(concat (reason-re-grab reason-re-ident) ":[^:]") 1 font-lock-variable-name-face)
    ;; Module names like `foo::`, highlight including the ::
    (,(reason-re-grab (concat reason-re-ident "::")) 1 font-lock-type-face)
    ;; Name punned labeled args like ::foo
    (,(concat "[[:space:]]+" (reason-re-grab (concat "::" reason-re-ident))) 1 font-lock-type-face)

    ;; TODO jsx attribs?
    (,
     (concat "<[/]?" (reason-re-grab reason-re-ident) "[^>]*" ">")
     1 font-lock-type-face)))

(defun reason-mode-try-find-alternate-file (mod-name extension)
  "Switch to the file given by MOD-NAME and EXTENSION."
  (let* ((filename (concat mod-name extension))
         (buffer (get-file-buffer filename)))
    (if buffer (switch-to-buffer buffer)
      (find-file filename))))

(defun reason-mode-find-alternate-file ()
  "Switch to implementation/interface file."
  (interactive)
  (let ((name buffer-file-name))
    (when (string-match "\\`\\(.*\\)\\.re\\([il]\\)?\\'" name)
      (let ((mod-name (match-string 1 name))
            (e (match-string 2 name)))
        (cond
         ((string= e "i")
          (reason-mode-try-find-alternate-file mod-name ".re"))
         (t
          (reason-mode-try-find-alternate-file mod-name ".rei")))))))

(defun reason--syntax-propertize-multiline-string (end)
  "Propertize Reason multiline string.
Argument END marks the end of the string."
  (let ((ppss (syntax-ppss)))
    (when (eq t (nth 3 ppss))
      (let ((key (save-excursion
                   (goto-char (nth 8 ppss))
                   (and (looking-at "{\\([a-z]*\\)|")
                        (match-string 1)))))
        (when (search-forward (format "|%s}" key) end 'move)
          (put-text-property (1- (match-end 0)) (match-end 0)
                             'syntax-table (string-to-syntax "|")))))))

(defun reason-syntax-propertize-function (start end)
  "Propertize Reason function.
Argument START marks the beginning of the function.
Argument END marks the end of the function."
  (goto-char start)
  (reason--syntax-propertize-multiline-string end)
  (funcall
   (syntax-propertize-rules
    (reason--char-literal-rx (1 "\"") (2 "\""))
    ;; multi line strings
    ("\\({\\)[a-z]*|"
     (1 (prog1 "|"
          (goto-char (match-end 0))
          (reason--syntax-propertize-multiline-string end)))))
   (point) end))

(defvar reason-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-a" #'reason-mode-find-alternate-file)
    (define-key map "\C-c\C-r" #'refmt-region-ocaml-to-reason)
    (define-key map "\C-c\C-o" #'refmt-region-reason-to-ocaml)
    map))

;;;###autoload
(define-derived-mode reason-mode prog-mode "Reason"
  "Major mode for Reason code.

\\{reason-mode-map}"
  :group 'reason
  :syntax-table reason-mode-syntax-table
  :keymap reason-mode-map

  ;; Syntax
  (setq-local syntax-propertize-function #'reason-syntax-propertize-function)
  ;; Indentation
  (setq-local indent-line-function 'reason-mode-indent-line)
  ;; Fonts
  (setq-local font-lock-defaults '(reason-font-lock-keywords))
  ;; Misc
  (setq-local comment-start "/*")
  (setq-local comment-end   "*/")
  (setq-local indent-tabs-mode nil)
  ;; Allow paragraph fills for comments
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local paragraph-start
              (concat "^[ \t]*$\\|\\*)$\\|" page-delimiter))
  (setq-local paragraph-separate paragraph-start)
  (setq-local require-final-newline t)
  (setq-local normal-auto-fill-function nil)
  (setq-local comment-multi-line t)

  (setq-local beginning-of-defun-function 'reason-beginning-of-defun)
  (setq-local end-of-defun-function 'reason-end-of-defun)
  (setq-local parse-sexp-lookup-properties t))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.rei?\\'" . reason-mode))

(defun reason-mode-reload ()
  "Reload Reason mode."
  (interactive)
  (unload-feature 'reason-mode)
  (unload-feature 'reason-indent)
  (unload-feature 'reason-interaction)
  (require 'reason-mode)
  (reason-mode))

(provide 'reason-mode)

;;; reason-mode.el ends here
