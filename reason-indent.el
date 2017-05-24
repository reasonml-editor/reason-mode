;;; reason-mode.el --- A major emacs mode for editing Reason (based on rust-mode) -*-lexical-binding: t-*-
;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;;; Commentary:

;; Indentation functions for Reason.

;;; Code:

(defcustom reason-indent-offset 2
  "Indent Reason code by this number of spaces."
  :type 'integer
  :group 'reason-mode
  :safe #'integerp)

(defun reason-looking-back-str (str)
  "Like `looking-back' but for fixed strings rather than regexps (so that it's not so slow)"
  (let ((len (length str)))
    (and (> (point) len)
         (equal str (buffer-substring-no-properties (- (point) len) (point))))))

(defun reason-paren-level () (nth 0 (syntax-ppss)))
(defun reason-in-str-or-cmnt () (nth 8 (syntax-ppss)))
(defun reason-rewind-past-str-cmnt () (goto-char (nth 8 (syntax-ppss))))
(defun reason-rewind-irrelevant ()
  (interactive)
  (let ((starting (point)))
    (skip-chars-backward "[:space:]\n")
    (if (reason-looking-back-str "*/") (backward-char))
    (if (reason-in-str-or-cmnt)
        (reason-rewind-past-str-cmnt))
    (if (/= starting (point))
        (reason-rewind-irrelevant))))

(defun reason-align-to-expr-after-brace ()
  (save-excursion
    (forward-char)
    ;; We don't want to indent out to the open bracket if the
    ;; open bracket ends the line
    (when (not (looking-at "[[:blank:]]*\\(?://.*\\)?$"))
      (when (looking-at "[[:space:]]")
        (forward-word 1)
        (backward-word 1))
      (current-column))))


;;; Start of a reason binding
(defvar reason-binding
  (regexp-opt '("let" "type")))

(defun reason-beginning-of-defun (&optional arg)
  "Move backward to the beginning of the current defun.

With ARG, move backward multiple defuns.  Negative ARG means
move forward.

This is written mainly to be used as `beginning-of-defun-function'.
Don't move to the beginning of the line. `beginning-of-defun',
which calls this, does that afterwards."
  (interactive "p")
  (re-search-backward (concat "^\\(" reason-binding "\\)\\_>")
                      nil 'move (or arg 1)))

(defun reason-end-of-defun ()
  "Move forward to the next end of defun.

With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

Assume that this is called after beginning-of-defun. So point is
at the beginning of the defun body.

This is written mainly to be used as `end-of-defun-function' for Reason."
  (interactive)
  ;; Find the opening brace
  (if (re-search-forward "[{]" nil t)
      (progn
        (goto-char (match-beginning 0))
        ;; Go to the closing brace
        (condition-case nil
            (forward-sexp)
          (scan-error
           ;; The parentheses are unbalanced; instead of being unable to fontify, just jump to the end of the buffer
           (goto-char (point-max)))))
    ;; There is no opening brace, so consider the whole buffer to be one "defun"
    (goto-char (point-max))))


(defun reason-rewind-to-beginning-of-current-level-expr ()
  (interactive)
  (let ((current-level (reason-paren-level)))
    (back-to-indentation)
    (when (looking-at "->")
      (reason-rewind-irrelevant)
      (back-to-indentation))
    (while (> (reason-paren-level) current-level)
      (backward-up-list)
      (back-to-indentation))
    ;; When we're in the where clause, skip over it.  First find out the start
    ;; of the function and its paren level.
    (let ((function-start nil) (function-level nil))
      (save-excursion
        (reason-beginning-of-defun)
        (back-to-indentation)
        ;; Avoid using multiple-value-bind
        (setq function-start (point)
              function-level (reason-paren-level)))
      ;; On a where clause
      (when (or (looking-at "\\bwhere\\b")
                ;; or in one of the following lines, e.g.
                ;; where A: Eq
                ;;       B: Hash <- on this line
                (and (save-excursion
                       (re-search-backward "\\bwhere\\b" function-start t))
                     (= current-level function-level)))
        (goto-char function-start)))))



(defun reason-mode-indent-line ()
  (interactive)
  (let ((indent
         (save-excursion
           (back-to-indentation)
           ;; Point is now at beginning of current line
           (let* ((level (reason-paren-level))
                  (baseline
                   ;; Our "baseline" is one level out from the indentation of the expression
                   ;; containing the innermost enclosing opening bracket. That
                   ;; way if we are within a block that has a different
                   ;; indentation than this mode would give it, we still indent
                   ;; the inside of it correctly relative to the outside.
                   (if (= 0 level)
                       0
                      (save-excursion
                        (reason-rewind-irrelevant)
                        ;; (backward-up-list)
                        (reason-rewind-to-beginning-of-current-level-expr)

                        (if (or (looking-at "|") (looking-at "switch"))
                            (current-column)
                          (+ (current-column) reason-indent-offset))

                        ))))
             (cond
              ;; A function return type is indented to the corresponding function arguments
              ((looking-at "->")
               (save-excursion
                 (backward-list)
                 (or (reason-align-to-expr-after-brace)
                     (+ baseline reason-indent-offset))))

              ((reason-in-str-or-cmnt)
               (cond
                ;; In the end of the block -- align with star
                ((looking-at "*/") (+ baseline 1))
                ;; Indent to the following shape:
                ;; /* abcd
                ;;  * asdf
                ;;  */
                ;;
                ((looking-at "*") (+ baseline 1))
                ;; Indent to the following shape:
                ;; /* abcd
                ;;    asdf
                ;;  */
                ;;
                (t (+ baseline (+ reason-indent-offset 1)))))

              ;; A closing brace is 1 level unindented
              ((looking-at "}\\|)\\|\\]") (- baseline reason-indent-offset))

              ;; Doc comments in /** style with leading * indent to line up the *s
              ((and (nth 4 (syntax-ppss)) (looking-at "*"))
               (+ 1 baseline))

              ;; If we're in any other token-tree / sexp, then:
              (t
               (or
                ;; If we are inside a pair of braces, with something after the
                ;; open brace on the same line and ending with a comma, treat
                ;; it as fields and align them.
                (when (> level 0)
                  (save-excursion
                    (reason-rewind-irrelevant)
                    (backward-up-list)
                    ;; Point is now at the beginning of the containing set of braces
                    (reason-align-to-expr-after-brace)))

                ;; When where-clauses are spread over multiple lines, clauses
                ;; should be aligned on the type parameters.  In this case we
                ;; take care of the second and following clauses (the ones
                ;; that don't start with "where ")
                (save-excursion
                  ;; Find the start of the function, we'll use this to limit
                  ;; our search for "where ".
                  (let ((function-start nil) (function-level nil))
                    (save-excursion
                      (reason-beginning-of-defun)
                      (back-to-indentation)
                      ;; Avoid using multiple-value-bind
                      (setq function-start (point)
                            function-level (reason-paren-level)))
                    ;; When we're not on a line starting with "where ", but
                    ;; still on a where-clause line, go to "where "
                    (when (and
                           (not (looking-at "\\bwhere\\b"))
                           ;; We're looking at something like "F: ..."
                           (and (looking-at (concat reason-re-ident ":"))
                                ;; There is a "where " somewhere after the
                                ;; start of the function.
                                (re-search-backward "\\bwhere\\b"
                                                    function-start t)
                                ;; Make sure we're not inside the function
                                ;; already (e.g. initializing a struct) by
                                ;; checking we are the same level.
                                (= function-level level)))
                      ;; skip over "where"
                      (forward-char 5)
                      ;; Unless "where" is at the end of the line
                      (if (eolp)
                          ;; in this case the type parameters bounds are just
                          ;; indented once
                          (+ baseline reason-indent-offset)
                        ;; otherwise, skip over whitespace,
                        (skip-chars-forward "[:space:]")
                        ;; get the column of the type parameter and use that
                        ;; as indentation offset
                        (current-column)))))

                (progn
                  (back-to-indentation)
                  (cond ((looking-at (regexp-opt '("constraint" "and" "type")))
                         baseline)
                        ((save-excursion
                         (reason-rewind-irrelevant)
                         (= (point) 1))
                         baseline)
                        ((save-excursion
                           (while (looking-at "|")
                             (reason-rewind-irrelevant)
                             (back-to-indentation))
                           (looking-at (regexp-opt '("type")))
                           )
                         (+ baseline reason-indent-offset))
                        ((looking-at "|\\|/[/*]")
                         baseline)
                        ((save-excursion
                         (reason-rewind-irrelevant)
                         (looking-back "[{;,\\[(]" (- (point) 2)))
                         baseline)
                        (t
                         (+ baseline reason-indent-offset)
                         )
                        )
                  ;; Point is now at the beginning of the current line
                  ))))))))

    (when indent
      ;; If we're at the beginning of the line (before or at the current
      ;; indentation), jump with the indentation change.  Otherwise, save the
      ;; excursion so that adding the indentations will leave us at the
      ;; equivalent position within the line to where we were before.
      (if (<= (current-column) (current-indentation))
          (indent-line-to indent)
        (save-excursion (indent-line-to indent))))))

(provide 'reason-indent)

;;; reason-indent.el ends here
