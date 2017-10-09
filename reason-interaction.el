;;; reason-interaction.el --- Phrase navitagion for rtop -*-lexical-binding: t-*-

;; Portions Copyright (c) 2015-present, Facebook, Inc. All rights reserved.

;;; Commentary:

;; Phrase navigation for utop and maybe other REPLs.

;; The utop compatibility layer for Reason was mainly taken from:
;; https://github.com/ocaml/tuareg/blob/master/tuareg-light.el (big thanks!)

;;; Code:

(defun reason-backward-char (&optional step)
  "Go back one char.
Similar to `backward-char` but it does not signal errors
`beginning-of-buffer` and `end-of-buffer`.  It optionally takes a
STEP parameter for jumping back more than one character."
  (when step (goto-char (- (point) step))
        (goto-char (1- (point)))))

(defun reason-forward-char (&optional step)
  "Go forward one char.
Similar to `forward-char` but it does not signal errors
`beginning-of-buffer` and `end-of-buffer`.  It optionally takes a
STEP parameter for jumping back more than one character."
  (when step (goto-char (+ (point) step))
    (goto-char (1+ (point)))))

(defun reason-in-literal-p ()
  "Return non-nil if point is inside an Reason literal."
  (nth 3 (syntax-ppss)))

(defconst reason-comment-delimiter-regexp "\\*/\\|/\\*"
  "Regex for identify either open or close comment delimiters.")

(defun reason-in-between-comment-chars-p ()
  "Return non-nil iff point is in between the comment delimiter chars.
It returns non-nil if point is between the chars only (*|/ or /|*
where | is point)."
  (and (not (bobp)) (not (eobp))
       (or (and (char-equal ?/ (char-before)) (char-equal ?* (char-after)))
           (and (char-equal ?* (char-before)) (char-equal ?/ (char-after))))))

(defun reason-looking-at-comment-delimiters-p ()
  "Return non-nil iff point in between comment delimiters."
  (looking-at-p reason-comment-delimiter-regexp))

(defun reason-in-between-comment-delimiters-p ()
  "Return non-nil if inside /* and */."
  (nth 4 (syntax-ppss)))

(defun reason-in-comment-p ()
  "Return non-nil iff point is inside or right before a comment."
  (or (reason-in-between-comment-delimiters-p)
      (reason-in-between-comment-chars-p)
      (reason-looking-at-comment-delimiters-p)))

(defun reason-beginning-of-literal-or-comment ()
  "Skip to the beginning of the current literal or comment (or buffer)."
  (interactive)
  (goto-char (or (nth 8 (syntax-ppss)) (point))))

(defun reason-inside-block-scope-p ()
  "Skip to the beginning of the current literal or comment (or buffer)."
  (and (> (nth 0 (syntax-ppss)) 0)
       (let ((delim-start (nth 1 (syntax-ppss))))
         (save-excursion
           (goto-char delim-start)
           (char-equal ?{ (following-char))))))

(defun reason-at-phrase-break-p ()
  "Is the underlying `;' a phrase break?"
  ;; Difference from OCaml, the phrase separator is a single semi-colon
  (and (not (eobp))
       (char-equal ?\; (following-char))))

(defun reason-skip-to-close-delimiter (&optional limit)
  "Skip to the end of a Reason block.
It basically calls `re-search-forward` in order to go to any
closing delimiter, not concerning itself with balancing of any
sort.  Client code needs to check that.
LIMIT is passed to `re-search-forward` directly."
  (re-search-forward "\\s)" limit 'move))

(defun reason-skip-back-to-open-delimiter (&optional limit)
  "Skip to the beginning of a Reason block backwards.
It basically calls `re-search-backward` in order to go to any
opening delimiter, not concerning itself with balancing of any
sort.  Client code needs to check that.
LIMIT is passed to `re-search-backward` directly."
  (re-search-backward "\\s(" limit 'move))

(defun reason-find-phrase-end ()
  "Skip to the end of a phrase."
  (while (and (not (eobp))
              (not (reason-at-phrase-break-p)))
    (if (re-search-forward ";" nil 'move)
        (progn (when (reason-inside-block-scope-p)
                 (reason-skip-to-close-delimiter))
               (goto-char (1- (point))))
      ;; avoid infinite loop at the end of the buffer
      (re-search-forward "[[:space:]\\|\n]+" nil 'move)))
  (min (goto-char (1+ (point))) (point-max)))

(defun reason-skip-blank-and-comments ()
  "Skip blank spaces and comments."
  (cond
   ((eobp) (point))
   ((or (reason-in-between-comment-chars-p)
        (reason-looking-at-comment-delimiters-p)) (progn
                                                    (reason-forward-char 1)
                                                    (reason-skip-blank-and-comments)))
   ((reason-in-between-comment-delimiters-p) (progn
                                               (search-forward "*/" nil t)
                                               (reason-skip-blank-and-comments)))
   ((eolp) (progn
             (reason-forward-char 1)
             (reason-skip-blank-and-comments)))
   (t (progn (skip-syntax-forward " ")
             (point)))))

(defun reason-skip-back-blank-and-comments ()
  "Skip blank spaces and comments backwards."
  (cond
   ((bobp) (point))
   ((looking-back reason-comment-delimiter-regexp) (progn
                                                     (reason-backward-char 1)
                                                     (reason-skip-back-blank-and-comments)))
   ((reason-in-between-comment-delimiters-p) (progn
                                               (search-backward "/*" nil t)
                                               (reason-backward-char 1)
                                               (reason-skip-back-blank-and-comments)))
   ((or (reason-in-between-comment-chars-p)
        (reason-looking-at-comment-delimiters-p)) (progn
                                                    (reason-backward-char 1)
                                                    (reason-skip-back-blank-and-comments)))
   ((bolp) (progn
             (reason-backward-char 1)
             (reason-skip-back-blank-and-comments)))
   (t (progn (skip-syntax-backward " ")
             (point)))))

(defun reason-ro (&rest words)
  "Build a regex matching iff at least a word in WORDS is present."
  (concat "\\<" (regexp-opt words t) "\\>"))

(defconst reason-find-phrase-beginning-regexp
  (concat (reason-ro "end" "type" "module" "sig" "struct" "class"
                     "exception" "open" "let")
          "\\|^#[ \t]*[a-z][_a-z]*\\>\\|;"))

(defun reason-at-phrase-start-p ()
  "Return t if is looking at the beginning of a phrase.
A phrase starts when a toplevel keyword is at the beginning of a line."
  (or (looking-at "#")
      (looking-at reason-find-phrase-beginning-regexp)))

(defun reason-find-phrase-beginning-backward ()
  "Find the beginning of a phrase and return point.
It scans code backwards, therefore the caller can assume that the
beginning of the phrase (if found) is always before the starting
point.  No error is signalled and (point-min) is returned when a
phrease cannot be found."
  (beginning-of-line)
  (while (and (not (bobp)) (not (reason-at-phrase-start-p)))
    (if (reason-inside-block-scope-p)
        (reason-skip-back-to-open-delimiter)
      (re-search-backward reason-find-phrase-beginning-regexp nil 'move)))
  (point))

(defun reason-discover-phrase ()
  "Discover a Reason phrase in the buffer."
  ;; TODO reason-with-internal-syntax ;; tuareg2 modifies the syntax table (removed for now)
  ;; TODO stop-at-and feature for phrase detection (do we need it?)
  ;; TODO tuareg2 has some custom logic for module and class (do we need it?)
  (save-excursion
    (let ((case-fold-search nil))
      (reason-skip-blank-and-comments)
      (list (reason-find-phrase-beginning-backward) ;; beginning
            (reason-find-phrase-end)                ;; end
            (save-excursion                         ;; end-with-comment
              (reason-skip-blank-and-comments)
              (point))))))

(defun reason-discover-phrase-debug ()
  "Discover a Reason phrase in the buffer (debug mode)."
  (let ((triple (reason-discover-phrase)))
    (message (concat "Evaluating: \"" (reason-fetch-phrase triple) "\""))
    triple))

(defun reason-fetch-phrase (triple)
  "Fetch the phrase text given a TRIPLE."
  (let* ((start (nth 0 triple))
         (end (nth 1 triple))) ;; we don't need end-with-comment
    (buffer-substring-no-properties start end)))

(defun reason-next-phrase ()
  "Skip to the beginning of the next phrase."
  (cond
   ((reason-at-phrase-start-p) (point))
   ((eolp) (progn
             (forward-char 1)
             (reason-skip-blank-and-comments)
             (reason-next-phrase)))
   ((reason-inside-block-scope-p) (progn (reason-skip-to-close-delimiter)
                                         (reason-next-phrase)))
   ((looking-at ";") (progn
                       (forward-char 1)
                       (reason-next-phrase)))
   (t (progn (end-of-line)
             (reason-next-phrase)))))

(provide 'reason-interaction)

;;; reason-interaction.el ends here
