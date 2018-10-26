;;; epoch-mode-el -- major mode for editing input-decks to the EPOCH PIC code.
;;; version 0.1
;;
;; Created with inspiration from the Mode Tutorial on emacswiki:
;; https://www.emacswiki.org/emacs/ModeTutorial
;;
;;
;;
;;
;; (c) 2018, Andréas Sundström
;;

;; Mode hook for use by outside functions
(defvar epoch-mode-hook nil)

;;
(defvar epoch-mode-map
  (let ((epoch-mode-map (make-sparse-keymap)))
    ;;(define-key epoch-mode-map "\C-j" 'newline-and-indent)
    (define-key epoch-mode-map "\C-c]" 'epoch-close-block)
    epoch-mode-map)
  "Keymap for EPOCH major mode")

;; 
(add-to-list 'auto-mode-alist '("\\.deck\\'" . epoch-mode))


;;;;;;;;;;;;;;;;;;;; HIGHLIGHTING ;;;;;;;;;;;;;;;;;;;;
(defconst epoch-font-lock-keywords-1
  (list
   ;'("\\(\\(begin:\\|end:\\).*\\)" . font-lock-bultin-face)
   ;; Some of the basic EOPCH blocks have begin:x end:x, where
   ;; x is one of:
   ;; control  boundaries  species  laser  fields window  output  
   ;; output_global  dist_fn probe  collisions  qed  subset  constant
   ;; Output from (regexp-opt '("..." "...")):
   '("\\_<\\(\\(?:begin:\\|end:\\)\\(?:boundaries\\|co\\(?:llisions\\|n\\(?:stant\\|trol\\)\\)\\|dist_fn\\|fields\\|laser\\|output\\(?:_global\\)?\\|probe\\|qed\\|s\\(?:pecies\\|ubset\\)\\|window\\)\\)\\_>" . font-lock-keyword-face)
   ;; True and False are represented by "T" and "F"
   '("\\<\\(?:T\\|F\\)\\>" . font-lock-constant-face)
   )
  "Minimal highlighting expressions for EPOCH mode.")

;; (defconst epoch-font-lock-keywords-2
;;   (append epoch-font-lock-keywords-1
;; 		  (list
;; 		   '("..." . font-lock-keyword-face)
;; 		   '("\\<\\(TRUE\\|FALSE\\)\\>" . font-lock-constant-face)))
;;   "Additional Keywords to highlight in EPOCH mode.")

(defvar epoch-font-lock-keywords epoch-font-lock-keywords-1
  "Default highlighting expressions for EPOCH mode.")



;;;;;;;;;;;;;;;;;;;; INDENTATION ;;;;;;;;;;;;;;;;;;;;
(defvar epoch-indent-offset 3) ; Indentation level

(defun epoch-indent-line ()
  "Indent current line as EPOCH code."
  (interactive)
  (beginning-of-line)
  (if (bobp) ; if at beginning of buffer (bobp)
      (indent-line-to 0) ;First line is always non-indented
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*end:")
	  ;;If the line we are looking at is the end of a block,
	  ;;then set the indentation to the same level as the begin
	  (progn
	    (save-excursion
	      ;;Sets the cur-indent to one indentation less
	      ;;than the previous line (decreasing indentation).
	      (while (and (not (looking-at "^[ \t]*begin:")) (not (bobp)))
		;;(print "Have not found begin:")
		(forward-line -1)
		)
	      ;;(forward-line -1)
	      ;;(setq cur-indent (- (current-indentation) epoch-indent-offset))
	      (setq cur-indent (current-indentation))
	      )
	    (if (< cur-indent 0)
		;;If this is beyond the left margin,
		;;then we go back to 0 indentation.
		(setq cur-indent 0)))
	(save-excursion
	  ;;Else [if we're not loking at an "end:" block]:
	  ;;Iterate backwards until we find an indentation hint:
	  (while not-indented
	    (forward-line -1) 
	    (if (looking-at "^[ \t]*end:")
		;;This hint indicates that we need to indent at the level of the end: token
		(progn
		  ;;Sets the cur-indent to the indentation level of the found "end:" block.
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil)) ;breaking the while loop
	      (if (looking-at "^[ \t]*begin:")
		  ;;This hint indicates that we need to indent an extra level
		  (progn
		    ;;Do the actual indenting:
		    (setq cur-indent (+ (current-indentation) epoch-indent-offset)) 
		    (setq not-indented nil)) ;breaking the while loop
		(if (bobp)
		    ;;Break the backward looking while loop when we've reached the
		    ;;beginning of the buffer (bobp)
		    (setq not-indented nil)))))
	  )
	(save-excursion
	  ;;Check if the previous line (and only that) is continued by a "\".
	  ;;Then we want to indent, but we don't want to indent multiple steps
	  ;;for multiple continuations.
	  (forward-line -1)
	  (when (looking-at "^.*\\\\")
	    ;;When the previos line is continued "\", we want to indent 1 level
	    ;;above the first continued line. Which is why we loop back to find
	    ;;the first continued line
	    (while (and (looking-at "^.*\\\\") (not (bobp)))
	      (forward-line -1)
	      )
	    ;;Go back to the last line visited which was continued.
	    ;;(This is the first line which was continued.)
	    (if (not (bobp))
		(forward-line 1))
	    ;;Indent one level above that line.
	    (setq cur-indent (+ (current-indentation) epoch-indent-offset))
	    ))
	)
      (if cur-indent
	  ;;then
	  (indent-line-to cur-indent)
	;;else
	(indent-line-to 0))
      ))) ; If we didn't see an indentation hint, then allow no indentation

(defun epoch-forward-logical-line (&optional N)
  "Move point like `forward-line' but consider logical lines.
This function takes line continuation through ?\\\\ into account."
  (interactive "p")
  (unless N
    (setq N 1))
  (if (> N 0)
	(while
	    (and
	     (progn
	       ;; Check for continuation line
	       (while (and (let ((line-end (line-end-position)))
			     (save-excursion
			       (< (+ (point) (skip-chars-forward "^\\\\" line-end))
				  line-end)))
			   (= (forward-line) 0)))
	       (= (forward-line) 0))
	       ;; Count down
	     (> (cl-decf N) 0)))
    (forward-line 0)
    (while
	(and
	 (null (bobp))
	 (or
	  ;; Check for continuation line
	  (let ((prev-beg (line-beginning-position 0)))
	    (save-excursion
	      (> (+ (point) (skip-chars-backward "^\\\\" prev-beg))
		 prev-beg)))
	  (and (< N 0)
	       (when (= (forward-line -1) 0)
		 (cl-incf N)))))))
  N)

(defun epoch-logical-line-beginning-position (&optional N)
  "Do the same as `line-beginning-position' but count logical lines."
  (unless N
    (setq N 0))
  (save-excursion
    (epoch-forward-logical-line N)
    (point)))

(defun epoch-logical-line-end-position (&optional N)
  "Do the same as `line-end-position' but count logical lines."
  (save-excursion
    (epoch-forward-logical-line (1- (or N 1)))
    (when (= (epoch-forward-logical-line 1) 0)
      (backward-char))
    (point)))

(defun epoch-logical-line (&optional remove-comments)
  "Return logical line at point as string.
If REMOVE-COMMENTS is non-nil remove line comments
starting with ?# and ?\\\\."
  (let ((line (save-excursion
		(buffer-substring-no-properties
		 (epoch-logical-line-beginning-position)
		 (epoch-logical-line-end-position)))))
    (when remove-comments
      ;; 2nd pass: Remove stretches of #.*$
      (setq line (replace-regexp-in-string "\\\\.*\\(?:\n\\|\\'\\)" "" line)
	    line (replace-regexp-in-string "#.*$" "" line)))
    line))

;;;;;;;;;;;;;;;;;;;; Block closure ;;;;;;;;;;;;;;;;;;;;
(defun epoch-close-block ()
  "Closes code blocks in EPOCH"
  (interactive)
  ;;This is the regex to match
  (let ((block-begin/end-regexp "^\\([[:blank:]]*\\)\\(?:\\(begin\\)\\|\\(end\\)\\):\\([a-z][a-z0-9_]+\\)")
	found
	line
	block-indent
	block-type)
    (save-excursion
      (while (and
	      (setq found (re-search-backward block-begin/end-regexp nil t))
	      (null (string-match block-begin/end-regexp (setq line (epoch-logical-line t)))))))
    (if found
	(progn
	  (when (match-beginning 3)
	    (user-error "Stumbled over block end while searching for block beginning"))
	  (setq block-indent (match-string 1 line)
		block-type (match-string 4 line))
	  ;;inserts the actual end:...
	  ;;(insert (concat "\nend:" block-type))
	  (let ((beg (epoch-logical-line-beginning-position)))
	    (setq line (buffer-substring-no-properties beg (point)))
	    (if (string-match "\\`[[:space:]]*\\'" line)
		(progn
		  (goto-char beg)
		  (insert (concat block-indent "end:" block-type "\n")))
	      (goto-char (epoch-logical-line-end-position))
	      (insert (concat "\n" block-indent "end:" block-type)))))
      (user-error "Fall off the top edge of the world")
      )))

;;;;;;;;;;;;;;;;;;;; Syntax Table ;;;;;;;;;;;;;;;;;;;;

(defvar epoch-mode-syntax-table
  (let((syn-tab (make-syntax-table)))
    ;; # used for comments, newline breaks commets
    (modify-syntax-entry ?# "<" syn-tab)
    (modify-syntax-entry ?\n ">" syn-tab)
    ;; \ breaks lines, everything following is ignored
    ;; (effectively a comments)
    (modify-syntax-entry ?\\ "<" syn-tab)
    syn-tab)
  "Syntax table for epoch-mode")

;;;;;;;;;;;;;;;;;;;; The epoch-mode function ;;;;;;;;;;;;;;;;;;;;
(defun epoch-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map epoch-mode-map)
  (set-syntax-table epoch-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(epoch-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'epoch-indent-line)  
  (setq major-mode 'epoch-mode)
  (setq mode-name "EPOCH")
  (run-hooks 'epoch-mode-hook))

(provide 'epoch-mode)
