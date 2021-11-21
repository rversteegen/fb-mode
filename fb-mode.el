;;; fb-mode.el --- An Emacs major mode for the FreeBASIC programming language

;; Author:     Ralph Versteegen <rbversteegen@gmail.com>
;; Version:    1.1.2
;; Keywords:   languages

;; This software is in the public domain and is provided with absolutely no warranty.

;;; Commentary:

;; Usage:
;; Make sure the directory this file is in is in your load-path, and add
;;  (require 'fb-mode)
;; to your .emacs file.
;; If you use other file extensions for FB files, such as .inc, add them to your
;; auto-mode-alist.
;; You will likely want to set the fb-emph-face to something suitable for your theme,
;; and customise fb-indent-level, the number of spaces to indent by.
;;
;; Available keys:
;;  C-c C-h:  Lookup a symbol/keyword (default at point) in the manual (doesn't handle ambiguous keywords)
;;  F5:       Run `compile', suitably defaulting to fbc, make or scons.
;;  C-M-j:    Split the current line at point (possibly in the middle of a comment or string)
;;  C-c C-f:  Insert a simple FOR loop

;;; Code:

(add-to-list 'auto-mode-alist '("\\.\\(bi\\|bas\\)\\'" . fb-mode))

(eval-when-compile (require 'cl))  ; Using cl-lib doesn't work properly, and I don't know why
(require 'cc-mode)   ; For c-mode-syntax-table

(defcustom fb-indent-level 4  ;c-basic-offset
  "Number of spaces for each indentation step."
  :type 'integer
  :safe 'integerp)

(defcustom fb-max-continuation-indent 20
  "Indentation columns will only be considered if their indentation
  is less than much more than the initial line indent."
  :type 'integer
  :safe 'integerp)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic utility functions


(defun fb-rex (string)
  "Form a regexp by escaping (, | and ) characters, unless prefixed with `.
Also '` ' becomes '\\\\s ' (a whitespace character).
 a|b`( -> a\\\\|b("
  (replace-regexp-in-string
   "` " "\\\\s "
   (replace-regexp-in-string
    "`\\\\\\([(|)]\\)" "\\1"   ; Undo escaping
    (replace-regexp-in-string "[(|)]" "\\\\\\\&" string))))
;(assert (equal (fb-rex "a|b`( ` ") "a\\|b( \\s "))

(defun fb-regexp-join-symbols (words)
  "Return a regexp matching any of a list of symbols, without any numbered groups.
There must be no regex operators in the words!"
  ;(mapconcat (lambda (x) (concat "\\_<" x "\\_>")) words "\\|"))
  ;(concat "\\_<\\(?:" (apply 'join "\\|" words) "\\)\\_>"))
  (concat "\\_<" (regexp-opt words) "\\_>"))

(defun fb-prompt-with-default (prompt default)
  "Prompt for a string, with a default string (possibly nil)"
  (let*
      ((prompt-and-default (if default
                               (concat prompt "(default: " default ") ")
                             prompt))
       (input (read-from-minibuffer prompt-and-default nil nil nil nil default)))
    (if (equal "" input)
        default
      input)))

(defun fb-prompt-with-default-at-point (prompt-text &optional not-regexp)
  "Prompt for a string, with default being the tag at point, if any"
  (fb-prompt-with-default
    prompt-text
    ;; -as-regexp variant used for two reasons:
    ;; firstly, want a regexp, secondly can be overridden with find-tag-default-function
    (if not-regexp (default-at-point) (find-tag-default-as-regexp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq fb-syntax-table (make-syntax-table c-mode-syntax-table))
(modify-syntax-entry ?/ ". 14c" fb-syntax-table) ; punctuation, /' and '/ (comments containing / are type c)
(modify-syntax-entry ?' "< 23" fb-syntax-table)  ; comment start, /' and '/
(modify-syntax-entry ?\n ">" fb-syntax-table)    ; comment end
(modify-syntax-entry ?\\ "_" fb-syntax-table)
(modify-syntax-entry ?# "w" fb-syntax-table)

;; Start of a function or type. Used for syntax highlighting
(defconst fb-toplevel-start
  (fb-rex "\\_<(?:(?:(?:public|private|static)?` *(?:function|sub|constructor|destructor|operator|property))|(?:#` *macro|type|union|enum))\\_>"))

;; TODO: incomplete
;; sub, function, etc are excluded because they're handled elsewhere
(defconst fb-keywords
  (split-string "if then else elseif end with while until wend for step next do loop to scope select case
                 exit continue break return goto gosub resume
                 shared preserve dim redim var const extern static common erase
                 new delete
                 inp asm
                 declare overload explicit virtual abstract extends
                 __function __thiscall threadcall
                 export naked alias cdecl pascal stdcall overload override"))


;(Not used) End of expression due to delimiter, ), comment, or newline
;(setq fb--end-of-var-decl "\\([\n,=']\\|/'\\)")

; comment start or newline : "\\s<\\|\n"


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Indentation


(defun fb--find-continuation-prev-line ()
  "If the previous line ends in _, move after the _, set match data and return non-nil.
Otherwise, returns nil and doesn't move point."
  (let ((save-point (point)))
    ;; Start of previous non-empty/commented line
    (beginning-of-line)
    ;; Go backwards over whitespace, newlines and comments. Doesn't work if inside a comment.
    (save-restriction
      ;; Don't go before the previous line
      (narrow-to-region (line-beginning-position 0) (point))
      (forward-comment -999))
    (skip-syntax-backward " ")
    ;; This will match a line ending with an open string "_ but that isn't valid FB syntax anyway
    ;; (if (> (point) 0)
    ;;     (backward-char))  ; Back over the _ if it's there
    (if (looking-back "\\_<_\\_>")
        (progn
          (backward-char)
          t)
      (goto-char save-point)
      nil)))  ;Failed

(defun fb-back-to-initial-indentation ()
  "Like back-to-indentation, but if the current line is a continuation of a previous one,
move to the real beginning of it (the first non-whitespace char on it)"
  (interactive)
  ;; Repeatedly search just the previous line
  ;(while (re-search-backward "\\_<_\\_>" (line-beginning-position 0) t))
  (while (fb--find-continuation-prev-line))
  (back-to-indentation))

;; These are keywords which create an alignment point if the line is broken by _.
;; For example after "dim preserve foo(10), _" want to align after "preserve" instead of "dim".
(defconst fb--alignable-keywords
      ;;(concat (fb-rex "(\"|'|/')|:|")
      (concat (fb-rex ":|")
              (fb-regexp-join-symbols
               (split-string "then else while until to case shared preserve"))))

(defun fb--indent-column-for-continued-line ()
  "Return a column number to indent to, or nil.
This function only handles the case where the current line is a continuation
of the previous, otherwise it returns nil."
  (save-excursion
    (let ((case-fold-search t)
          (indenting-linenum (line-number-at-pos)))
      ;; Look at last _ line continuation, if any
      (when (fb--find-continuation-prev-line)
        (let ((before-last-continuation (match-beginning 0))
              after-last-line-indent
              (last-point -1)
              best
              initial-linenum
              initial-column)
          (flet
              ((-consider-point ()
                                (if (< (current-column) (+ initial-column fb-max-continuation-indent))
                                    (setq best (point)))))
            ;;(goto-char before-last-continuation)
            (back-to-indentation)
            (setq after-last-line-indent (point))

            ;; Go to the first line of the continuation and record it
            (fb-back-to-initial-indentation)
            (setq initial-column (current-column))
            (setq initial-linenum (line-number-at-pos))
            (save-restriction
              ;; Consider everything between start of the first line
              ;; and the start of the line we're indenting
              (narrow-to-region (point) before-last-continuation)

              (if (> (1- indenting-linenum) initial-linenum)
                  ;; This isn't the first continuation line, so use the previous
                  ;; line's indentation as the default (if no unclosed sexp).
                  (setq best after-last-line-indent)

                ;; This is the first continuation line, so the default (if we have no unclosed sexp)
                ;; is to align after the first symbol on the initial line.
                (forward-symbol 1)  ; Can't skip over _ on the line by itself, because we narrowed the buffer
                ;; ...but if there are certain keywords like PRESERVE present, align after them instead.
                ;; (This isn't totally satisfactory, and line-end-position may not be right either)
                (while (re-search-forward fb--alignable-keywords (line-end-position) t))
                ;; Skip over punctuation or whitespace, so given "foo = bar _", align to "bar"
                (skip-syntax-forward " .")
                (when (= (point) before-last-continuation)
                  ;; Ignore any whitespace before the _ so that we don't align to the _
                  (skip-syntax-backward " "))
                (-consider-point))

              ;; Look for an open sexp, ie. a (, { or [ with no matching ), } or ], and if so indent just past it.
              (while (> (point) last-point)
                (setq last-point (point))
                (condition-case nil
                    ;; Can use fb-skip-variable-decl instead
                    (forward-sexp)
                  (scan-error
                   ;; This should be an "Unbalanced parenthesis" error,
                   ;; though it could also be that there's an extra ), ] or }.
                   ;; Skip over punctation, eg "foo = {" after the "foo" we want to move up to the {
                   (skip-syntax-forward ".")
                   ;;(skip-syntax-forward " ")  ; Doesn't skip newlines
                   (skip-chars-forward " \t\n")
                   (skip-syntax-forward "(")
                   (-consider-point))))
              ) ; Exit narrowing so can get actual column number
            (goto-char best)
            (current-column)))))))

(defun fb--indent-column ()
  "Return the best-guess indentation of the current line, by looking for
opening and closing brackets on the current and previous non-blank line and multiplying
by fb-indent-level."
  (or (fb--indent-column-for-continued-line)
      (save-excursion
        (back-to-indentation)
        (+ (* fb-indent-level
              ;; end (if|with|scope|select|sub|function|operator|constructor|destructor|type|enum|...)
              (+ (if (looking-at (fb-rex "([]`)}]|(end` [a-z]+|else|elseif|loop|wend|next|case|#endif|#else|#elseif|#endmacro)\\_>)"))
                     -1 0)
                 (cl-block nil
                   ;; Skip whitespace, blank lines and comments backwards. Doesn't work properly
                   ;; if already in a comment
                   (forward-comment -9999)
                   ;; Either the line ends in one of these...
                   (when (looking-back (fb-rex "\\_<(then|else)"))
                     (cl-return 1))
                   (fb-back-to-initial-indentation)
                   ;; ...or begins with one of these
                   (when (looking-at (concat
                                      (fb-rex "(for|with|case|while|do|select|scope|withnode|readnode|#if|#ifdef|#ifndef|#elseif|")
                                      fb-toplevel-start
                                      "\\)\\b"))
                     (cl-return 1))
                   0)))
           (progn
             (fb-back-to-initial-indentation)
             (current-column))))))

(defun fb-indent-line ()
  "Indent current line to the same
level as previous line +/- fb-indent-level if previous non-blank line ends in ({[, )}],
or begin, end. If run multiple times in a row, then instead indents the line by
an extra fb-indent-level each time."
  (interactive)
  (let ((blank-line
         (save-excursion
           (beginning-of-line)
           (skip-chars-forward " \t")
           (looking-at "\n")))
        (col
         (if (eq this-command last-command)
             (save-excursion
               (back-to-indentation)
               (+ (current-column) fb-indent-level))
           (fb--indent-column))))
    (if blank-line
        (progn
          (delete-horizontal-space)
          (indent-to col))
      (save-excursion
        (back-to-indentation)
        (delete-horizontal-space)
        (indent-to col))
      (if (< (current-column) col)   ; Shouldn't happen?
          (back-to-indentation)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Interactive functions


(defun fb-split-indent-line ()
  "Splits the current line at point, adding a _ line continuation
and indenting the new line. Can split in the middle of a string or comment!"
  (interactive)
  (let ((state (syntax-ppss)))
    (cond ((nth 3 state)                ; In a string (and it is the string char)
           (insert (nth 3 state))
           (insert " _")
           (newline)
           (insert (nth 3 state))
           (indent-according-to-mode))
          ((nth 4 state)                ; In a comment
           (indent-new-comment-line))
          (t
           (delete-horizontal-space)    ; Delete spaces around point
           (insert " _")
           (newline-and-indent)))))

;; Not used
(defun fb-auto-capitalise-self-insert-function ()
  (when (and (eq (char-before) last-command-event) ; Sanity check.
             (memq (char-syntax last-command-event) '(?\( ? )))
    (save-excursion
      (backward-char)
      (when (and (symbol-at-point)
                 (or (re-search-backward (concat "\\_<" (symbol-at-point) "\\_>")
                                         nil t)
                     (re-search-forward (concat "\\_<" (symbol-at-point) "\\_>")
                                         nil t)))
        ;;TODO
        ))))

(defun fb-make-for-loop (iter end)
  "Insert a FOR loop with integer index, prompting for variable name and end bound."
  (interactive "sLoop variable: \nsEnd point: \n")
  (indent-according-to-mode)
  (insert "for " iter " as integer = 0 to " end "\n\nend if")
  (indent-according-to-mode)
  (previous-line)
  (indent-according-to-mode))

(defconst fb-operator-keyword-list
  (split-string "and andalso eqv imp not or orelse xor shl shr let mod strptr varptr procptr is new delete")
  "List of all operators that have no punctuation in the name (excluding for, next, step, cast
although they are listed as operators in the manual since they can be overridden).")

(defun fb-doc-pagename (name)
  "Return the wakka page name for a given symbol. Incomplete!"
  ;; Normalise case and remove spaces (e.g. in "# pragma reserve", "option dynamic"),
  ;; though wiki URLs are case insensitive anyway
  (setq name (replace-regexp-in-string " " "" (capitalize name)))
  (let ((pagename
         (or
          ;; First check exceptions (currently, take first one if the name is ambiguous)
          ;; TODO: there are still more ambiguous keywords not included below
          ;; TODO: ensure all symbols like "write #" can be found when searching for "write" too
          ;; TODO: handle combining assignments like +=
          (alist-get (downcase name)
                     '(("end" . "End") ("end" . "Endblock")
                       ("static" . "Static") ("static" . "StaticMember")
                       ("const" . "Const") ("const" . "ConstMember")
                       ("constructor" . "Constructor") ("constructor" . "ModuleConstructor")
                       ("destructor" . "Destructor") ("destructor" . "ModuleDestructor")
                       ("byref" . "Byref") ("byref" . "ByrefFunction")  ("byref" . "ByrefVariables")
                       ("type" . "Type") ("type" . "TypeAlias")  ("type" . "TypeTemp")
                       ("explicit" . "Enum") ("explicit" . "OptionExplicit")  ;No individual page for enums?
                       ("return" . "Return") ("return" . "ReturnGosub")
                       ("on" . "OnError") ("on" . "OnGoto") ("on" . "OnGosub")
                       ("error" . "Error") ("error" . "OnError")
                       ("if" . "IfThen")
                       ("select" . "SelectCase")
                       ("is" . "OpIs") ("is" . "Is")
                       ("extern" . "Extern") ("extern" . "ExternBlock")
                       ("base" . "Base") ("base" . "BaseInit")
                       ("?" . "Print") ("?#" . "PrintPp") ("?using" . "Printusing")
                       ("read" . "ReadFile") ("read" . "Read") ("readwrite" . "ReadWriteFile")
                       ("write" . "WriteFile") ("write" . "Write")
                       ("get" . "Getfileio") ("get" . "GetGraphics") ("get#" . "Getfileio")
                       ("put" . "Putfileio") ("put" . "PutGraphics") ("put#" . "Putfileio")
                       ("input" . "InputFilemode") ("input" . "InputNum")
                       ("seek" . "SeekSet") ("seek" . "SeekReturn")
                       ("mid" . "MidFunction") ("mid" . "MidStatement")
                       ("string" . "StringFunction") ("string" . "String")
                       ("wstring" . "WstringFunction") ("wstring" . "Wstring")
                       ("mod" . "OpModulus") ("shl" . "OpShiftLeft") ("shr" . "OpShiftRight")
                       ("let" . "Let") ("let" . "LetList")
                       ("=" . "OpEqual") ("=" . "OpAssignment") ("=>" . "OpAssignment")
                       ("<>" . "OpNotEqual") ("<" . "OpLessThan") (">" . "OpGreaterThan")
                       ("<=" . "OpLessThanOrEqual") (">=" . "OpGreaterThanOrEqual")
                       ("+" . "OpAdd") ("+" . "OpConcat") ("&" . "OpConcatConvert")
                       ("-" . "OpSubtract") ("-" . "OpNegate")
                       ("*" . "OpMultiply") ("*" . "OpValueOf")
                       ("/" . "OpDivide") ("\\" . "OpIntegerDivide") ("^" . "OpExponentiate")
                       ("." . "OpMemberAccess") ("->" . "OpPtrMemberAccess") ("@" . "OpAt")
                       ("pointer" . "Ptr")
                       ("procptr" . "OpProcptr") ("strptr" . "OpStrPtr") ("varptr" . "OpVarptr")
                       ("()" . "OpArrayIndex") ("[]" . "OpStringIndex") ("[]" . "OpPtrIndex")
                       ("..." . "Dots")
                       ("#" . "OpPpStringize") ("##" . "OpPpConcat")
                       ("#endmacro" . "PpMacro") ("#include" . "Include") ("#inclib" . "Inclib")
                       ("once" . "Include")
                       ("!" . "OpPpEscape") ("$" . "OpPpNoescape"))
                     nil nil 'string=)
          ;; Operators
          (when (seq-contains-p fb-operator-keyword-list name)
            (concat "Op" name))
          ;; Preprocessor and tokens containing #, e.g. #define -> PpDefine, print # -> PrintPp
          (when (string-match "#" name)
            (replace-regexp-in-string "#" "Pp" name))
          ;; '$... preprocessor
          (when (string-match "\\$\\(.*\\)" name)
            (concat "Meta" (match-string 1 name)))
          ;; Defines
          (when (string-match "^__\\(.*\\)__$" name)
            (setq name (concat "Dd" name)))
          ;; else
          name)))
    (concat "KeyPg"
            ;; Remove underscores
            (replace-regexp-in-string "_" "" pagename))))

(defun fb-lookup-doc (name)
  "Open a web browser for a page in the FB manual documenting a function/keyword.
To lookup a keyword which is a non-text token or multiple words, like '->' or
'line input' or 'get #', you have to manually type it at the prompt."
  ;; TODO: detect tokens at point like "@" and "...", as well as preprocessor tokens containing
  ;; whitespace like "# define" and keywords like "open #", "print using", "$dynamic",
  ;; but beware optional type suffixes like "input$"
  ;; TODO: if the keyword is ambiguous prompt which page to open
  (interactive
   (list (fb-prompt-with-default-at-point
          "Keyword (eg. \"line input #\") to lookup in manual? ")))
  (browse-url (concat "http://www.freebasic.net/wiki/wikka.php?wakka=" (fb-doc-pagename name))))

(defun fb-beginning-of-defun (&optional arg)
  "Move point to start of line starting a function or other toplevel block.
arg tells which block: 1 means last defun start, 2 the one before, -1 the one after, etc"
  (interactive "p")
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (search-backward-regexp (concat "^" fb-toplevel-start )
                          nil :move-to-start arg))

(defun fb-end-of-defun (&optional arg)
  "Move point past last line of a function or other toplevel block.
arg tells which block: 1 means next end, 2 the one after, -1 the one before, etc"
  (interactive "p")
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (when (search-forward-regexp
         ;; Accept only ')' or 'end' or 'end <fb-toplevel>' on a line by
         ;; itself, excluding whitespace and comments
         (concat (fb-rex "^`)|^end[ \t]*(\n|\\s<)|^#` *endmacro|^end *(") fb-toplevel-start "\\)")
         nil :move-to-end arg)
    (forward-line)))

;; Not used; not complete or useful -- need a fill-paragraph implementation
(defun fb-adaptive-fill ()
  (when (looking-at "[ \t]*'[ \t]*")
    (match-string 0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Compiling


(defun fb--set-compile-command ()
  "Called from fb-mode to set the default command for `compile' to run either 'fbc', 'make', or 'scons'."
  ;; If there's a makefile, stick to default ("make -k ")
  (unless (or (file-exists-p "makefile")
	      (file-exists-p "Makefile"))
    (setq-local compile-command
                (if (cl-some 'file-exists-p '("SConstruct" "Sconstruct" "sconstruct"
                                              "SConstruct.py" "Sconstruct.py" "sconstruct.py)"))
                    "scons "
		  (concat "fbc "
			  (if buffer-file-name
			      (shell-quote-argument (file-relative-name buffer-file-name))))))))

(require 'compile)

;; Recognise fbc's errors
(push '("^\\([^(\n]+\\)(\\([0-9]+\\)) \\(\\(error\\)\\|\\(warning\\)\\) [0-9()]+:" 1 2 nil (5))
      compilation-error-regexp-alist)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Syntax highlighting (font-lock)


;; Adapted from my fb-mark-arg in misc/fb-utils.el
(defun fb-skip-variable-decl (args &optional limit pushmark)
  "In the middle of an argument list, move point to end of the args-th arg on from point,
and return the position of the start (no earlier than where we started (also set as
the mark if called interactively."
  ;; limit defaults to nil, the others default to 1 when called interactively
  (interactive "p\ni\np") ;"nSkip forward how many args? ")
  (when (char-equal (char-after) ?, ) (forward-char))
  (when pushmark
    (push-mark))
  (let ((start (point)) char (argno 1))
    (save-restriction
      (when limit
        (narrow-to-region start limit))
      (catch 'done
        (while		;when search fails, go to limit and break if on last arg
            (re-search-forward "[,([{\"]" limit (if (eq argno args) 0 nil))
          ;;(backward-char)
          (setq char (char-before))
          (cond ((eq (char-syntax char) ?\( ) (backward-char) (forward-sexp 1))
                ;;((char-equal char ?\) ) (error "not enough arguments"))
                ((char-equal char ?, ) (if (<= (incf argno) args) (set-mark (1- (point)))
                                         (throw 'done nil)))
					;I assume forward-sexp uses normal escape codes, not ideal
                ((char-equal char ?\") (backward-char) (forward-sexp 1))))))
    ;;(forward-char))))
    (when (< (point) (or limit (buffer-end 1))) (backward-char))
    ;; (when (> args 0) (backward-char)))
    start))

(defun fb-match-variable-decl (&optional end-pos)
  "This parses two forms of variable declaration, as below, setting the match data and point.
Point is moved to the end of this var declaration (skipping initialisers).

  [byref|byval] varname [as typename] [= initialiser]
The type is optional to allow 'dim as integer x, y'.
BUG: if there are array dims after varname, 'as typename' is missed!

  as typename varname [= initialiser]
In this case, byref|byval must have been skipped by the caller.
varname is not in the match data, instead point is left in front of varname
so that a repeat call will match it."
  ;;(interactive)
  (let ((case-fold-search t))
    (when (null end-pos) (setq end-pos (line-end-position)))
    (let ((arg-start (point)) ret)
      ;; Ignore errors due to unbalanced parens
      (condition-case nil
          ;; Get start and end (point) of the decl
          (setq arg-start (fb-skip-variable-decl 1 end-pos))
        (scan-error))
      (setq end-pos (point))
      ;; point is now positioned by a comma or the end
      (goto-char arg-start)

      ;; Begins with 'as'?
      (if (looking-at "\\s *\\_<as\\_>")
          (progn
            ;; Skip to end of type and variable name (allow * for fixed strings)
            ;;(search-forward-regexp "[^=('/,\n]*" end-pos)
            (search-forward-regexp "[ ._a-z0-9*]*" end-pos)
            ;; Back over the variable name
            ;;(skip-syntax-backward " " arg-start)
            (search-backward-regexp "\\_<[._a-z0-9]+\\s *" arg-start t)
            ;;(message "skipped back over %s" (match-string 0))
            (setq end-pos (point))
            (goto-char arg-start)
            ;; Match just the 'as type', byref/byval and variable name blank
            (setq ret (search-forward-regexp
                       (fb-rex "` *\\_<()()(as` +([ ._a-z0-9*]+)?)") end-pos t)))
        (setq ret (search-forward-regexp
                   (fb-rex "` *(byref|byval)?` *([._a-z0-9]+)(` +as` +([ ._a-z0-9*]+))?") end-pos t)))
      ;; (fb-rex "` *([a-z0-9_.]+)(` +as` +[a-z0-9_. ]+)?") end-pos t))
      ;; (message "match: total(%s), %s , %s, %s, %s" (match-string 0)
      ;;          (match-string 1) (match-string 2) (match-string 3) (match-string 4))

      (goto-char end-pos)
      ret)))

;; TODO: figure out how to use a good default if the current style is light-coloured
(defface fb-emph-face
;(face-spec-set 'fb-emph-face
  '((t :inherit default-face :background "#7070A0" :weight bold))
  "Face used for : statement separator.")

(defvar fb-font-lock-keywords
  (list
   ;;; Do these first, so they override everything except comments and strings
   ;; (cons "=\\|@" font-lock-variable-name-face)
   (cons ":\\|\\_<_\\_>" ''fb-emph-face) ;font-lock-warning-face)
   ;; Don't highlight a : after a case. You can write 'case asc("="):'
   (list "\\_<case\\_>\\(?:[ ,][-._a-z0-9<>()]*\\|\"[^\"]*\"\\)+?\\(:\\)" 1 ''default t)

   ;;; Preprocesser #blocks and special functions/keywords
   (cons (fb-rex "^` *#` *[a-z_]+( +once)?") font-lock-preprocessor-face)
   ;; Preprocessor intrinsic defines (TODO: explicitly enumerate the large number of builtin defines)
   (cons (fb-rex "\\_<__(fb_[a-z0-9_]+|file|file_nq|path|function|function_nq|line|date|date_iso|time)__\\_>")
         font-lock-preprocessor-face)
   (cons (fb-rex "\\_<(defined|namespace|using|option)\\_>|##?")
         font-lock-preprocessor-face)
   ;; #define and #macro names and arglists. (TODO: The arglist is optional for macros?)
   (list (fb-rex "^` *#` *(define|macro)` +([._a-z0-9]+)` *`(([^)]*)`)")
         '(2 font-lock-function-name-face)
         '(3 font-lock-variable-name-face))
   ;; Highlight define with no args as a variable/const rather than function.
   (list (fb-rex "^` *#` *define` +([._a-z0-9]+)")
         '(1 font-lock-variable-name-face))

   ;;; Keywords and flow control
   (list (fb-regexp-join-symbols fb-keywords)
         '(0 font-lock-keyword-face keep))

   ;; FIXME: in "seq.next", "next" gets highlighted because . is punctuation, not symbol

   ;;; Function/Sub/Type/etc names and arglists
   ;; The first regex finds the function name and the start of the arglist, if any.
   ;; We skip over the ( at the end, as it would cause fb-match-variable-decl to skip everything.
   ;; We might catch some function attributes like 'alias "foo"', or even either type of comment,
   ;; or "extends", but because we use 'keep this doesn't matter.
   ;; Also, the function name is optional, to support 'as sub(...)' type declarations.
   ;; FIXME: "function debug_surfaces_list as integer" without brackets is allowed too
   (list (concat (fb-rex "^` *(?:declare` *)?(?:(?:virtual|abstract)` *)?(") fb-toplevel-start (fb-rex ")([^`(\n]*)`(?"))
         '(1 font-lock-keyword-face)   ;Also done below
         '(2 font-lock-function-name-face keep)
         ;; Just highlight the whole arglist one colour
         ;; '("(\\([^)]*\\))" nil nil
         ;;   (1 font-lock-variable-name-face keep)))
         '(fb-match-variable-decl nil nil
                        ;;(1 font-lock-builtin-face keep noerror)     ; byref/byval
                        (2 font-lock-variable-name-face keep noerror) ; variable name
                        (4 font-lock-type-face keep noerror)))        ; type

   ;;; Highlight variable declarations (DIM, EXTERN, etc)
   (list (concat "\\(" (fb-regexp-join-symbols '("dim" "redim" "var" "const" "extern" "static"))
                 (fb-rex ")` +(shared |byref |preserve )*"))  ;"\\_<dim\\_>"
         ;;'(0 font-lock-keyword-face keep)  ;; dim as well as shared, static, etc. Done below
         '(fb-match-variable-decl nil nil
                        ;;(1 font-lock-builtin-face keep noerror)  ; byref/byval
                        (2 font-lock-variable-name-face keep noerror)  ; variable name
                        (4 font-lock-type-face keep noerror)))   ; type
   ;; FOR is simpler, as the "AS type" is handled separately
   (list (fb-rex "\\_<for` +([._a-z0-9]+)` +as") '(1 font-lock-variable-name-face))

   ;;; Highlight (more) types
   ;; The above doesn't catch types after array bounds var() as foo. This also is needed for TYPE members.
   (list "\\_<as\\_>\\([\t ._a-z0-9*]+\\)"
         '(1 font-lock-type-face keep))

   ;;; Declaration-related keywords
   (list (fb-regexp-join-symbols
          (split-string "byval as any extends is"))
         '(0 font-lock-keyword-face keep))
   (list (fb-regexp-join-symbols '("byref"))
         '(0 font-lock-warning-face keep))

   ;;; Other toplevel blocks
   (cons fb-toplevel-start font-lock-keyword-face) ;; This is needed to highlight 'sub' in 'end sub', etc

   ;;; Operators and builtin functions
   ;; "= ( ) <> + - * / ^ ,"
   ;; (split-string "mid left right instr chr asc ubound lbound new delete allocate callocate deallocate
   ;;                 int cint trunc fix mod "))

   ;;; Special operators
   (cons (fb-regexp-join-symbols
          (split-string "or and not orelse andalso sizeof typeof cast cptr iif"))
         font-lock-keyword-face)  ;font-lock-builtin-face)
   ))

(defvar fb-font-lock-defaults
  ; Ignore case while fontifying
  '(fb-font-lock-keywords nil t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar fb-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Replace C-M-j which adds a new line and indent only while in a comment block
    (define-key map "\C-\M-j" 'fb-split-indent-line)
    (define-key map "\C-c\C-f" 'fb-make-for-loop)
    (define-key map "\C-c\C-h" 'fb-lookup-doc)
    (define-key map (kbd "<f5>") 'compile)
    map))


(define-derived-mode fb-mode prog-mode "FreeBASIC"
  "Major mode to edit FreeBASIC."
  (set-syntax-table fb-syntax-table)
  (setq-local font-lock-defaults fb-font-lock-defaults)
  (setq-local comment-start "'")
  (setq-local comment-end "")
  (setq-local comment-start-skip "/?'+[ \t]*")
  ;; (setq-local comment-start "/'")
  ;; (setq-local comment-end "'/")
  (setq-local comment-end-skip "[ \t]*'+/")
  (setq-local beginning-of-defun-function 'fb-beginning-of-defun)
  (setq-local end-of-defun-function 'fb-end-of-defun)
  (setq-local indent-line-function 'fb-indent-line)
  ;; FIXME: if user changes this, a lot of functions will break
  (setq-local case-fold-search t)

  ;; Support ' for FreeBasic comments (just add ' to the default value)
  (setq-local adaptive-fill-regexp
              (purecopy "[ \t]*\\([-–!|'#%;>*·•‣⁃◦]+[ \t]*\\)*"))
  ;; Alternative to changing adaptive-fill-regexp
  ;(setq-local adaptive-fill-function #'fb-adaptive-fill)

  (fb--set-compile-command))



(provide 'fb-mode)
