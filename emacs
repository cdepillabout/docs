
;;; General commands
; Undo
; C-x u

;;; Scheme.
;http://www.cs.huji.ac.il/~osigor/emacs/pllab.html#5.1
; in emacs, "M-x run-scheme" to run the scheme interpreter.  
; Or "C-c C-q r"
; Then, "C-x h" to select the whole region.
; "C-c M-r" to send the region to the scheme interpreter
; "C-c C-z" to switch to the repl buffer (from the editing window).

; "C-x C-e" to send the last sexp to the repl.
; "C-c M-e" send the last definition to the repl and go to the repl.

; From repl, "C-c C-L" to load a file to be evaluated. Then,
; just push enter to load the file you loaded last time.


;;; Running commands in emacs
; Change to *scratch*, write the emacs lisp you want to write
; (eg. '(setq scroll-conservatively 1)')
; Then C-j and it will be evaluated.


;;; Indentation
; Indent several lines at once.
; Make all the lines to indent, then C-M-\


;;; Search and replace
; To search for some examples of a string and replace them,
; do M-%.

;;; Macros
; Start macro
; C-x (
; <type some stuff...>
; End macro
; C-x )
; Execute most recent macro
; C-x e
