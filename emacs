
;;; General commands
; Undo
; C-x u


;;; Windows and Buffers
; To create a new buffer, just C-x b, and type the buffer name.
; To Kill them, just C-x k.  To save them, just C-x C-w.
; Split windows vertially: C-x 2
; Split windows horizontally: C-x 3
; Balance windows: C-x +
; move to next window: C-x o
; delete other windows: C-x 1
; list buffers: C-x C-b.


;;; HELP
; show all keyboard shortcuts that are defined for current buffer:
; M-x describe-bindings
; Then, scroll down to the command and hit enter.  It will show you
; help for that command.
;
; To find help for a particular key sequence,
; M-x describe-key
; Then, type the key sequence.
;
; If you are interested in finding a command, but not completely
; sure about the name, do
; M-x apropos
; and type in a regular expression.


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
;
; fill-paragraph: M-p
; intelligently line-wraps your text for you. 


;;; Search and replace
; To search for some examples of a string and replace them,
; do M-%.
; To do incremental search forward, 
; C-s.  Backward is C-r.


;;; Macros
; Start macro
; C-x (
; <type some stuff...>
; End macro
; C-x )
; Execute most recent macro
; C-x e

;;; ESS 
; When running a remote SAS, using M-x ssh, you can run the sas
; command like this:
; /usr/local/SAS/SASFoundation/9.2/sas -nodms
;
; First, limited documentation about each ESS command can be
; obtained by typing C-h f. For example, if you type C-h f
; ess-eval-region, documentation for that command will appear in a
; separate *Help* buffer. Second, a complete list of keybindings that
; are available in each ESS mode and brief description of that mode is
; available by typing C-h m within an ESS buffer.

;; moving around in the shell buffer that holds the sas session
; M-{ and M-}		move you back and forward through commands in the session.
; C-x [ and C-x ]	move you back and forward through outputs from the session.
; C-c C-e			move you to the end of the buffer.
; 
; When you are on an old command line, RET, C-c RET, and M-RET also have special meanings.
; There are also commands like Bash's C-r.
