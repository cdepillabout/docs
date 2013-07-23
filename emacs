
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
;
; Show EMACS build in info.
; M-x info
;


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
; M-{ and M-} move you back and forward through commands in the session.
; C-x [ and C-x ] move you back and forward through outputs from the
; session.  
; C-c C-e move you to the end of the buffer.
; When you are on an old command line, RET, C-c RET, 
; and M-RET also have special meanings.  
; There are also commands like Bash's C-r.


; Use this to get a real shell
; M-x ansi-term
; You can probably also use this:
; M-x multi-term


; Ctrl-x 2:  split-window-vertically -- splits your current window into two 
;										equal-height windows showing the same 
;										buffer (until you change one of them
;										to show something else.)
; Ctrl-x 3:  split-window-horizontally -- most people don't use this as often, 
;										  but it's occasionally 
;										  useful. Splits the window into a 
;										  left-side and a right-side.
; Ctrl-x +:  balance-windows -- makes all visible windows approximately 
;								equal height. This is useful if you've just 
;								done Ctrl-x 2 twice in a row, because you'll 
;								have two 1/4-height windows and one 1/2-height 
;								window.	Ctrl-x + makes them all the same height.
; Ctrl-x o:   other-window -- moves the cursor into the next window in the 
;							  window list, which usually means moving it to the 
;							  window below the current one, or wrapping back up 
;							  to the top.
; Ctrl-x 1:  delete-other-windows -- makes the currently focused window fill 
;									 the entire frame; the others go away. 
;									 Note that the buffers they were visiting 
;									 stay around in the buffer-list, so it's 
;									 always perfectly safe to execute this 
;									 command.
; Ctrl-x Ctrl-b: list-buffers -- shows a list of all the buffers you have open, 
;								 in a nicely formatted buffer called 
;								 "*Buffer List*". This buffer's mode has many 
;								 convenience keys for manipulating the buffer 
;								 list. For instance, typing "d" while the 
;								 cursor is on one of the entries will flag that 
;								 buffer for deletion, "x" will kill the buffer, 
;								 and so on. Use M-x describe-bindings to view
;								 all the Buffer-menu key bindings. 


; Turn on font-lock-mode (syntax highlighting) for any buffer that doesn't already have it
; M-x font-lock-mode

; format a paragraph
; M-x fill-paragraph


; Code folding in emacs:
; M-x hs-minor-mode
; Fold or unfold something:
; C-c @ C-c
; Fold all
; C-c @ C-M-h
; Unfold all
; C-c @ C-M-s

; There is also set selective-display
; To fold
; M-1 C-x $
; To expand all
; C-x $


; Turn on line wrapping. (This also works for org-mode.)
M-x global-visual-line-mode

; Turn on clean view for org-mode
M-x org-indent-mode
