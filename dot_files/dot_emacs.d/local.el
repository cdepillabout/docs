;;;; Here are just some of my local commands.
;;;; This find should be loaded on startup.


(defun forward-to-char (arg char)
  "This function tries to emulate VIM's f command."
  (interactive "p\ncForward to char: ")
  (search-forward (char-to-string char) nil nil arg))

(global-set-key (kbd "C-c f") 'forward-to-char)

(defun organize-lisp-comments ()
  "This is just a helper for copying and pasting commands from 
Practical Common Lisp.  It formats example commands in a pleasing manner."
  (interactive)
  (let ((point (point)))
	(beginning-of-line)
	(search-forward "==>" nil nil nil)
	(backward-char 3)
	(if (looking-at "==> ")
		(progn (loop while (eql (char-before) (string-to-char " "))
					 do (backward-delete-char 1))
			   (insert " ; ")))
	(goto-char point)))

(add-hook 'lisp-mode-hook
		  (lambda ()
			(local-set-key (kbd "C-c o") 'organize-lisp-comments)))
