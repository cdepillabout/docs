" Vim syntax file
" Language:	SAS
" Maintainer:	
" Last Change:	25 Jan 2010
"		Added a little bit of folding support.


" These next two are set to this from the normal sas.vim file:
"syn match sasProc		"^\s*PROC \w\+"
"syn keyword sasStep		RUN QUIT DATA
" We don't want them, so we unset them.
syn clear sasStep
syn clear sasProc

" we want to fold datalines...
syn clear sasStatement
syn keyword sasStatement	ABORT ARRAY ATTRIB BY CALL CARDS CARDS4 CATNAME
syn keyword sasStatement	CONTINUE DELETE DISPLAY
syn keyword sasStatement	DM DROP ENDSAS ERROR FILE FILENAME FOOTNOTE
syn keyword sasStatement	FORMAT GOTO INFILE INFORMAT INPUT KEEP
syn keyword sasStatement	LABEL LEAVE LENGTH LIBNAME LINK LIST LOSTCARD
syn keyword sasStatement	MERGE MISSING MODIFY OPTIONS OUTPUT PAGE
syn keyword sasStatement	PUT REDIRECT REMOVE RENAME REPLACE RETAIN
syn keyword sasStatement	RETURN SELECT SET SKIP STARTSAS STOP TITLE
syn keyword sasStatement	UPDATE WAITSAS WHERE WINDOW X SYSTASK


" Block comment
"syn region sasComment	start="/\*"  end="\*/" contains=sasTodo fold
"syn region sasComment start=";\s*\*"hs=s+1 end=";" contains=sasTodo fold
"
" This one seems to be enough to get multiline comments folded.
syn region sasComment start="^\s*\*" end=";" contains=sasTodo fold
"
" Ignore misleading //JCL SYNTAX... (Bob Heckel)
"syn region sasComment	start="[^/][^/]/\*"  end="\*/" contains=sasTodo



" matchgroup associates the matched part of the expression with the group
" sasStartFoldStep.  We then can apply highlighting to this group.
"syn region sasFoldStep matchgroup=sasStartFoldStep start=/^\(PROC \w\+\|DATA\)/ end=/^\(RUN\|QUIT\)/ fold transparent
syn region sasFoldStep matchgroup=sasStartFoldStep start=/^\(PROC \w\+\|DATA\s\)/ end=/^\(RUN\|QUIT\)/ fold transparent
syn region sasFoldStep matchgroup=sasStartFoldStep start=/^\(PROC \w\+\|DATA\s\)/ end=/^DATALINES;/me=s-1 fold transparent
" I don't know how to make the ; after datalines not be highlighted
syn region sasFoldStep matchgroup=sasStartFoldStep start=/^DATALINES/rs=e+2,he=e-1 end=/;/ fold transparent
hi def link sasStartFoldStep sProc

" Syncronize from beginning to keep large blocks from losing
" syntax coloring while moving through code.
syn sync fromstart

" vim: ts=8
