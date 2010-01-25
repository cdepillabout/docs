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

" matchgroup associates the matched part of the expression with the group
" sasStartFoldStep.  We then can apply highlighting to this group.
syn region sasFoldStep matchgroup=sasStartFoldStep start=/^\(PROC \w\+\|DATA\)/ end=/^\(RUN\|QUIT\)/ fold transparent
hi def link sasStartFoldStep sProc

" Syncronize from beginning to keep large blocks from losing
" syntax coloring while moving through code.
syn sync fromstart

" vim: ts=8
