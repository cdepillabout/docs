" Vim syntax file
" Language:	Java
" Maintainer:	
" Last Change:	25 Jan 2010
"		Added a little bit of folding support.

syntax clear javaBraces
syntax clear javaDocComment

syn region javaBraces start="{" end="}" transparent fold
syn region javaDocComment start="/\*\*" end="\*/" keepend contains=javaCommentTitle,@javaHtml,javaDocTags,javaDocSeeTag,javaTodo,@Spell fold

