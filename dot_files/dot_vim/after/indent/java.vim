
" Java files don't get indented correctly after an annotation.
" Everything is pushed over to the side.  So, for instance, 
" a function will look like this after indenting it automatically:
"
" @Override
"	public void myfunc()...
"
" When it should actually look like this:
"
" @Override
" public void myfunc()...
"
" This code below fixes the problem. 
" (This may need to be removed at some point in the future if 
"  it is added into the stock vim indentation code.)

function! GetJavaIndent_improved()
	let theIndent = GetJavaIndent()
	let lnum = prevnonblank(v:lnum - 1)
	let line = getline(lnum)
	if line =~ '^\s*@.*$'
		let theIndent = indent(lnum)
	endif

	return theIndent
endfunction

setlocal indentexpr=GetJavaIndent_improved()
