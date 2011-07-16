" To use Cottontail syntax highlighting in Vim, add one line to you .vimrc:
"   au BufNewFile,BufRead *.ctn setf cottontail
" Then copy this file to ~/.vim/syntax/

" Nothing to do if the user wants custom syntax
if exists("b:current_syntax")
  finish
endif

" Syntax features
setlocal  iskeyword=!,#,$,&,+,-,/,48-57,<,=,>,?,@,A-Z,\\,94,_,a-z,\|,~
syn case match

" Symbols 
syn match cottontailDefine +\<==\>+
syn match cottontailAssign +\<#\?=\>+
syn match cottontailMember +\.+
syn match cottontailNest +:+
hi def link cottontailDefine Special
hi def link cottontailAssign Special
hi def link cottontailMember Special
hi def link cottontailNest Special

" Bracketting

" Keywords
syn keyword cottontailKeyword use lt lte gte ne eq
hi def link cottontailKeyword Statement

" Constants
syn keyword cottontailConstant true false nothing
hi def link cottontailConstant Constant

" Strings
syn region cottontailStringLiteral start=+'+ end=+'+
syn region cottontailStringInterpolate start=+"+ end=+"+
hi def link cottontailStringLiteral String
hi def link cottontailStringInterpolate String

" Numbers
syn match cottontailNumber +\<[0-9]*\>+
syn match cottontailHex "\<0x[0-9A-Fa-f_]*\>"
hi def link cottontailNumber Number
hi def link cottontailHex Number

let b:current_syntax = "cottontail"
