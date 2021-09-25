" Vim syntax file
" Language: Astral
" Maintainer: 05st
" Latest Revision: 26 September 2021

if exists("b:current_syntax")
    finish
endif

" Preprocs
syn keyword PreProc import infixl infixr infix prefix postfix

" Keywords
syn keyword Keyword let data class impl

" Statements
syn keyword Statement if then else match with

" Built-in types
syn keyword Type Int Float Bool Char

" Boolean values
syn keyword Boolean True False

" Integers
syn match Number '[-ox]\?\d\+'

" Floats
syn match Float '[-]\?\d\+\.\d*'

" Strings
syn match SpecialChar contained "\\."
syn region String start='"' end='"' contains=SpecialChar

" Characters
syn match Character "'.'"
syn match Special "'\\.'"

" Operators
syn match Keyword "[:!#$%&*+./<=>\?@^|\-~]\+"

" Semicolons
syn match Ignore ";"

" Identifiers
syn match Ignore "[a-zA-Z][a-zA-Z0-9_']*"

" Comments
syn keyword Todo contained TODO FIXME NOTE
syn match Comment "--.*$" contains=Todo
syn region Comment start="{-" end="-}" contains=Todo
