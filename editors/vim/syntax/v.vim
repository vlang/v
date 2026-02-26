" Vim syntax file
" Language: V
" Maintainer: V contributors

if exists('b:current_syntax')
	finish
endif

syn case match

syn keyword vTodo TODO FIXME XXX NOTE BUG contained
syn match vComment +//.*$+ contains=vTodo,@Spell
syn region vComment start='/\*' end='\*/' contains=vTodo,@Spell

syn region vString start=+r'+ end=+'+
syn region vString start=+c'+ skip=+\\\\\|\\'+ end=+'+ contains=vEscape,vInterpolation
syn region vString start=+'+ skip=+\\\\\|\\'+ end=+'+ contains=vEscape,vInterpolation
syn region vChar start=+`+ skip=+\\\\\|\\`+ end=+`+ contains=vEscape

syn match vEscape +\\[abfnrtv'"`\\$]+ contained
syn match vEscape +\\x[0-9A-Fa-f]\{2}+ contained
syn match vEscape +\\u[0-9A-Fa-f]\{4}+ contained
syn region vInterpolation start=+\${+ end=+}+ contained contains=vComptime,vNumber,vString,vChar,vOperator

syn match vNumber /\v<0x[0-9A-Fa-f_]+>/
syn match vNumber /\v<0b[01_]+>/
syn match vNumber /\v<0o[0-7_]+>/
syn match vNumber /\v<\d[\d_]*\.\d[\d_]*([eE][+-]?\d[\d_]*)?>/
syn match vNumber /\v<\d[\d_]*([eE][+-]?\d[\d_]*)?>/

syn keyword vBoolean true false
syn keyword vConstant none nil
syn keyword vKeyword as asm assert atomic break const continue defer else enum false for fn __global go goto if import in interface is match module mut shared lock rlock none nil return select sizeof isreftype _likely_ _unlikely_ __offsetof struct true type typeof dump or union pub static volatile unsafe spawn implements like ilike
syn keyword vType bool string rune i8 i16 int i64 i128 isize byte u8 u16 u32 u64 u128 usize f32 f64 char map chan any voidptr byteptr charptr

syn match vComptime /\$[A-Za-z_][A-Za-z0-9_]*/
syn match vComptime /@[A-Z_][A-Z0-9_]*/
syn region vAttribute start=/@\[/ end=/\]/ contains=vComptime

syn match vOperator /::\|:=\|==\|!=\|<=\|>=\|<<=\|>>=\|>>>=\|&&=\|||=\|<<\|>>>\|>>\|&&\|||\|+=\|-=\|\*=\|\/=\|%=\|\^=\||=\|&=\|<-\|++\|--/
syn match vOperator /[+\-*\/%&|^~!=<>?:]/

syn match vFunction /\<[A-Za-z_][A-Za-z0-9_]*\ze\s*(/

hi def link vTodo Todo
hi def link vComment Comment
hi def link vString String
hi def link vChar Character
hi def link vEscape SpecialChar
hi def link vInterpolation Special
hi def link vNumber Number
hi def link vBoolean Boolean
hi def link vConstant Constant
hi def link vKeyword Keyword
hi def link vType Type
hi def link vComptime PreProc
hi def link vAttribute PreProc
hi def link vOperator Operator
hi def link vFunction Function

let b:current_syntax = 'v'
