// (c) 2019 keito940 All Rights Reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module toml

import ebnf

const (
	INT = 1
	BIN = 2
	HEX = 3
	OCT = 4
)

// bison
#include "toml.tab.h"
// yacc
#include "y.tab.h"

fn compile(){
	// lex or flex compile for C
	lex := system('flex -d toml.l') or system('lex -d toml.l') or eprintln('Please Install lex/flex.')
	// yacc or bison compile for C
	yacc := system('bison toml.y') or system('yacc toml.y') or eprintln('Please Install yacc/bison.')
}

fn C.yyparse()

fn load_parse(){
	&C.yyparse()
}

struct KeyVal{
	key string
	val TOMLVal
}

fn toml_parse(){
	// compile yacc(bison)/lex(flex) file.
	compile()
	load_parse()
}

struct Array{
	name 			string
	mut:
		val 			[]TOMLVal
		kind 			int
		value_type 		int
		arr 			[]Array
}

struct Table{
	name		string
	mut:
		key 		[]KeyVal
		kind 		int
		value_type 	int
		table		[]Table
}

// TOML's Valiable
struct TOMLVal{
	pub mut:
		integer		string
		binary_int  string
		octical_int string
		hexical_int string
		str 		string
		arr			Array
}

struct TimeStamp{
	year   		int
	month  		int
	date   		int
	hour   		int
	minute 		int
	second 		int
	millsecond 	int	
}