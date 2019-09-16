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
// Original yacc
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

struct TOML{
	pub mut:
		tbl 	[]Table
		arr		[]Array
}

fn (t TOML) toml_parse(){
	// compile yacc(bison)/lex(flex) file.
	compile()
	// Parse Start.
	load_parse()
}

struct KeyVal{
	key string
	val TOMLVal
}

fn (k KeyVal) key_name() string{

}

struct Array{
	name 			string
	mut:
		val 			[]TOMLVal
		kind 			int
		value_type 		int
		arr 			[]Array
		tbl				[]Table
}

struct Table{
	name		string
	mut:
		key 		[]KeyVal
		kind 		int
		value_type 	int
		tbl			[]Table
}

// TOML's Value.
union TOMLVal{
	integer		string
	binary_int  string
	octical_int string
	hexical_int string
	str 		string
	boolean 	bool
	TimeStamp	TimeStamp
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