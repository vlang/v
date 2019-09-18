// (c) 2019 keito940 All Rights Reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module toml

import ebnf

const (
	DEC = 1
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
	// Data Writing.
	writing_data()
}

fn writing_data(){

}

struct KeyVal{
	name	string
	mut:
		val TOMLVal
}

fn (k KeyVal) name() string{
	return k.name
}

fn (k KeyVal) search(s string) {
	k.name == s or {
		panic('Invaild Key Name!')
	}
}

struct Array{
	name 			string
	mut:
		val 			[]TOMLVal
		value_type 		int
		arr 			[]Array
		tbl				[]Table
}

fn (a Array) name() string{
	return a.name
}

fn (a Array) search(s string){
	a.name == s or{
		panic('Invaild Array Name!')
	}
}

struct Table{
	name		string
	mut:
		key 		[]KeyVal
		tbl			[]Table
		arr			[]Array
}

fn (t Table) name() string{
	return t.name
}

struct TOMLInt{
	val 	i64
	str_val	string
	enum IntType{
		demical binary octal hex
	}
}

// TOML's Value.
struct TOMLVal{
	integer		TOMLInt
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