// (c) 2019 keito940 All Rights Reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// Thanks: Nett,tomlc99.

module toml
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
		tbl 	&[]Table
		arr		&[]Array
}


fn (t TOML) toml_parse(){
	// compile yacc(bison)/lex(flex) file.
	compile()
	// Parse Start.
	load_parse()
	// Data Writing.
	writing_data()
}

struct KeyVal{
	name	string
	mut:
		val TOMLVal
}

struct Array{
	name 			string
	value_type 		ArrayType
	mut:
		val 			&[]TOMLVal
		arr 			&[]Array
		tbl				&[]Table

	enum ArrayType{
		integer str muitl_str float time_stamp
	}
}

fn (a Array) val (i int) TOMLVal{
	return a.val[i]
}

fn new_array(name string,val TOMLVal,val_type int){
	return Array{}
}

fn (a Array) array_of_table(s string) ?Table{
	for table in a.tbl.key{
		if s == table.name{
			return table
		}
	}
	return error('Array:$a.name of Table:$a.tbl.name \'s Key:$s is Not Found.')
}

struct Table{
	name		string
	mut:
		key 		&[]KeyVal
		tbl			&[]Table
		arr			&[]Array
}

fn new_table(key_name string,val TOMLVal){
	return Table{name: key_name,key: val}
}

fn (t Table) key(s string) ?KeyVal{
	// Key Search.
	for table in t.key{
		if s == table.name{
			return table.name
		}			
	}
	return error('Key:$s is Not Found.')
}

struct TOMLInt{
	int_type IntType
	mut:
		val 	i64
		str_val	string
	enum IntType{
		demical binary octal hex
	}
}

struct TOMLDouble{
	mut:
		val f64
		str_val string
}

// TOML's Value.
struct TOMLVal{
	mut:
		integer		TOMLInt
		str 		string
		boolean 	bool
		time_stamp	TimeStamp
		arr			Array
}

fn (t TOMLInt) val() i64 {
	return t.val
}

fn (t TOMLInt) str_val() string{
	return t.str_val
}

fn (t TOMLDouble) val() f64{
	return t.val
}

fn (t TOMLDouble) str_val() string{
	return t.str_val
}

struct TimeStamp{
	// Calender
	year,month,date   					int
	// Clock
	hour,minute,second,millsecond  		int
}