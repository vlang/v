// Copyright (c) 2019-2021 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import x.toml.token
//import x.toml.util

pub type Key = Bare | Quoted

pub fn (k Key) str() string {
	return k.text
}

//pub type Table = map[string]Value
//pub type Array = []Value
pub type Value = Quoted | Date | []Value | map[string]Value
/*
pub fn (v Value) str() string {
	return match v {
		Quoted, Date {
			v.text
		}
		[]Value {
			'<array>'
		}
		map[string]Value {
			'<table>'
		}
	}
}
*/
//pub type Node = Root | Comment | KeyValue


pub struct Comment {
pub:
	text     string
	pos      token.Position
}

pub fn (c Comment) str() string {
	mut s := typeof(c).name+'{\n'
	s += '  text:  \'$c.text\'\n'
	s += '  pos:  $c.pos\n'
	s += '}'
	return s
}
/*
pub struct KeyValue {
pub mut:
	key      Key
	value    Value
}

pub fn (kv KeyValue) str() string {
	mut s := typeof(kv).name+'{\n'
	s += '  key:  $kv.key\n'
	s += '  value:  $kv.value\n'
	s += '}'
	return s
}
*/
/*
pub struct Err {}

pub fn (e Err) str() string {
	return typeof(e).name+'{}\n'
}
*/
pub struct Quoted {
pub:
	text     string
	pos      token.Position
}

pub fn (q Quoted) str() string {
	mut str := typeof(q).name+'{\n'
	str += '  text:  \'$q.text\'\n'
	str += '  pos:  $q.pos\n'
	str += '}'
	return str
}

pub struct Bare {
pub:
	text     string
	pos      token.Position
}

pub fn (b Bare) str() string {
	mut str := typeof(b).name+'{\n'
	str += '  text:  \'$b.text\'\n'
	str += '  pos:  $b.pos\n'
	str += '}'
	return str
}

pub struct Date {
pub:
	text     string
	pos      token.Position
}

pub fn (d Date) str() string {
	mut str := typeof(d).name+'{\n'
	str += '  text:  \'$d.text\'\n'
	str += '  pos:  $d.pos\n'
	str += '}'
	return str
}

/*
// Table
pub struct Table {
pub mut:
	comments []Comment
	pairs    map[string]Value
}

pub fn (t Table) str() string {
	mut str := typeof(t).name+'{\n'
	str += '  comments:  \'$t.comments\'\n'
	str += '  pairs:  \'$t.pairs\'\n'
	str += '}'
	return str
}
*/
/*
pub fn (t Table) has_table(key string) bool {
	return key in t.pairs
}
*/

/*
pub fn (t Table) find(key string) ?Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, '"$key"')
	dump(t.pairs)
	if key == '' {
		panic(@MOD + '.' + @STRUCT + '.' + @FN + ' can\'t find empty key')
	}
	skey := key.trim_right('/').split('/')
	for k, v in t.pairs {
		//if kv.key.str() == skey[0] {
		if k == skey[0] {
			val := v//.value
			if val is Quoted || val is Date {
				return v //kv.value
			} else if val is Table {
				//if skey.len > 1 {
				tbl := Table{...val}
				return tbl.find(skey[1..].join('/'))
				//} else {
				//	val
				//}
			}
			else {
				return error(@MOD + '.' + @STRUCT + '.' + @FN + ' TODO BUG')
			}
		}
	}
	dump(t.pairs)
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'path "$key" has none')
	return none
}
*/
/*
pub fn (t Table) get_active_table() &ast.Table {
	return t.get_table(t.active_table)
}
*/
/*
pub fn (mut t Table) insert(key Key, value Value) {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'inserting to "$key"')
	for k, _ in t.pairs {
		//if kv.key.str() == key.str() {
		if k == key.str() {
			t.pairs[k] = value
			//util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'replaced $k <- ${value.str()}')
			return
		}
	}
	//util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'new ${key.str()} <- ${value.str()}')
	t.pairs[key.str()] = value

	//dump(t.pairs)
	//panic(@MOD + '.' + @FN + '.pairs[\'$key\'] doesn\'t exist')
}
*/

/*
pub struct Array {
pub mut:
	values    []Value
}

pub fn (a Array) str() string {
	mut str := typeof(a).name+'{\n'
	str += '  values:  \'$a.values\'\n'
	str += '}'
	return str
}
*/

/*
pub fn (a Array) find(key string) ?Value {
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, '"$key"')
	if key == '' {
		panic(@MOD + '.' + @FN + ' can\'t find empty key')
	}
	skey := key.split('/')
	for val in a.values {
		if val is Quoted || val is Date {
			return val
		} else if val is Array {
			//if skey.len > 1 {
			arr := Array{...val}
			return arr.find(skey[1..].join('/'))
			//} else {
			//	val
			//}
		}
		else {
			return error(@MOD + '.' + @FN + ' TODO BUG')
		}
	}
	util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, 'path "$key" has none')
	return none
}*/

pub struct EOF {
pub:
	pos      token.Position
}

pub fn (e EOF) str() string {
	mut str := typeof(e).name+'{\n'
	str += '  pos:  $e.pos\n'
	str += '}'
	return str
}
