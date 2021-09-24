// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import toml.token

// Key is a sumtype representing all types of keys that
// can be found in a TOML document.
pub type Key = Bare | Bool | Null | Number | Quoted

pub fn (k Key) str() string {
	return k.text
}

// Node is a sumtype representing all possible value types
// found in a TOML document.
pub type Node = Bool | Date | DateTime | Null | Number | Quoted | Time | []Node | map[string]Node

pub fn (v Node) to_json() string {
	match v {
		Quoted, Date, DateTime, Time {
			return '"$v.text"'
		}
		Bool, Null, Number {
			return v.text
		}
		map[string]Node {
			mut str := '{'
			for key, val in v {
				str += ' "$key": $val.to_json(),'
			}
			str = str.trim_right(',')
			str += ' }'
			return str
		}
		[]Node {
			mut str := '['
			for val in v {
				str += ' $val.to_json(),'
			}
			str = str.trim_right(',')
			str += ' ]'
			return str
		}
	}
}

// DateTimeType is a sumtype representing all possible date types
// found in a TOML document.
pub type DateTimeType = Date | DateTime | Time

pub fn (dtt DateTimeType) str() string {
	return dtt.text
}

// value queries a value from the map.
// `key` should be in "dotted" form e.g.: `"a.b.c.d"`
pub fn (v map[string]Node) value(key string) &Node {
	null := &Node(Null{})
	key_split := key.split('.')
	// util.printdbg(@MOD + '.' + @STRUCT + '.' + @FN, ' retreiving value at "$key"')
	if key_split[0] in v.keys() {
		value := v[key_split[0]] or {
			return null
			// TODO return error(@MOD + '.' + @STRUCT + '.' + @FN + ' key "$key" does not exist')
		}
		// `match` isn't currently very suitable for these types of sum type constructs...
		if value is map[string]Node {
			m := (value as map[string]Node)
			next_key := key_split[1..].join('.')
			if next_key == '' {
				return &value
			}
			return m.value(next_key)
		}
		return &value
	}
	return null
	// TODO return error(@MOD + '.' + @STRUCT + '.' + @FN + ' key "$key" does not exist')
}

// value queries a value from the map.
pub fn (v map[string]Node) exists(key string) bool {
	key_split := key.split('.')
	if key_split[0] in v.keys() {
		value := v[key_split[0]] or { return false }
		// `match` isn't currently very suitable for these types of sum type constructs...
		if value is map[string]Node {
			m := (value as map[string]Node)
			next_key := key_split[1..].join('.')
			if next_key == '' {
				return true
			}
			return m.exists(next_key)
		}
		return true
	}
	return false
}

pub struct Comment {
pub:
	text string
	pos  token.Position
}

pub fn (c Comment) str() string {
	mut s := typeof(c).name + '{\n'
	s += '  text:  \'$c.text\'\n'
	s += '  pos:  $c.pos\n'
	s += '}'
	return s
}

// Null is used in sumtype checks as a "default" value when nothing else is possible.
pub struct Null {
pub:
	text string
	pos  token.Position
}

pub fn (n Null) str() string {
	return n.text
}

pub struct Quoted {
pub:
	text string
	pos  token.Position
}

pub fn (q Quoted) str() string {
	mut str := typeof(q).name + '{\n'
	str += '  text:  \'$q.text\'\n'
	str += '  pos:  $q.pos\n'
	str += '}'
	return str
}

pub struct Bare {
pub:
	text string
	pos  token.Position
}

pub fn (b Bare) str() string {
	mut str := typeof(b).name + '{\n'
	str += '  text:  \'$b.text\'\n'
	str += '  pos:  $b.pos\n'
	str += '}'
	return str
}

pub struct Bool {
pub:
	text string
	pos  token.Position
}

pub fn (b Bool) str() string {
	mut str := typeof(b).name + '{\n'
	str += '  text:  \'$b.text\'\n'
	str += '  pos:  $b.pos\n'
	str += '}'
	return str
}

pub struct Number {
pub:
	text string
	pos  token.Position
}

pub fn (n Number) str() string {
	mut str := typeof(n).name + '{\n'
	str += '  text:  \'$n.text\'\n'
	str += '  pos:  $n.pos\n'
	str += '}'
	return str
}

pub struct Date {
pub:
	text string
	pos  token.Position
}

pub fn (d Date) str() string {
	mut str := typeof(d).name + '{\n'
	str += '  text:  \'$d.text\'\n'
	str += '  pos:  $d.pos\n'
	str += '}'
	return str
}

pub struct Time {
pub:
	text   string
	offset int
	pos    token.Position
}

pub fn (t Time) str() string {
	mut str := typeof(t).name + '{\n'
	str += '  text:  \'$t.text\'\n'
	str += '  offset:  \'$t.offset\'\n'
	str += '  pos:  $t.pos\n'
	str += '}'
	return str
}

pub struct DateTime {
pub:
	text string
	pos  token.Position
	date Date
	time Time
}

pub fn (dt DateTime) str() string {
	mut str := typeof(dt).name + '{\n'
	str += '  text:  \'$dt.text\'\n'
	str += '  date:  \'$dt.date\'\n'
	str += '  time:  \'$dt.time\'\n'
	str += '  pos:  $dt.pos\n'
	str += '}'
	return str
}

pub struct EOF {
pub:
	pos token.Position
}

pub fn (e EOF) str() string {
	mut str := typeof(e).name + '{\n'
	str += '  pos:  $e.pos\n'
	str += '}'
	return str
}
