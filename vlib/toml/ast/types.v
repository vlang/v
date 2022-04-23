// Copyright (c) 2021 Lars Pontoppidan. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
module ast

import toml.token
import strconv

// Key is a sumtype representing all types of keys that
// can be found in a TOML document.
pub type Key = Bare | Bool | Null | Number | Quoted

pub fn (k Key) str() string {
	return k.text
}

// Value is a sumtype representing all possible value types
// found in a TOML document.
pub type Value = Bool
	| Date
	| DateTime
	| Null
	| Number
	| Quoted
	| Time
	| []Value
	| map[string]Value

// str outputs the value in JSON-like format for eased
// debugging
pub fn (v Value) str() string {
	match v {
		Quoted, Date, DateTime, Time {
			return '"$v.text"'
		}
		Bool, Null, Number {
			return v.text
		}
		map[string]Value {
			mut str := '{'
			for key, val in v {
				str += ' "$key": $val,'
			}
			str = str.trim_right(',')
			str += ' }'
			return str
		}
		[]Value {
			mut str := '['
			for val in v {
				str += ' $val,'
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

// str returns the `string` representation of the `DateTimeType` type.
pub fn (dtt DateTimeType) str() string {
	return dtt.text
}

// Comment is the data representation of a TOML comment (`# This is a comment`).
pub struct Comment {
pub:
	text string
	pos  token.Pos
}

// str returns the `string` representation of the `Comment` type.
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
	pos  token.Pos
}

// str returns the `string` representation of the `Null` type
pub fn (n Null) str() string {
	return n.text
}

// Quoted is the data representation of a TOML quoted type (`"quoted-key" = "I'm a quoted value"`).
// Quoted types can appear both as keys and values in TOML documents.
pub struct Quoted {
pub mut:
	text string
pub:
	pos          token.Pos
	is_multiline bool
	quote        u8
}

// str returns the `string` representation of the `Quoted` type.
pub fn (q Quoted) str() string {
	mut str := typeof(q).name + '{\n'
	str += '  text:  \'$q.text\'\n'
	str += '  pos:  $q.pos\n'
	str += '  is_multiline:  $q.is_multiline\n'
	str += '  quote: \'$q.quote\'\n'
	str += '}'
	return str
}

// Bare is the data representation of a TOML bare type (`bare_key = ...`).
// Bare types can appear only as keys in TOML documents. Otherwise they take the
// form of Bool or Numbers.
pub struct Bare {
pub:
	text string
	pos  token.Pos
}

// str returns the `string` representation of the `Bare` type.
pub fn (b Bare) str() string {
	mut str := typeof(b).name + '{\n'
	str += '  text:  \'$b.text\'\n'
	str += '  pos:  $b.pos\n'
	str += '}'
	return str
}

// Bool is the data representation of a TOML boolean type (`... = true`).
// Bool types can appear only as values in TOML documents. Keys named `true` or `false`
// are considered as Bare types.
pub struct Bool {
pub:
	text string
	pos  token.Pos
}

// str returns the `string` representation of the `Bool` type.
pub fn (b Bool) str() string {
	mut str := typeof(b).name + '{\n'
	str += '  text:  \'$b.text\'\n'
	str += '  pos:  $b.pos\n'
	str += '}'
	return str
}

// Number is the data representation of a TOML number type (`25 = 5e2`).
// Number types can appear both as keys and values in TOML documents.
// Number can be integers, floats, infinite, NaN - they can have exponents (`5e2`) and be sign prefixed (`+2`).
pub struct Number {
pub:
	pos token.Pos
pub mut:
	text string
}

// str returns the `string` representation of the `Number` type.
pub fn (n Number) str() string {
	mut str := typeof(n).name + '{\n'
	str += '  text:  \'$n.text\'\n'
	str += '  pos:  $n.pos\n'
	str += '}'
	return str
}

// i64 returns the `n Number` as an `i64` value.
pub fn (n Number) i64() i64 {
	if n.text.starts_with('0x') {
		hex := n.text.all_after('0x').to_upper().replace('_', '')
		return strconv.parse_int(hex, 16, 64) or { i64(0) }
	} else if n.text.starts_with('0o') {
		oct := n.text.all_after('0o').replace('_', '')
		return strconv.parse_int(oct, 8, 64) or { i64(0) }
	} else if n.text.starts_with('0b') {
		bin := n.text.all_after('0b').replace('_', '')
		return strconv.parse_int(bin, 2, 64) or { i64(0) }
	}
	return strconv.parse_int(n.text, 0, 64) or { i64(0) }
}

// f64 returns the `n Number` as an `f64` value.
pub fn (n Number) f64() f64 {
	return n.text.replace('_', '').f64()
}

// Date is the data representation of a TOML date type (`YYYY-MM-DD`).
// Date types can appear both as keys and values in TOML documents.
// Keys named like dates e.g. `1980-12-29` are considered Bare key types.
pub struct Date {
pub:
	text string
	pos  token.Pos
}

// str returns the `string` representation of the `Date` type.
pub fn (d Date) str() string {
	mut str := typeof(d).name + '{\n'
	str += '  text:  \'$d.text\'\n'
	str += '  pos:  $d.pos\n'
	str += '}'
	return str
}

// Time is the data representation of a TOML time type (`HH:MM:SS.milli`).
// Time types can appear only as values in TOML documents.
pub struct Time {
pub:
	text   string
	offset int
	pos    token.Pos
}

// str returns the `string` representation of the `Time` type.
pub fn (t Time) str() string {
	mut str := typeof(t).name + '{\n'
	str += '  text:  \'$t.text\'\n'
	str += '  offset:  \'$t.offset\'\n'
	str += '  pos:  $t.pos\n'
	str += '}'
	return str
}

// DateTime is the data representation of a TOML date-time type (`YYYY-MM-DDTHH:MM:SS.milli`).
// DateTime types can appear only as values in TOML documents.
pub struct DateTime {
pub mut:
	text string
pub:
	pos  token.Pos
	date Date
	time Time
}

// str returns the `string` representation of the `DateTime` type.
pub fn (dt DateTime) str() string {
	mut str := typeof(dt).name + '{\n'
	str += '  text:  \'$dt.text\'\n'
	str += '  date:  \'$dt.date\'\n'
	str += '  time:  \'$dt.time\'\n'
	str += '  pos:  $dt.pos\n'
	str += '}'
	return str
}

// EOF is the data representation of the end of the TOML document.
pub struct EOF {
pub:
	pos token.Pos
}

// str returns the `string` representation of the `EOF` type.
pub fn (e EOF) str() string {
	mut str := typeof(e).name + '{\n'
	str += '  pos:  $e.pos\n'
	str += '}'
	return str
}
