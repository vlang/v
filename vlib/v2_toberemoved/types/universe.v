// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// [has_globals]
module types

// primitives
pub const bool_ = Primitive{
	props: .boolean
}
const i8_ = Primitive{
	props: .integer
	size:  8
}
const i16_ = Primitive{
	props: .integer
	size:  16
}
const i32_ = Primitive{
	props: .integer
	size:  32
}
// TODO: represent platform specific size
// will this be calculated at compile time?
pub const int_ = Primitive{
	props: .integer
	// size: 32
}
const i64_ = Primitive{
	props: .integer
	size:  64
}
const u8_ = Primitive{
	props: .integer | .unsigned
	size:  8
}
// byte_ = Primitive{props: .integer | .unsigned, size: 8}
const byte_ = Alias{
	name:      'byte'
	base_type: u8_
}
const u16_ = Primitive{
	props: .integer | .unsigned
	size:  16
}
const u32_ = Primitive{
	props: .integer | .unsigned
	size:  32
}
const u64_ = Primitive{
	props: .integer | .unsigned
	size:  64
}
const f32_ = Primitive{
	props: .float
	size:  32
}
pub const f64_ = Primitive{
	props: .float
	size:  64
}
// complex / non primitives
// String struct is defined in cmd/v2/builtin/string.v:
// pub struct string { str &u8, len int, is_lit int }
pub const string_ = String(0)
const chan_ = Channel{
	elem_type: none
}
const char_ = Char(0)
const isize_ = ISize(0)
const usize_ = USize(0)
const rune_ = Rune(0)
pub const void_ = Void(0)
const nil_ = Nil(0)
const none_ = None(0)
const byteptr_ = Alias{
	name:      'byteptr'
	base_type: Pointer{
		base_type: Type(byte_)
	}
}
const charptr_ = Alias{
	name:      'charptr'
	base_type: Pointer{
		base_type: Type(char_)
	}
}
pub const voidptr_ = Alias{
	name:      'voidptr'
	base_type: Pointer{
		base_type: Type(void_)
	}
}
const int_literal_ = Primitive{
	props: .untyped | .integer
}
const float_literal_ = Primitive{
	props: .untyped | .float
}
// int_literal_   = IntLiteral(0)
// float_literal_ = FloatLiteral(0)
// TODO: is this what thread should be?
const thread_ = Thread{
	elem_type: none
}

// NOTE: universe MUST be declared AFTER all primitive/alias/type constants above.
// For SSA-based backends (arm64, x64), struct literal consts need runtime init.
// init_universe() references bool_, int_, u8_, etc., so they must be initialized first.
const universe = init_universe()

pub fn builtin_type(name string) ?Type {
	return match name {
		'bool' { Type(bool_) }
		'i8' { Type(i8_) }
		'i16' { Type(i16_) }
		'i32' { Type(i32_) }
		'int' { Type(int_) }
		'i64' { Type(i64_) }
		'u8' { Type(u8_) }
		'byte' { Type(byte_) }
		'u16' { Type(u16_) }
		'u32' { Type(u32_) }
		'u64' { Type(u64_) }
		'f32' { Type(f32_) }
		'f64' { Type(f64_) }
		'string' { Type(string_) }
		'chan' { Type(chan_) }
		'char' { Type(char_) }
		'isize' { Type(isize_) }
		'usize' { Type(usize_) }
		'rune' { Type(rune_) }
		'void' { Type(void_) }
		'nil' { Type(nil_) }
		'none' { Type(none_) }
		'byteptr' { Type(byteptr_) }
		'charptr' { Type(charptr_) }
		'voidptr' { Type(voidptr_) }
		'int_literal' { Type(int_literal_) }
		'float_literal' { Type(float_literal_) }
		'thread' { Type(thread_) }
		else { return none }
	}
}

pub fn init_universe() &Scope {
	mut universe_ := new_scope(unsafe { nil })
	universe_.insert('bool', Type(bool_))
	universe_.insert('i8', Type(i8_))
	universe_.insert('i16', Type(i16_))
	universe_.insert('i32', Type(i32_))
	universe_.insert('int', Type(int_))
	universe_.insert('i64', Type(i64_))
	universe_.insert('u8', Type(u8_))
	universe_.insert('byte', Type(byte_))
	universe_.insert('u16', Type(u16_))
	universe_.insert('u32', Type(u32_))
	universe_.insert('u64', Type(u64_))
	universe_.insert('f32', Type(f32_))
	universe_.insert('f64', Type(f64_))
	// TODO:
	universe_.insert('string', Type(string_))
	universe_.insert('chan', Type(chan_))
	universe_.insert('char', Type(char_))
	universe_.insert('isize', Type(isize_))
	universe_.insert('usize', Type(usize_))
	universe_.insert('rune', Type(rune_))
	universe_.insert('void', Type(void_))
	universe_.insert('nil', Type(nil_))
	universe_.insert('none', Type(none_))
	universe_.insert('byteptr', Type(byteptr_))
	universe_.insert('charptr', Type(charptr_))
	universe_.insert('voidptr', Type(voidptr_))
	universe_.insert('int_literal', Type(int_literal_))
	universe_.insert('float_literal', Type(float_literal_))
	universe_.insert('float_literal', Type(float_literal_))
	universe_.insert('thread', Type(thread_))
	// Built-in marker interfaces consumed by the ownership checker
	// (`-d ownership`):
	//   * `Copy`  — explicit "always-copy" marker. Assignment of a value of
	//               this type never moves; the struct must not contain Owned
	//               fields. Useful to document intent and lock the type in.
	//   * `Owned` — explicit "move-on-assign" marker. A struct that
	//               implements `Owned` is tracked just like a `.to_owned()`
	//               string: assignment, struct-literal nesting, array/map
	//               literal nesting, and pass-by-value to a function all
	//               transfer ownership; reusing the source afterwards is a
	//               compile error.
	//   * `Drop`  — explicit "needs cleanup" marker. A struct that implements
	//               `Drop` MUST provide a `drop(mut self)` method. The checker
	//               schedules `var.drop()` for every owned, non-moved binding
	//               of such a type at scope exit (the schedule is exposed for
	//               codegen via `drop_schedule`).
	// All three are empty interfaces except `Drop`, which is satisfied by the
	// presence of a `drop(mut self)` method.
	universe_.insert('Copy', Type(Interface{
		name: 'Copy'
	}))
	universe_.insert('Owned', Type(Interface{
		name: 'Owned'
	}))
	universe_.insert('Drop', Type(Interface{
		name: 'Drop'
	}))
	return universe_
}
