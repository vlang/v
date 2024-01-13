// Copyright (c) 2020-2024 Joe Conigliaro. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.
// [has_globals]
module types

// __global universe = init_universe()
const universe = init_universe()

// primitives
const bool_ = Primitive{
	props: .boolean
}
const i8_ = Primitive{
	props: .integer
	size: 8
}
const i16_ = Primitive{
	props: .integer
	size: 16
}
const i32_ = Primitive{
	props: .integer
	size: 32
}
// TODO: represent platform specific size
// will this be calculated at compile time?
const int_ = Primitive{
	props: .integer
	// size: 32
}
const i64_ = Primitive{
	props: .integer
	size: 64
}
const u8_ = Primitive{
	props: .integer | .unsigned
	size: 8
}
// byte_ = Primitive{props: .integer | .unsigned, size: 8}
const byte_ = Alias{
	name: 'byte'
	base_type: u8_
}
const u16_ = Primitive{
	props: .integer | .unsigned
	size: 16
}
const u32_ = Primitive{
	props: .integer | .unsigned
	size: 32
}
const u64_ = Primitive{
	props: .integer | .unsigned
	size: 64
}
const f32_ = Primitive{
	props: .float
	size: 32
}
const f64_ = Primitive{
	props: .float
	size: 64
}
// complex / non primitives
const string_ = String(0)
const chan_ = Channel{}
const char_ = Char(0)
const isize_ = ISize(0)
const usize_ = USize(0)
const rune_ = Rune(0)
const void_ = Void(0)
const nil_ = Nil(0)
const none_ = None(0)
const byteptr_ = Alias{
	name: 'byteptr'
	base_type: Pointer{
		base_type: byte_
	}
}
const charptr_ = Alias{
	name: 'charptr'
	base_type: Pointer{
		base_type: char_
	}
}
const voidptr_ = Alias{
	name: 'voidptr'
	base_type: Pointer{
		base_type: void_
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
const thread_ = Thread{}

pub fn init_universe() &Scope {
	// universe scope
	mut universe_ := new_scope(unsafe { nil })
	universe_.insert('bool', Type(types.bool_))
	universe_.insert('i8', Type(types.i8_))
	universe_.insert('i16', Type(types.i16_))
	universe_.insert('i32', Type(types.i32_))
	universe_.insert('int', Type(types.int_))
	universe_.insert('i64', Type(types.i64_))
	universe_.insert('u8', Type(types.u8_))
	universe_.insert('byte', Type(types.byte_))
	universe_.insert('u16', Type(types.u16_))
	universe_.insert('u32', Type(types.u32_))
	universe_.insert('u64', Type(types.u64_))
	universe_.insert('f32', Type(types.f32_))
	universe_.insert('f64', Type(types.f64_))
	// TODO:
	universe_.insert('string', Type(types.string_))
	universe_.insert('chan', Type(types.chan_))
	universe_.insert('char', Type(types.char_))
	universe_.insert('isize', Type(types.isize_))
	universe_.insert('usize', Type(types.usize_))
	universe_.insert('rune', Type(types.rune_))
	universe_.insert('void', Type(types.void_))
	universe_.insert('nil', Type(types.nil_))
	universe_.insert('none', Type(types.nil_))
	universe_.insert('byteptr', Type(types.byteptr_))
	universe_.insert('charptr', Type(types.charptr_))
	universe_.insert('voidptr', Type(types.voidptr_))
	universe_.insert('int_literal', Type(types.int_literal_))
	universe_.insert('float_literal', Type(types.float_literal_))
	universe_.insert('float_literal', Type(types.float_literal_))
	universe_.insert('thread', Type(types.thread_))
	return universe_
}
