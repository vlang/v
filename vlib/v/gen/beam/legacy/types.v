module beam

import v.ast

// V to Erlang type mapping:
// V int       -> Erlang integer
// V f64       -> Erlang float
// V string    -> Erlang binary (<<...>>)
// V bool      -> Erlang atom (true/false)
// V []T       -> Erlang list
// V struct    -> Erlang map with {vbeam, type} tag
// V map[K]V   -> Erlang map
// V enum      -> Erlang atom or tagged tuple

fn (g Gen) type_to_erl(typ ast.Type) string {
	sym := g.table.sym(typ)
	return match sym.kind {
		.int, .i8, .i16, .i32, .i64 { 'integer' }
		.u8, .u16, .u32, .u64 { 'integer' }
		.int_literal { 'integer' }
		.f32, .f64 { 'float' }
		.float_literal { 'float' }
		.string { 'binary' }
		.bool { 'atom' }
		.array { 'list' }
		.map { 'map' }
		.struct { 'map' }
		.enum { 'atom' }
		else { 'any' }
	}
}

fn (g Gen) is_numeric_type(typ ast.Type) bool {
	sym := g.table.sym(typ)
	return match sym.kind {
		.int, .i8, .i16, .i32, .i64 { true }
		.u8, .u16, .u32, .u64 { true }
		.int_literal { true }
		.f32, .f64 { true }
		.float_literal { true }
		else { false }
	}
}

fn (g Gen) is_int_type(typ ast.Type) bool {
	sym := g.table.sym(typ)
	return match sym.kind {
		.int, .i8, .i16, .i32, .i64 { true }
		.u8, .u16, .u32, .u64 { true }
		.int_literal { true }
		else { false }
	}
}

fn (g Gen) is_float_type(typ ast.Type) bool {
	sym := g.table.sym(typ)
	return match sym.kind {
		.f32, .f64 { true }
		.float_literal { true }
		else { false }
	}
}

fn (g Gen) is_string_type(typ ast.Type) bool {
	sym := g.table.sym(typ)
	return sym.kind == .string
}

fn (g Gen) is_bool_type(typ ast.Type) bool {
	sym := g.table.sym(typ)
	return sym.kind == .bool
}

// Conversion function names for println
fn (g Gen) to_binary_fn(typ ast.Type) string {
	sym := g.table.sym(typ)
	return match sym.kind {
		.int, .i8, .i16, .i32, .i64 { 'integer_to_binary' }
		.u8, .u16, .u32, .u64 { 'integer_to_binary' }
		.int_literal { 'integer_to_binary' }
		.f32, .f64 { 'float_to_binary' }
		.float_literal { 'float_to_binary' }
		.string { '' } // Already binary
		.bool { 'atom_to_binary' }
		else { 'vbeam_conv:to_binary' }
	}
}
