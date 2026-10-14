module wasm

// Coverage for the `Function` instruction emitters. Each one appends a
// WebAssembly opcode and its leb128 operands to `func.code`, so the tests
// assert the exact byte sequence against the opcode numbers in the spec
// (https://webassembly.github.io/spec/core/binary/instructions.html), not
// against whatever the implementation happens to produce.

fn make_test_function() Function {
	return Function{
		locals: []FunctionLocal{}
	}
}

fn test_const_instructions_emit_opcode_then_operand() {
	mut func := make_test_function()

	func.i32_const(-1)
	// 0x41 i32.const, then the leb128 signed encoding of -1.
	assert func.code == [u8(0x41), 0x7f]

	func.code = []u8{}
	func.i64_const(0)
	assert func.code == [u8(0x42), 0x00]

	func.code = []u8{}
	func.f32_const(1.5)
	assert func.code == [u8(0x43), 0x00, 0x00, 0xc0, 0x3f]

	func.code = []u8{}
	func.f64_const(1.5)
	assert func.code == [u8(0x44), 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x3f]
}

fn test_local_instructions_emit_opcode_then_index() {
	mut func := make_test_function()

	func.local_get(0)
	assert func.code == [u8(0x20), 0x00]

	func.code = []u8{}
	func.local_set(1)
	assert func.code == [u8(0x21), 0x01]

	func.code = []u8{}
	func.local_tee(2)
	assert func.code == [u8(0x22), 0x02]

	// An index past 127 needs a two-byte leb128, not a truncated byte.
	func.code = []u8{}
	func.local_get(300)
	assert func.code == [u8(0x20), 0xac, 0x02]
}

fn test_global_get_emits_index_for_an_imported_global() {
	mut func := make_test_function()
	func.global_get(GlobalImportIndex(3))
	assert func.code == [u8(0x23), 0x03]

	func.code = []u8{}
	func.global_set(GlobalImportIndex(0))
	assert func.code == [u8(0x24), 0x00]
}

// A local global is resolved at compile time, so `global_get` records a patch
// with the position rather than emitting the index.
fn test_global_get_records_a_patch_for_a_local_global() {
	mut func := make_test_function()
	func.global_get(GlobalIndex(2))
	assert func.code == [u8(0x23)]
	assert func.patches.len == 1
	match func.patches[0] {
		FunctionGlobalPatch {
			assert func.patches[0].idx == GlobalIndex(2)
			// The position is where the index will be written in.
			assert func.patches[0].pos == 1
		}
		else {
			assert false, 'a local global must produce a FunctionGlobalPatch'
		}
	}

	func.code = []u8{}
	func.global_set(GlobalIndex(1))
	assert func.patches.len == 2
}

fn test_drop_and_select_emit_bare_opcodes() {
	mut func := make_test_function()
	func.drop()
	assert func.code == [u8(0x1a)]

	func.code = []u8{}
	func.c_select()
	assert func.code == [u8(0x1b)]
}

// The arithmetic families are contiguous per type, and the opcode depends on
// the `NumType` passed in, so the four types must land in different blocks.
fn test_arithmetic_opcodes_differ_per_type() {
	mut func := make_test_function()

	func.add(.i32_t)
	func.sub(.i32_t)
	func.mul(.i32_t)
	// 0x6a add, 0x6b sub, 0x6c mul.
	assert func.code == [u8(0x6a), 0x6b, 0x6c]

	func.code = []u8{}
	func.add(.i64_t)
	func.sub(.i64_t)
	func.mul(.i64_t)
	// 0x7c add, 0x7d sub, 0x7e mul.
	assert func.code == [u8(0x7c), 0x7d, 0x7e]

	func.code = []u8{}
	func.add(.f32_t)
	func.sub(.f32_t)
	func.mul(.f32_t)
	assert func.code == [u8(0x92), 0x93, 0x94]

	func.code = []u8{}
	func.add(.f64_t)
	func.sub(.f64_t)
	func.mul(.f64_t)
	assert func.code == [u8(0xa0), 0xa1, 0xa2]
}

fn test_bitwise_opcodes_are_contiguous_per_type() {
	mut func := make_test_function()

	func.b_and(.i32_t)
	func.b_or(.i32_t)
	func.b_xor(.i32_t)
	func.b_shl(.i32_t)
	func.b_shr(.i32_t, false)
	// 0x71 and, 0x72 or, 0x73 xor, 0x74 shl, 0x76 shr_u.
	assert func.code == [u8(0x71), 0x72, 0x73, 0x74, 0x76]

	func.code = []u8{}
	func.b_shr(.i32_t, true)
	// 0x75 shr_s, which must differ from shr_u.
	assert func.code == [u8(0x75)]

	func.code = []u8{}
	func.b_and(.i64_t)
	func.b_or(.i64_t)
	func.b_xor(.i64_t)
	assert func.code == [u8(0x83), 0x84, 0x85]
}

fn test_unary_count_opcodes() {
	mut func := make_test_function()

	func.clz(.i32_t)
	func.ctz(.i32_t)
	func.popcnt(.i32_t)
	// 0x67 clz, 0x68 ctz, 0x69 popcnt.
	assert func.code == [u8(0x67), 0x68, 0x69]

	func.code = []u8{}
	func.clz(.i64_t)
	func.ctz(.i64_t)
	func.popcnt(.i64_t)
	assert func.code == [u8(0x79), 0x7a, 0x7b]
}

fn test_comparison_opcodes_differ_per_type() {
	mut func := make_test_function()

	func.eqz(.i32_t)
	func.eq(.i32_t)
	func.ne(.i32_t)
	func.lt(.i32_t, true)
	func.gt(.i32_t, true)
	func.le(.i32_t, true)
	func.ge(.i32_t, true)
	// 0x45 eqz, 0x46 eq, 0x47 ne, 0x48 lt_s, 0x4a gt_s, 0x4c le_s, 0x4e ge_s.
	assert func.code == [u8(0x45), 0x46, 0x47, 0x48, 0x4a, 0x4c, 0x4e]

	// Unsigned comparisons use different opcodes, and the signed/unsigned
	// distinction is exactly the kind of thing that regresses silently.
	func.code = []u8{}
	func.lt(.i32_t, false)
	func.gt(.i32_t, false)
	func.le(.i32_t, false)
	func.ge(.i32_t, false)
	// 0x49 lt_u, 0x4b gt_u, 0x4d le_u, 0x4f ge_u.
	assert func.code == [u8(0x49), 0x4b, 0x4d, 0x4f]
}

fn test_float_unary_opcodes_differ_per_type() {
	mut func := make_test_function()

	func.abs(.f32_t)
	func.neg(.f32_t)
	func.ceil(.f32_t)
	func.floor(.f32_t)
	func.trunc(.f32_t)
	func.nearest(.f32_t)
	func.sqrt(.f32_t)
	assert func.code == [u8(0x8b), 0x8c, 0x8d, 0x8e, 0x8f, 0x90, 0x91]

	func.code = []u8{}
	func.abs(.f64_t)
	func.neg(.f64_t)
	assert func.code == [u8(0x99), 0x9a]
}

fn test_control_flow_opcodes() {
	mut func := make_test_function()

	func.unreachable()
	assert func.code == [u8(0x00)]

	func.code = []u8{}
	func.nop()
	assert func.code == [u8(0x01)]

	func.code = []u8{}
	func.c_return()
	assert func.code == [u8(0x0f)]
}

fn test_new_local_returns_sequential_indices_and_stores_the_type() {
	mut func := make_test_function()

	assert func.new_local(.i32_t) == 0
	assert func.new_local(.i64_t) == 1
	assert func.new_local(.f32_t) == 2
	// The index is the position before the append, so the third call saw two.
	assert func.locals.len == 3
	assert func.locals[0].typ == .i32_t
	assert func.locals[1].typ == .i64_t
	assert func.locals[2].typ == .f32_t
}

fn test_new_local_named_keeps_the_name_for_debug_info() {
	mut func := make_test_function()

	idx := func.new_local_named(.i32_t, 'counter')
	assert idx == 0
	assert func.locals[0].name? == 'counter'
	assert func.locals[0].typ == .i32_t
}

fn test_call_emits_opcode_then_records_a_patch() {
	mut func := make_test_function()
	func.patches = []

	// `call` records the target as a patch resolved at compile time, so the
	// index is absent from the emitted code.
	func.call('target')
	assert func.code == [u8(0x10)]
	assert func.patches.len == 1
	match func.patches[0] {
		FunctionCallPatch {
			assert func.patches[0].name == 'target'
		}
		ImportCallPatch {
			assert false, 'a local call must not resolve to an import patch'
		}
		FunctionGlobalPatch {
			assert false, 'a local call must not resolve to a global patch'
		}
	}
}
