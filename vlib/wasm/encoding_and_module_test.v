module wasm

import math.bits

// The `wasm` module serialises a module into the WebAssembly binary format.
// These byte-level tests complement the modules validated in tests/ and cover
// the section encoders and public compile function without external tools.
//
// The expectations here are byte-level and derived from the spec or computed
// by hand, not measured off the implementation, so a green test means the
// encoding is right rather than merely self-consistent. Where a value is a
// floating-point bit pattern it is written as the little-endian byte sequence
// the spec mandates.

fn make_test_module() Module {
	return Module{
		functions: map[string]Function{}
	}
}

// push_f32 writes the value as four little-endian bytes.
fn test_push_f32_is_little_endian() {
	mut buf := []u8{}
	push_f32(mut buf, 1.5)
	// f32 1.5 is 0x3FC00000, least significant byte first.
	assert buf == [u8(0x00), 0x00, 0xc0, 0x3f]

	mut negative := []u8{}
	push_f32(mut negative, -1.5)
	// f32 -1.5 is 0xBFC00000, which must not collapse to the positive pattern.
	assert negative == [u8(0x00), 0x00, 0xc0, 0xbf]
	assert negative != buf
}

// push_f64 writes the value as eight little-endian bytes.
fn test_push_f64_is_little_endian() {
	mut buf := []u8{}
	push_f64(mut buf, 1.5)
	// f64 1.5 is 0x3FF8000000000000.
	assert buf == [u8(0x00), 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0x3f]

	mut negative := []u8{}
	push_f64(mut negative, -1.5)
	assert negative == [u8(0x00), 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0xbf]
}

// The round trip through the bit pattern must land back on the same float,
// which is what makes the byte assertions above meaningful.
fn test_push_float_round_trips_through_bits() {
	mut buf := []u8{}
	push_f32(mut buf, 0.15625)
	assert bits.f32_from_bits(u32(buf[0]) | u32(buf[1]) << 8 | u32(buf[2]) << 16 | u32(buf[3]) << 24) == 0.15625

	mut dbuf := []u8{}
	push_f64(mut dbuf, 0.15625)
	mut word := u64(0)
	for i, b in dbuf {
		word |= u64(b) << u64(8 * i)
	}
	assert bits.f64_from_bits(word) == 0.15625
}

fn test_result_type_encodes_count_then_each_type() {
	mut mod := make_test_module()
	mod.result_type([]ValType{})
	// Zero results is a bare count byte.
	assert mod.buf == [u8(0)]

	mod.buf = []u8{}
	mod.result_type([ValType.i32_t, ValType.f64_t])
	// Two results, then each type byte in order.
	assert mod.buf == [u8(2), u8(ValType.i32_t), u8(ValType.f64_t)]
}

fn test_function_type_encodes_0x60_then_both_result_vectors() {
	mut mod := make_test_module()
	mod.function_type(FuncType{
		parameters: [ValType.i32_t]
		results:    [ValType.i64_t]
	})
	assert mod.buf == [u8(0x60), u8(1), u8(ValType.i32_t), u8(1), u8(ValType.i64_t)]
}

fn test_global_type_encodes_value_type_then_mutability() {
	mut mod := make_test_module()
	mod.global_type(ValType.i32_t, false)
	assert mod.buf == [u8(ValType.i32_t), u8(0)]

	mod.buf = []u8{}
	mod.global_type(ValType.f64_t, true)
	assert mod.buf == [u8(ValType.f64_t), u8(1)]
}

// `name` is a leb128 length prefix followed by the raw bytes, no terminator.
fn test_name_encodes_length_then_bytes() {
	mut mod := make_test_module()
	mod.name('')
	assert mod.buf == [u8(0)]

	mod.buf = []u8{}
	mod.name('ab')
	assert mod.buf == [u8(2), u8(`a`), u8(`b`)]

	// A name long enough to need a two-byte leb128 length must not be
	// truncated to one byte.
	mod.buf = []u8{}
	long := 'x'.repeat(128)
	mod.name(long)
	assert mod.buf[0] == u8(0x80)
	assert mod.buf[1] == u8(0x01)
	assert mod.buf.len == 130
}

// start_section writes the section id and returns the offset where the length
// will be inserted later; it does not write a placeholder byte.
fn test_start_and_end_section_backfill_the_length() {
	mut mod := make_test_module()
	patch := mod.start_section(Section.type_section)
	assert mod.buf == [u8(Section.type_section)]

	mod.buf << [u8(0xde), 0xad, 0xbe, 0xef]
	mod.end_section(patch)
	// The id, then the length of everything after the insertion point.
	assert mod.buf == [u8(Section.type_section), u8(4), 0xde, 0xad, 0xbe, 0xef]
}

fn test_start_subsection_writes_id_and_patches_the_length_later() {
	mut mod := make_test_module()
	patch := mod.start_subsection(Subsection.name_function)
	assert mod.buf == [u8(Subsection.name_function)]

	mod.buf << [u8(1), 2, 3]
	mod.end_section(patch)
	assert mod.buf == [u8(Subsection.name_function), u8(3), 1, 2, 3]
}

// patch_len inserts, rather than overwrites, so the length prefix is written
// before the payload is known.
fn test_patch_len_inserts_and_keeps_the_payload() {
	mut mod := make_test_module()
	pos := mod.patch_start()
	assert pos == 0

	mod.buf << [u8(1), 2, 3, 4, 5]
	mod.patch_len(pos)
	assert mod.buf == [u8(5), 1, 2, 3, 4, 5]
}

fn test_patch_u32_inserts_the_encoded_value_at_the_position() {
	mut mod := make_test_module()
	pos := mod.patch_start()
	mod.buf << [u8(9), 9]
	mod.patch_u32(pos, 300)
	// 300 in leb128 is 0xAC 0x02, and the original payload follows untouched.
	assert mod.buf == [u8(0xac), u8(0x02), 9, 9]
}

fn test_u32_appends_leb128_in_place() {
	mut mod := make_test_module()
	mod.u32(0)
	mod.u32(127)
	mod.u32(128)
	mod.u32(300)
	assert mod.buf == [u8(0), u8(127), u8(0x80), u8(0x01), u8(0xac), u8(0x02)]
}

fn test_get_local_func_idx_adds_the_import_count() {
	mut mod := make_test_module()
	mod.functions['inner'] = Function{
		idx: 0
	}
	mod.fn_imports = [FunctionImport{}, FunctionImport{}]
	// Local indices are offset past the imported functions.
	assert mod.get_local_func_idx('inner') == 2
}

// NOTE: `get_local_func_idx` panics for an unknown name, and V has no
// assert_panics to catch it, so that defensive branch has no test here.

fn test_get_function_idx_resolves_a_local_call_patch() {
	mut mod := make_test_module()
	mod.functions['target'] = Function{
		idx: 5
	}
	// With no imports the local index passes through unchanged.
	assert mod.get_function_idx(FunctionCallPatch{
		name: 'target'
	}) == 5
}

fn test_get_function_idx_resolves_an_import_call_patch() {
	mut mod := make_test_module()
	mod.fn_imports = [
		FunctionImport{
			mod:  'env'
			name: 'first'
		},
		FunctionImport{
			mod:  'env'
			name: 'second'
		},
	]
	assert mod.get_function_idx(ImportCallPatch{
		mod:  'env'
		name: 'second'
	}) == 1
}

// NOTE: `get_function_idx` panics for an unknown import, which V cannot catch
// in a test, so that branch is untested here.

fn test_patch_emits_code_around_resolved_indices() {
	mut mod := make_test_module()
	mod.functions['callee'] = Function{
		idx: 3
	}
	mod.fn_imports = []FunctionImport{}

	ft := Function{
		patches: [CallPatch(FunctionCallPatch{
			name: 'callee'
			pos:  2
		})]
		code:    [u8(0xaa), 0xbb, 0xcc, 0xdd]
	}
	mod.patch(ft)
	// Everything before the patch position, then the resolved index (3), then the rest.
	assert mod.buf == [u8(0xaa), 0xbb, u8(3), 0xcc, 0xdd]
}

fn test_patch_with_no_patches_emits_code_verbatim() {
	mut mod := make_test_module()
	ft := Function{
		code: [u8(1), 2, 3]
	}
	mod.patch(ft)
	assert mod.buf == [u8(1), 2, 3]
}

fn test_constexpr_value_dispatches_on_the_value_type() {
	assert constexpr_value(7).code == [u8(0x41), 7]
	assert constexpr_value(i64(7)).code == [u8(0x42), 7]
	assert constexpr_value(0.5).code == [u8(0x44), 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xe0, 0x3f]
}

fn test_constexpr_value_zero_covers_every_permitted_type() {
	assert constexpr_value_zero(.i32_t).code == [u8(0x41), 0]
	assert constexpr_value_zero(.i64_t).code == [u8(0x42), 0]
	assert constexpr_value_zero(.f32_t).code == [u8(0x43), 0, 0, 0, 0]
	assert constexpr_value_zero(.f64_t).code == [u8(0x44), 0, 0, 0, 0, 0, 0, 0, 0]

	// Reference types become ref.null rather than a numeric zero.
	expr := constexpr_value_zero(.funcref_t)
	assert expr.code == [u8(0xd0), u8(RefType.funcref_t)]
}

// Every WebAssembly module starts with the eight magic bytes.
fn test_compile_emits_magic_and_version() {
	mut mod := Module{
		functions: map[string]Function{}
	}
	out := mod.compile()
	assert out.len >= 8
	// The magic word is "\0asm" and the version is 1.
	assert out[0] == 0x00
	assert out[1] == u8(`a`)
	assert out[2] == u8(`s`)
	assert out[3] == u8(`m`)
	assert out[4] == 0x01
	assert out[5] == 0x00
	assert out[6] == 0x00
	assert out[7] == 0x00
}
