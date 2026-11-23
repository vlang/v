module runtime

import encoding.leb128

fn encode_section(id u8, data []u8) []u8 {
	mut bytes := [id]
	bytes << leb128.encode_u32(u32(data.len))
	bytes << data
	return bytes
}

fn raw_module(parameters []u8, results []u8, instructions []u8) []u8 {
	mut types := [u8(1), 0x60, u8(parameters.len)]
	types << parameters
	types << u8(results.len)
	types << results
	mut body := [u8(0)]
	body << instructions
	mut code := [u8(1)]
	code << leb128.encode_u32(u32(body.len))
	code << body
	mut bytes := [u8(0), 0x61, 0x73, 0x6d, 1, 0, 0, 0]
	bytes << encode_section(1, types)
	bytes << encode_section(3, [u8(1), 0])
	bytes << encode_section(7, [u8(1), 1, `f`, 0, 0])
	bytes << encode_section(10, code)
	return bytes
}

fn reject(bytes []u8) {
	load(bytes) or {
		assert err.msg().len > 0
		return
	}
	assert false, 'accepted invalid binary: ${bytes.hex()}'
}

fn test_reject_malformed_modules_and_instructions() {
	valid := raw_module([], [u8(0x7f)], [u8(0x41), 42, 0x0b])
	// Every truncated prefix must be rejected, except a complete empty module.
	for n in 0 .. valid.len {
		if n in [8, 15] { continue } // complete header and type-only module
		reject(valid[..n])
	}
	for body in [
		[u8(0x6a), 0x0b], // stack underflow
		[u8(0x42), 0, 0x0b], // wrong return type
		[u8(0x41), 0, 0x41, 1, 0x0b], // extra return value
		[u8(0x20), 0, 0x0b], // invalid local
		[u8(0x10), 1, 0x0b], // invalid callee
		[u8(0x0c), 1, 0x0b], // invalid label
		[u8(0x41), 0, 0x0b, 1], // trailing code
		[u8(0x41), 0x80, 0x80, 0x80, 0x80, 0x08, 0x0b], // signed LEB overflow
		[u8(0x41), 0, 0x28, 2, 0, 0x0b], // missing memory
		[u8(0x05), 0x0b], // misplaced else
		[u8(0xfd), 0, 0x0b], // unsupported SIMD
	] {
		reject(raw_module([], [u8(0x7f)], body))
	}
	mut duplicate := valid.clone()
	duplicate << encode_section(1, [u8(0)])
	reject(duplicate)
}

fn test_multivalue_branches_and_typed_loop_parameters() ! {
	// A type-index block takes and returns two values, jumping over unreachable code.
	bytes := raw_module([u8(0x7f), 0x7e], [u8(0x7f), 0x7e], [
		u8(0x20),
		0,
		0x20,
		1,
		0x02,
		0,
		0x0c,
		0,
		0,
		0x0b,
		0x0b,
	])
	mut vm := instantiate(load(bytes)!, [], Config{})!
	assert vm.call('f', [Value.i32(7), Value.i64(9)])! == [Value.i32(7), Value.i64(9)]
	// Branch to the implicit function label, retaining the result.
	mut root := instantiate(load(raw_module([], [u8(0x7f)], [u8(0x41), 42, 0x0c, 0, 0x0b]))!, [], Config{})!
	assert root.call('f', [])![0].as_i32() == 42
	// A br_table takes both the explicit and default edge to the same result block.
	mut table := instantiate(load(raw_module([u8(0x7f)], [u8(0x7f)], [
		u8(0x02),
		0x7f,
		0x41,
		42,
		0x20,
		0,
		0x0e,
		1,
		0,
		0,
		0x0b,
		0x0b,
	]))!, [], Config{})!
	assert table.call('f', [Value.i32(0)])![0].as_i32() == 42
	assert table.call('f', [Value.i32(-1)])![0].as_i32() == 42
}

fn test_traps_and_resource_limits_are_recoverable() ! {
	mod := load(raw_module([u8(0x7f), 0x7f], [u8(0x7f)], [u8(0x20), 0, 0x20, 1, 0x6d, 0x0b]))!
	mut vm := instantiate(mod, [], Config{})!
	for args in [[Value.i32(1), Value.i32(0)], [Value.i32(-2147483648), Value.i32(-1)]] {
		mut trapped := false
		vm.call('f', args) or { trapped = true }
		assert trapped
		assert vm.call('f', [Value.i32(6), Value.i32(2)])![0].as_i32() == 3
	}
	mut infinite := instantiate(load(raw_module([], [], [u8(3), 0x40, 0x0c, 0, 0x0b, 0x0b]))!, [],
		fuel: 100
	)!
	mut exhausted := false
	infinite.call('f', []) or {
		assert err.msg() == 'execution fuel exhausted'
		exhausted = true
	}
	assert exhausted
	mut recursive := instantiate(load(raw_module([], [], [u8(0x10), 0, 0x0b]))!, [],
		max_call_depth: 8
	)!
	exhausted = false
	recursive.call('f', []) or {
		assert err.msg() == 'call stack limit exceeded'
		exhausted = true
	}
	assert exhausted
}

fn test_leb_boundaries_and_empty_sections() ! {
	mut vm := instantiate(load(raw_module([], [u8(0x7e)], [u8(0x42), 0x80, 0x80, 0x80, 0x80, 0x80,
		0x80, 0x80, 0x80, 0x80, 0x7f, 0x0b]))!, [], Config{})!
	assert vm.call('f', [])![0].bits == u64(1) << 63
	mut bytes := [u8(0), 0x61, 0x73, 0x6d, 1, 0, 0, 0]
	bytes << encode_section(4, [u8(0)])
	bytes << encode_section(5, [u8(0)])
	load(bytes)!
}
