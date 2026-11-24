module runtime

import wasm
import math.bits

fn test_builder_add_and_instance_isolation() ! {
	mut m := wasm.Module{}
	mut f := m.new_function('add', [.i32_t, .i32_t], [.i32_t])
	f.local_get(0)
	f.local_get(1)
	f.add(.i32_t)
	m.commit(f, true)
	mut bytes := m.compile()
	mod := load(bytes)!
	bytes.clear()
	mut instance := instantiate(mod, [], Config{})!
	assert instance.call('add', [Value.i32(20), Value.i32(22)])![0].as_i32() == 42
	assert instance.call('add', [Value.i32(0x7fffffff), Value.i32(1)])![0].as_i32() == -2147483648
	instance.call('add', [Value.i64(1), Value.i32(2)]) or {
		assert err.msg().contains('type mismatch')
		return
	}
	assert false
}

fn test_loop_and_recursive_calls() ! {
	mut m := wasm.Module{}
	mut f := m.new_function('sum', [.i32_t], [.i32_t])
	sum := f.new_local(.i32_t)
	done := f.c_block([], [])
	loop := f.c_loop([], [])
	f.local_get(0)
	f.eqz(.i32_t)
	f.c_br_if(done)
	f.local_get(sum)
	f.local_get(0)
	f.add(.i32_t)
	f.local_set(sum)
	f.local_get(0)
	f.i32_const(1)
	f.sub(.i32_t)
	f.local_set(0)
	f.c_br(loop)
	f.c_end(loop)
	f.c_end(done)
	f.local_get(sum)
	m.commit(f, true)
	mut rec := m.new_function('fac', [.i64_t], [.i64_t])
	rec.local_get(0)
	rec.eqz(.i64_t)
	cond := rec.c_if([], [.i64_t])
	rec.i64_const(1)
	rec.c_else(cond)
	rec.local_get(0)
	rec.local_get(0)
	rec.i64_const(1)
	rec.sub(.i64_t)
	rec.call('fac')
	rec.mul(.i64_t)
	rec.c_end(cond)
	m.commit(rec, true)
	mod := load(m.compile())!
	mut instance := instantiate(mod, [], Config{})!
	assert instance.call('sum', [Value.i32(1000)])![0].as_i32() == 500500
	assert instance.call('fac', [Value.i64(10)])![0].as_i64() == 3628800
}

fn host_double(mut instance Instance, args []Value) ![]Value {
	instance.write_memory(4, [u8(42)])!
	return [Value.i32(args[0].as_i32() * 2)]
}

fn test_host_import_and_memory() ! {
	mut m := wasm.Module{}
	m.assign_memory('memory', true, 1, 2)
	m.new_function_import('host', 'double', [.i32_t], [.i32_t])
	mut f := m.new_function('run', [.i32_t], [.i32_t])
	f.local_get(0)
	f.call_import('host', 'double')
	f.i32_const(4)
	f.load8(.i32_t, false, 0, 0)
	f.add(.i32_t)
	m.commit(f, true)
	mod := load(m.compile())!
	mut instance := instantiate(mod, [HostFunction{
		module:    'host'
		name:      'double'
		signature: Signature{ parameters: [.i32], results: [.i32] }
		callback:  host_double
	}], Config{})!
	assert instance.call('run', [Value.i32(10)])![0].as_i32() == 62
	assert instance.read_memory(4, 1)! == [u8(42)]
	assert mod.imports()[0].module == 'host'
	assert mod.imports()[0].signature.parameters == [ValueType.i32]
}

fn test_numeric_edges() ! {
	assert numeric(0x6a, 0xffffffff, 1)! == 0
	assert numeric(0x7e, ~u64(0), 2)! == ~u64(1)
	assert numeric(0x6f, 0x80000000, 0xffffffff)! == 0
	assert numeric(0x81, u64(1) << 63, ~u64(0))! == 0
	assert numeric(0x76, 0x80000000, 33)! == 0x40000000
	assert numeric(0x75, 0x80000000, 33)! == 0xc0000000
	assert numeric(0x8a, 1, 1)! == u64(1) << 63
	assert numeric(0x96, 0, 0x80000000)! == 0x80000000
	assert numeric(0x97, 0, 0x80000000)! == 0
	assert numeric(0x100, u64(bits.f32_bits(1.0e30)), 0)! == 0x7fffffff
	assert numeric(0x107, bits.f64_bits(-10.0), 0)! == 0
	assert numeric(0xa9, u64(bits.f32_bits(-0.5)), 0)! == 0
	assert numeric(0xc4, 0xffffffff, 0)! == ~u64(0)
	numeric(0x7f, u64(1) << 63, ~u64(0)) or {
		assert err.msg() == 'integer overflow'
		return
	}
	assert false
}
