module wasm

import os
import v.parser
import v.pref
import v.ssa
import v.ssa.optimize
import v.types

fn ssa_wasm_test_dir(name string) string {
	dir := os.join_path(os.vtmp_dir(), 'ssa_wasm_${name}_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	return dir
}

fn assert_ssa_wasm_execution(path string, checks string) {
	bytes := os.read_bytes(path) or { panic(err) }
	assert bytes.len > 8
	assert bytes[..8] == [u8(0), 97, 115, 109, 1, 0, 0, 0]
	node := os.find_abs_path_of_executable('node') or {
		eprintln('> node unavailable; skipping SSA WebAssembly execution')
		return
	}
	runner := os.join_path(os.dir(path), 'check.mjs')
	os.write_file(runner, "import assert from 'node:assert/strict';\nimport { readFileSync } from 'node:fs';\nconst bytes = readFileSync(process.argv[2]);\nassert.ok(WebAssembly.validate(bytes));\nconst { instance } = await WebAssembly.instantiate(bytes, {});\nconst e = instance.exports;\nassert.ok(e.memory instanceof WebAssembly.Memory);\n${checks}\n") or {
		panic(err)
	}
	result := os.exec([node, runner, path])
	assert result.exit_code == 0, result.output
}

fn test_ssa_wasm_numeric_source_before_and_after_optimization() {
	dir := ssa_wasm_test_dir('numeric')
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'numeric.v')
	os.write_file(source, '
module main

pub fn sum(n int) int {
	mut total := 0
	for i := 0; i < n; i++ {
		if i % 2 == 0 {
			total += i
		} else {
			total -= i
		}
	}
	return total
}


pub fn fib(n int) int {
	if n < 2 {
		return n
	}
	return fib(n - 1) + fib(n - 2)
}

pub fn signed(a i64, b i64) i64 {
	return a / b + a % b
}

pub fn unsigned(a u64, b u64) u64 {
	return a / b
}

pub fn unsigned_less(a u64, b u64) bool {
	return a < b
}

pub fn f32_math(a f32, b f32) f32 {
	return a * b + b
}

pub fn f64_math(a f64, b f64) f64 {
	return a / b - b
}

pub fn signed_narrow(n int) int {
	return int(i8(n))
}

pub fn unsigned_narrow(n int) int {
	return int(u8(n))
}

pub fn write(value int) int {
	return value + 7
}

pub fn call_write(value int) int {
	return write(value)
}

pub fn narrow_logical_shift_assign() int {
	mut x := i8(-5)
	x >>>= 1
	mut y := i16(-5)
	y >>>= 1
	return int(x) + int(y)
}

pub fn labeled_loops() int {
	mut cb := 0
	outer: for i := 0; i < 3; i++ {
		for j := 0; j < 3; j++ {
			cb++
			break outer
		}
	}
	mut cc := 0
	again: for i := 0; i < 3; i++ {
		for j := 0; j < 3; j++ {
			cc++
			continue again
		}
	}
	mut cn := 0
	for i := 0; i < 3; i++ {
		for j := 0; j < 3; j++ {
			cn++
			break
		}
	}
	return cb * 100 + cc * 10 + cn
}
') or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(source)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	exports := {
		'sum':                         'sum'
		'fib':                         'fib'
		'signed':                      'signed'
		'unsigned':                    'unsigned'
		'unsigned_less':               'unsigned_less'
		'f32_math':                    'f32_math'
		'f64_math':                    'f64_math'
		'signed_narrow':               'signed_narrow'
		'unsigned_narrow':             'unsigned_narrow'
		'write':                       'write'
		'call_write':                  'call_write'
		'narrow_logical_shift_assign': 'narrow_logical_shift_assign'
		'labeled_loops':               'labeled_loops'
	}
	checks := '
assert.equal(e.sum(6), -3);
assert.equal(e.sum(7), 3);
assert.equal(e.fib(10), 55);
assert.equal(e.signed(-11n, 3n), -5n);
assert.equal(e.unsigned(-1n, 3n), 6148914691236517205n);
assert.equal(e.unsigned_less(-1n, 1n), 0);
assert.equal(e.unsigned_less(1n, -1n), 1);
assert.equal(e.f32_math(1.5, 2), 5);
assert.equal(e.f64_math(12.5, 2), 4.25);
assert.equal(e.signed_narrow(128), -128);
assert.equal(e.signed_narrow(255), -1);
assert.equal(e.unsigned_narrow(256), 0);
assert.equal(e.unsigned_narrow(-1), 255);
assert.equal(e.write(3), 10);
assert.equal(e.call_write(4), 11);
assert.equal(e.narrow_logical_shift_assign(), 32890);
assert.equal(e.labeled_loops(), 133);
'
	for production in [false, true] {
		mut m := ssa.build_with_options(a, map[string]bool{}, &tc, ssa.BuildOptions{
			target: ssa.TargetData{ ptr_size: 4 }
		})
		if production {
			optimize.optimize(mut m)
		}
		mut g := SSAGen.new(m)
		g.configure(exports, []string{}, '')
		g.gen() or { panic(err) }
		path := os.join_path(dir, 'numeric_${production}.wasm')
		g.write(path) or { panic(err) }
		assert_ssa_wasm_execution(path, checks)
	}
}

fn ssa_wasm_swap_loop() &ssa.Module {
	mut m := ssa.Module.new()
	m.target = ssa.TargetData{ ptr_size: 4 }
	i32_type := m.type_store.get_int(32)
	bool_type := m.type_store.get_int(1)
	func := m.new_function('swap_loop', i32_type)
	n := m.add_value(.argument, i32_type, 'n', 0)
	m.func_add_param(func, n)
	entry := m.add_block(func, 'entry')
	header := m.add_block(func, 'header')
	body := m.add_block(func, 'body')
	done := m.add_block(func, 'done')
	zero := m.get_or_add_const(i32_type, '0')
	one := m.get_or_add_const(i32_type, '1')
	two := m.get_or_add_const(i32_type, '2')
	ten := m.get_or_add_const(i32_type, '10')
	m.add_instr(.jmp, entry, 0, [ssa.ValueID(header)])
	a := m.add_instr(.phi, header, i32_type, [])
	b := m.add_instr(.phi, header, i32_type, [])
	i := m.add_instr(.phi, header, i32_type, [])
	condition := m.add_instr(.lt, header, bool_type, [i, n])
	m.add_instr(.br, header, 0, [condition, ssa.ValueID(body), ssa.ValueID(done)])
	next_i := m.add_instr(.add, body, i32_type, [i, one])
	m.add_instr(.jmp, body, 0, [ssa.ValueID(header)])
	m.append_phi_operands(a, one, entry)
	m.append_phi_operands(a, b, body)
	m.append_phi_operands(b, two, entry)
	m.append_phi_operands(b, a, body)
	m.append_phi_operands(i, zero, entry)
	m.append_phi_operands(i, next_i, body)
	left := m.add_instr(.mul, done, i32_type, [a, ten])
	result := m.add_instr(.add, done, i32_type, [left, b])
	m.add_instr(.ret, done, 0, [result])
	// Block order differs from control-flow order; the backedge swaps both phis.
	mut function := m.funcs[func]
	function.blocks = [entry, done, body, header]
	m.funcs[func] = function
	return m
}

fn test_ssa_wasm_loop_phi_parallel_copies() {
	dir := ssa_wasm_test_dir('phi')
	defer { os.rmdir_all(dir) or {} }
	for production in [false, true] {
		mut m := ssa_wasm_swap_loop()
		if production {
			optimize.optimize(mut m)
			mut has_assign := false
			for block in m.blocks {
				for id in block.instrs {
					if m.values[id].kind == .instruction {
						instruction := m.instrs[m.values[id].index]
						assert instruction.op != .phi
						has_assign = has_assign || instruction.op == .assign
					}
				}
			}
			assert has_assign
		}
		mut g := SSAGen.new(m)
		g.gen() or { panic(err) }
		path := os.join_path(dir, 'phi_${production}.wasm')
		g.write(path) or { panic(err) }
		assert_ssa_wasm_execution(path, '
assert.equal(e.swap_loop(0), 12);
assert.equal(e.swap_loop(1), 21);
assert.equal(e.swap_loop(2), 12);
assert.equal(e.swap_loop(3), 21);
assert.equal(e.swap_loop(100), 12);
')
	}
}

fn test_ssa_wasm_reports_unsupported_instruction() {
	mut m := ssa.Module.new()
	m.target = ssa.TargetData{ ptr_size: 4 }
	func := m.new_function('unsupported', 0)
	entry := m.add_block(func, 'entry')
	m.add_instr(.go_call, entry, 0, [])
	m.add_instr(.ret, entry, 0, [])
	mut g := SSAGen.new(m)
	g.gen() or {
		assert err.msg().contains('unsupported'), err.msg()
		assert err.msg().contains('go_call'), err.msg()
		return
	}
	assert false, 'unsupported SSA instructions must fail WebAssembly generation'
}

fn ssa_wasm_aggregate_swap_loop() &ssa.Module {
	mut m := ssa.Module.new()
	m.target = ssa.TargetData{ ptr_size: 4 }
	i32_type := m.type_store.get_int(32)
	bool_type := m.type_store.get_int(1)
	pair_type := m.type_store.register(ssa.Type{
		kind:        .struct_t
		fields:      [i32_type, i32_type]
		field_names: ['first', 'second']
	})
	ptr_pair := m.type_store.get_ptr(pair_type)
	ptr_i32 := m.type_store.get_ptr(i32_type)
	func := m.new_function('aggregate_swap', i32_type)
	n := m.add_value(.argument, i32_type, 'n', 0)
	m.func_add_param(func, n)
	entry := m.add_block(func, 'entry')
	header := m.add_block(func, 'header')
	body := m.add_block(func, 'body')
	done := m.add_block(func, 'done')
	zero := m.get_or_add_const(i32_type, '0')
	one := m.get_or_add_const(i32_type, '1')
	four := m.get_or_add_const(i32_type, '4')
	hundred := m.get_or_add_const(i32_type, '100')
	mut pairs := []ssa.ValueID{}
	for scalar in [1, 2] {
		slot := m.add_instr(.alloca, entry, ptr_pair, [])
		first := m.add_instr(.get_element_ptr, entry, ptr_i32, [slot, zero])
		second := m.add_instr(.get_element_ptr, entry, ptr_i32, [slot, four])
		m.add_instr(.store, entry, 0, [m.get_or_add_const(i32_type, scalar.str()), first])
		m.add_instr(.store, entry, 0, [
			m.get_or_add_const(i32_type, (scalar * 10).str()),
			second,
		])
		pairs << m.add_instr(.load, entry, pair_type, [slot])
	}
	m.add_instr(.jmp, entry, 0, [ssa.ValueID(header)])
	a := m.add_instr(.phi, header, pair_type, [])
	b := m.add_instr(.phi, header, pair_type, [])
	i := m.add_instr(.phi, header, i32_type, [])
	condition := m.add_instr(.lt, header, bool_type, [i, n])
	m.add_instr(.br, header, 0, [condition, ssa.ValueID(body), ssa.ValueID(done)])
	next_i := m.add_instr(.add, body, i32_type, [i, one])
	m.add_instr(.jmp, body, 0, [ssa.ValueID(header)])
	m.append_phi_operands(a, pairs[0], entry)
	m.append_phi_operands(a, b, body)
	m.append_phi_operands(b, pairs[1], entry)
	m.append_phi_operands(b, a, body)
	m.append_phi_operands(i, zero, entry)
	m.append_phi_operands(i, next_i, body)
	slot := m.add_instr(.alloca, done, ptr_pair, [])
	m.add_instr(.store, done, 0, [a, slot])
	first_ptr := m.add_instr(.get_element_ptr, done, ptr_i32, [slot, zero])
	second_ptr := m.add_instr(.get_element_ptr, done, ptr_i32, [slot, four])
	first := m.add_instr(.load, done, i32_type, [first_ptr])
	second := m.add_instr(.load, done, i32_type, [second_ptr])
	left := m.add_instr(.mul, done, i32_type, [first, hundred])
	result := m.add_instr(.add, done, i32_type, [left, second])
	m.add_instr(.ret, done, 0, [result])
	return m
}

fn test_ssa_wasm_aggregate_phi_parallel_copies() {
	dir := ssa_wasm_test_dir('aggregate_phi')
	defer { os.rmdir_all(dir) or {} }
	for production in [false, true] {
		mut m := ssa_wasm_aggregate_swap_loop()
		if production {
			optimize.optimize(mut m)
		}
		mut g := SSAGen.new(m)
		g.gen() or { panic(err) }
		path := os.join_path(dir, 'aggregate_phi_${production}.wasm')
		g.write(path) or { panic(err) }
		assert_ssa_wasm_execution(path, '
assert.equal(e.aggregate_swap(0), 110);
assert.equal(e.aggregate_swap(1), 220);
assert.equal(e.aggregate_swap(2), 110);
assert.equal(e.aggregate_swap(3), 220);
assert.equal(e.aggregate_swap(100), 110);
')
	}
}

fn test_ssa_wasm_function_references_and_indirect_calls() {
	dir := ssa_wasm_test_dir('indirect')
	defer { os.rmdir_all(dir) or {} }
	for production in [false, true] {
		mut m := ssa.Module.new()
		m.target = ssa.TargetData{ ptr_size: 4 }
		i32_type := m.type_store.get_int(32)
		i64_type := m.type_store.get_int(64)
		bool_type := m.type_store.get_int(1)
		add := m.new_function('add', i32_type)
		a := m.add_value(.argument, i32_type, 'a', 0)
		b := m.add_value(.argument, i32_type, 'b', 1)
		m.func_add_param(add, a)
		m.func_add_param(add, b)
		add_entry := m.add_block(add, 'entry')
		result := m.add_instr(.add, add_entry, i32_type, [a, b])
		m.add_instr(.ret, add_entry, 0, [result])
		// The shared builder represents stored function references as i64 values.
		ref := m.add_value(.func_ref, i64_type, 'add', add)
		caller := m.new_function('indirect_add', i32_type)
		x := m.add_value(.argument, i32_type, 'x', 0)
		m.func_add_param(caller, x)
		entry := m.add_block(caller, 'entry')
		ptr_ref := m.type_store.get_ptr(i64_type)
		slot := m.add_instr(.alloca, entry, ptr_ref, [])
		m.add_instr(.store, entry, 0, [ref, slot])
		loaded := m.add_instr(.load, entry, i64_type, [slot])
		three := m.get_or_add_const(i32_type, '3')
		called := m.add_instr(.call_indirect, entry, i32_type, [loaded, x, three])
		m.add_instr(.ret, entry, 0, [called])
		check_ref := m.new_function('reference_is_nonzero', bool_type)
		check_entry := m.add_block(check_ref, 'entry')
		zero := m.get_or_add_const(i64_type, '0')
		nonzero := m.add_instr(.ne, check_entry, bool_type, [ref, zero])
		m.add_instr(.ret, check_entry, 0, [nonzero])
		null_call := m.new_function('null_indirect_call', i32_type)
		null_entry := m.add_block(null_call, 'entry')
		null_result := m.add_instr(.call_indirect, null_entry, i32_type, [zero, three, three])
		m.add_instr(.ret, null_entry, 0, [null_result])
		if production {
			optimize.optimize(mut m)
		}
		mut g := SSAGen.new(m)
		g.configure({
			'indirect_add':         'indirect_add'
			'reference_is_nonzero': 'reference_is_nonzero'
			'null_indirect_call':   'null_indirect_call'
		}, []string{}, '')
		g.gen() or { panic(err) }
		path := os.join_path(dir, 'indirect_${production}.wasm')
		g.write(path) or { panic(err) }
		assert_ssa_wasm_execution(path, '
assert.equal(e.indirect_add(7), 10);
assert.equal(e.indirect_add(-4), -1);
assert.equal(e.reference_is_nonzero(), 1);
assert.throws(() => e.null_indirect_call(), WebAssembly.RuntimeError);
')
	}
}

fn test_ssa_wasm_internal_shadowing_scope_lowering() {
	dir := ssa_wasm_test_dir('shadowing')
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'shadowing.v')
	os.write_file(source, '
fn scope() int {
	i := 10
	for i := 0; i < 1; i++ {}
	x := 1
	mut total := 0
	for j := 0; j < 2; j++ {
		x := j + 2
		total = total * 10 + x
	}
	return i * 1000 + total * 10 + x
}

fn post_scope() int {
	mut i := 0
	mut count := 0
	for ; i < 3; i++ {
		i := 10
		_ = i
		count++
		if count > 100 {
			break
		}
	}
	return i * 1000 + count
}

fn initializer_scope() int {
	x := 5
	mut total := 0
	{
		x := x + 1
		total = x
	}
	return total * 10 + x
}
') or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(source)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	// Source checking rejects shadowing; retain these internal lowering regressions
	// by passing the parsed, annotated tree directly to the shared SSA builder.
	tc.annotate_types()
	for production in [false, true] {
		mut m := ssa.build_with_options(a, map[string]bool{}, &tc, ssa.BuildOptions{
			target: ssa.TargetData{ ptr_size: 4 }
		})
		if production {
			optimize.optimize(mut m)
		}
		mut g := SSAGen.new(m)
		g.configure({
			'scope':             'scope'
			'post_scope':        'post_scope'
			'initializer_scope': 'initializer_scope'
		}, []string{}, '')
		g.gen() or { panic(err) }
		path := os.join_path(dir, 'shadowing_${production}.wasm')
		g.write(path) or { panic(err) }
		assert_ssa_wasm_execution(path, '
assert.equal(e.scope(), 10231);
assert.equal(e.post_scope(), 3003);
assert.equal(e.initializer_scope(), 65);
')
	}
}

fn test_ssa_wasm_module_scoped_globals_and_void_calls() {
	dir := ssa_wasm_test_dir('module_globals')
	defer { os.rmdir_all(dir) or {} }
	os.mkdir_all(os.join_path(dir, 'foo')) or { panic(err) }
	main_source := os.join_path(dir, 'main.v')
	foo_source := os.join_path(dir, 'foo', 'foo.v')
	os.write_file(main_source, '
module main
import foo
__global counter int
pub fn counters() int {
	counter = 100
	foo.bump()
	foo.bump()
	foo.bump()
	return counter * 10 + foo.get()
}
') or { panic(err) }
	os.write_file(foo_source, '
module foo
__global counter int
pub fn bump() {
	counter++
}
pub fn get() int {
	return counter
}
') or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.enable_globals = true
	mut p := parser.Parser.new(prefs)
	a := p.parse_files([foo_source, main_source])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.enable_globals = true
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	mut metadata := Gen.new(a, &tc, map[string]bool{})
	config := metadata.ssa_configuration()
	assert metadata.import_paths == ['foo'], metadata.import_paths.str()
	assert config.used_fns['foo.bump'], config.used_fns.str()
	assert config.used_fns['foo.get'], config.used_fns.str()
	assert config.source_modules[foo_source] == 'foo', config.source_modules.str()
	assert config.source_imports[main_source]['foo'] == 'foo', config.source_imports.str()
	for production in [false, true] {
		mut m := ssa.build_with_options(a, config.used_fns, &tc, ssa.BuildOptions{
			target:         ssa.TargetData{ ptr_size: 4 }
			exact_used_fns: true
			source_modules: config.source_modules
			source_imports: config.source_imports
		})
		if production {
			optimize.optimize(mut m)
		}
		mut g := SSAGen.new(m)
		g.configure(config.exports, config.init_fns, config.main_fn)
		g.gen() or { panic(err) }
		path := os.join_path(dir, 'globals_${production}.wasm')
		g.write(path) or { panic(err) }
		assert_ssa_wasm_execution(path, 'assert.equal(e.counters(), 1003);')
	}
}

fn test_ssa_wasm_rejects_intrinsic_function_references() {
	mut m := ssa.Module.new()
	m.target = ssa.TargetData{ ptr_size: 4 }
	i64_type := m.type_store.get_int(64)
	ptr_type := m.type_store.get_ptr(m.type_store.get_int(8))
	external := m.new_function('C.malloc', ptr_type)
	m.funcs[external].is_c_extern = true
	func := m.new_function('intrinsic_reference', i64_type)
	entry := m.add_block(func, 'entry')
	ref := m.add_value(.func_ref, i64_type, 'C.malloc', external)
	m.add_instr(.ret, entry, 0, [ref])
	mut g := SSAGen.new(m)
	g.gen() or {
		assert err.msg().contains('unknown function reference'), err.msg()
		assert err.msg().contains('C.malloc'), err.msg()
		return
	}
	assert false, 'external intrinsic references require an explicit WebAssembly implementation'
}

fn test_ssa_wasm_pointer_integer_casts_preserve_wasm32_bits() {
	dir := ssa_wasm_test_dir('pointer_casts')
	defer { os.rmdir_all(dir) or {} }
	for production in [false, true] {
		mut m := ssa.Module.new()
		m.target = ssa.TargetData{ ptr_size: 4 }
		u64_type := m.type_store.get_uint(64)
		ptr_type := m.type_store.get_ptr(m.type_store.get_int(8))
		func := m.new_function('pointer_number', u64_type)
		ptr := m.add_value(.argument, ptr_type, 'ptr', 0)
		m.func_add_param(func, ptr)
		entry := m.add_block(func, 'entry')
		number := m.add_instr(.bitcast, entry, u64_type, [ptr])
		m.add_instr(.ret, entry, 0, [number])
		round_trip := m.new_function('pointer_round_trip', u64_type)
		input := m.add_value(.argument, u64_type, 'input', 0)
		m.func_add_param(round_trip, input)
		round_entry := m.add_block(round_trip, 'entry')
		narrowed := m.add_instr(.bitcast, round_entry, ptr_type, [input])
		widened := m.add_instr(.bitcast, round_entry, u64_type, [narrowed])
		m.add_instr(.ret, round_entry, 0, [widened])
		if production {
			optimize.optimize(mut m)
		}
		mut g := SSAGen.new(m)
		g.gen() or { panic(err) }
		path := os.join_path(dir, 'pointer_${production}.wasm')
		g.write(path) or { panic(err) }
		assert_ssa_wasm_execution(path, '
assert.equal(e.pointer_number(0x80000000), 2147483648n);
assert.equal(e.pointer_number(0xffffffff), 4294967295n);
assert.equal(e.pointer_round_trip(-1n), 4294967295n);
assert.equal(e.pointer_round_trip(4294967296n), 0n);
')
	}
}

fn test_ssa_wasm_imported_function_value_keeps_its_definition() {
	dir := ssa_wasm_test_dir('imported_fn_ref')
	defer { os.rmdir_all(dir) or {} }
	os.mkdir_all(os.join_path(dir, 'foo')) or { panic(err) }
	main_source := os.join_path(dir, 'main.v')
	foo_source := os.join_path(dir, 'foo', 'foo.v')
	os.write_file(main_source, '
module main
import foo as helper
pub fn invoke(value int) int {
	callback := helper.add
	return callback(value)
}
') or { panic(err) }
	os.write_file(foo_source, '
module foo
pub fn add(value int) int {
	return value + 7
}
') or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_files([foo_source, main_source])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	_ = tc.check_semantics_opt(false)
	assert tc.errors.len == 0, tc.errors.str()
	tc.annotate_types()
	mut metadata := Gen.new(a, &tc, map[string]bool{})
	config := metadata.ssa_configuration()
	assert config.used_fns['foo.add'], config.used_fns.str()
	for production in [false, true] {
		mut m := ssa.build_with_options(a, config.used_fns, &tc, ssa.BuildOptions{
			target:         ssa.TargetData{ ptr_size: 4 }
			exact_used_fns: true
			source_modules: config.source_modules
			source_imports: config.source_imports
		})
		if production {
			optimize.optimize(mut m)
		}
		mut g := SSAGen.new(m)
		g.configure(config.exports, config.init_fns, config.main_fn)
		g.gen() or { panic(err) }
		path := os.join_path(dir, 'imported_fn_ref_${production}.wasm')
		g.write(path) or { panic(err) }
		assert_ssa_wasm_execution(path, '
assert.equal(e.invoke(3), 10);
assert.equal(e.invoke(-7), 0);
')
	}
}
