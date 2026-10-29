module x64

import v.ssa
import os
import v.parser
import v.pref
import v.types

fn c_extern_abi_fixture(aggregate_return bool, aggregate_argument bool, pointer_argument bool) &Gen {
	mut m := ssa.Module.new()
	i64_type := m.type_store.get_int(64)
	aggregate := m.type_store.register(ssa.Type{
		kind:   .struct_t
		fields: [i64_type, i64_type]
	})
	return_type := if aggregate_return { aggregate } else { i64_type }
	param_type := if pointer_argument {
		m.type_store.get_ptr(aggregate)
	} else if aggregate_argument {
		aggregate
	} else {
		i64_type
	}
	extern_id := m.new_function('c_fixture', return_type)
	m.func_set_c_extern(extern_id, true)
	param := m.add_value(.argument, param_type, 'arg', 0)
	m.func_add_param(extern_id, param)
	caller_id := m.new_function('caller', i64_type)
	block := m.add_block(caller_id, 'entry')
	callee := m.add_value(.func_ref, return_type, 'c_fixture', extern_id)
	m.add_instr(.call, block, return_type, [callee, param])
	return Gen.new(m)
}

fn test_c_aggregate_returns_and_arguments_are_rejected_before_codegen() {
	for aggregate_return in [false, true] {
		g := c_extern_abi_fixture(aggregate_return, !aggregate_return, false)
		g.validate_c_extern_calls() or {
			assert err.msg().contains(if aggregate_return { 'return values' } else { 'arguments' })
			assert err.msg().contains('c_fixture')
			continue
		}
		assert false, 'by-value C aggregate must produce a diagnostic'
	}
}

fn test_c_scalars_and_pointers_to_aggregates_remain_supported() {
	c_extern_abi_fixture(false, false, false).validate_c_extern_calls()!
	c_extern_abi_fixture(false, false, true).validate_c_extern_calls()!
}

fn c_reference_fixture(body string) &Gen {
	path := os.join_path(os.vtmp_dir(), 'x64_c_reference_${os.getpid()}.v')
	os.write_file(path, 'module main
struct C.Pair {
    first i64
    second i64
}
fn C.return_pair(i64) C.Pair
fn C.accept_pair(C.Pair) i64
fn C.scalar(i64) i64
fn C.pointer(&C.Pair) i64
fn C.labs(i64) i64
fn C.strlen(&u8) u64
fn C.pow(f64, f64) f64
fn C.strcmp(&u8, &u8) int
fn C.printf(&u8, ...voidptr) int
fn C.exit(int)
fn C.apply_integer(fn (i64) i64, i64) i64
fn C.apply_pointer(fn (voidptr) voidptr, voidptr) voidptr
fn C.apply_float(fn (f64) f64, f64) f64
fn C.apply_narrow(fn (int) int, int) int
fn C.apply_aggregate(fn (C.Pair) C.Pair) i64
fn C.apply_variadic(fn (...i64) i64) i64
fn C.apply_nested(fn (fn (f64) f64) i64) i64
fn C.apply_nested_variadic(fn (fn (...i64) i64) i64) i64
fn C.make_callback() fn (f64) f64
fn C.make_integer_callback() fn (i64) i64
fn integer_callback(x i64) i64 { return x }
fn pointer_callback(x voidptr) voidptr { return x }
fn float_callback(x f64) f64 { return x }
fn narrow_callback(x int) int { return x }
fn aggregate_callback(x C.Pair) C.Pair { return x }
fn variadic_callback(x ...i64) i64 { return x.len }
fn nested_callback(x fn (f64) f64) i64 { return 0 }
fn nested_variadic_callback(x fn (...i64) i64) i64 { return 0 }
struct Callbacks {
    scalar fn (i64) i64
    pointer fn (&u8) u64
}
fn main() {
    ${body}
}
') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	mut p := parser.Parser.new(pref.new_preferences())
	a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	return Gen.new(ssa.build_with_used(a, map[string]bool{}, tc))
}

fn test_unused_c_aggregate_prototypes_are_allowed() {
	c_reference_fixture('').validate_c_extern_calls()!
}

fn assert_typed_c_reference(g &Gen, name string) {
	for value in g.m.values {
		if value.kind == .func_ref && value.name == 'C.${name}' {
			assert g.m.funcs[value.index].is_c_extern
			assert g.m.type_store.types[value.typ].kind == .func_t
			assert g.m.type_store.types[value.typ].params.len > 0
			assert g.m.type_store.types[value.typ].ret_type == g.m.funcs[value.index].typ
			return
		}
	}
	assert false, 'source C function address must retain its signature: ${name}'
}

fn test_scalar_and_pointer_c_function_addresses_are_allowed() {
	for name in ['scalar', 'pointer'] {
		g := c_reference_fixture('callback := C.${name}\n_ = callback')
		assert_typed_c_reference(g, name)
		g.validate_c_extern_calls()!
	}
}

fn test_c_aggregate_function_addresses_are_rejected_before_indirect_calls() {
	for name in ['return_pair', 'accept_pair'] {
		g := c_reference_fixture('callback := C.${name}\n_ = callback')
		assert_typed_c_reference(g, name)
		g.validate_c_extern_calls() or {
			assert err.msg().contains('C aggregate')
			assert err.msg().contains(name)
			continue
		}
		assert false, 'taking an unsupported C function address must produce a diagnostic'
	}
}

fn test_unsupported_scalar_c_function_addresses_are_rejected() {
	for name in ['pow', 'strcmp', 'printf'] {
		g := c_reference_fixture('callback := C.${name}\n_ = callback')
		assert_typed_c_reference(g, name)
		g.validate_c_extern_calls() or {
			assert err.msg().contains('C function addresses')
			assert err.msg().contains(name)
			continue
		}
		assert false, 'unsupported indirect C convention must produce a diagnostic'
	}
}

fn test_direct_c_scalar_calls_keep_their_calling_convention() {
	c_reference_fixture("_ = C.pow(2.0, 3.0)\n_ = C.strcmp(c'a', c'b')\n_ = C.printf(c'%d', 1)").validate_c_extern_calls()!
}

fn test_c_callback_parameters_preserve_their_declared_signatures() {
	for body in ['_ = C.apply_integer(integer_callback, 7)',
		'_ = C.apply_pointer(pointer_callback, voidptr(0))'] {
		g := c_reference_fixture(body)
		mut found := false
		for instr in g.m.instrs {
			if instr.op == .call && instr.operands.len > 1 {
				callee := g.m.values[instr.operands[0]]
				if callee.name.trim_string_left('C.') in ['apply_integer', 'apply_pointer'] {
					assert g.m.type_store.types[g.call_param_type(instr, 0)].kind == .func_t
					found = true
				}
			}
		}
		assert found, 'C callback test must exercise the actual source call'
		g.validate_c_extern_calls()!
	}
}

fn test_unsupported_c_callbacks_are_rejected_at_calls_and_addresses() {
	for name, callback in {
		'apply_float':           'float_callback, 1.0'
		'apply_narrow':          'narrow_callback, 1'
		'apply_aggregate':       'aggregate_callback'
		'apply_variadic':        'variadic_callback'
		'apply_nested':          'nested_callback'
		'apply_nested_variadic': 'nested_variadic_callback'
	} {
		for body in ['_ = C.${name}(${callback})', 'callback := C.${name}\n_ = callback'] {
			g := c_reference_fixture(body)
			g.validate_c_extern_calls() or {
				assert err.msg().contains('C callback')
					|| (body.starts_with('callback :=') && err.msg().contains('C function addresses')), '${name}: ${body}: ${err.msg()}'
				assert err.msg().contains(name)
				continue
			}
			assert false, 'unsupported C callback convention must produce a diagnostic: ${name}'
		}
	}
}

fn test_unsupported_c_callback_results_are_rejected() {
	for body in ['_ = C.make_callback()', 'callback := C.make_callback\n_ = callback'] {
		g := c_reference_fixture(body)
		mut found := false
		for function in g.m.funcs {
			if function.name == 'C.make_callback' && function.is_c_extern {
				assert g.m.type_store.types[function.typ].kind == .func_t
				found = true
			}
		}
		assert found
		g.validate_c_extern_calls() or {
			assert err.msg().contains('C callback')
			assert err.msg().contains('make_callback')
			continue
		}
		assert false, 'unsupported returned C callback must produce a diagnostic'
	}
}

fn test_supported_c_function_addresses_can_be_stored_and_called() {
	$if linux && amd64 {
		for optimize in [false, true] {
			mut g := c_reference_fixture("callbacks := Callbacks{scalar: C.labs, pointer: C.strlen}
	if callbacks.scalar(-13) != 13 || callbacks.pointer(c'abc') != 3 { C.exit(1) }")
			g.optimize_runtime = optimize
			g.validate_c_extern_calls()!
			g.gen()
			output := os.join_path(os.vtmp_dir(), 'x64_c_callbacks_${os.getpid()}_${optimize}')
			defer { os.rm(output) or {} }
			g.write_and_link(output)
			result := os.exec([output])
			assert result.exit_code == 0, result.output
		}
	}
}

fn test_supported_callbacks_cross_the_c_boundary_in_both_directions() {
	$if linux && amd64 {
		for optimize in [false, true] {
			mut g := c_reference_fixture('if C.apply_integer(integer_callback, 13) != 13 { C.exit(1) }
	p := voidptr(u64(23))
	if C.apply_pointer(pointer_callback, p) != p { C.exit(2) }
	callback := C.make_integer_callback()
	if callback(-13) != 13 { C.exit(3) }')
			g.optimize_runtime = optimize
			g.validate_c_extern_calls()!
			g.gen()
			output := os.join_path(os.vtmp_dir(), 'x64_c_callback_bridge_${os.getpid()}_${optimize}')
			object := output + '.o'
			c_source := output + '.c'
			defer {
				for path in [output, object, c_source] {
					os.rm(path) or {}
				}
			}
			g.write_file(object)
			os.write_file(c_source, 'long apply_integer(long (*callback)(long), long x) { return callback(x); }
	void *apply_pointer(void *(*callback)(void *), void *x) { return callback(x); }
	static long callback_abs(long x) { return x < 0 ? -x : x; }
	long (*make_integer_callback(void))(long) { return callback_abs; }
	')!
			link := os.exec(['cc', '-no-pie', object, c_source, '-o', output, '-lm', '-ldl', '-lpthread'])
			assert link.exit_code == 0, link.output
			result := os.exec([output])
			assert result.exit_code == 0, result.output
		}
	}
}
