module ssa

import os
import v.cmdexec
import v.parser
import v.pref
import v.transform
import v.types

fn test_wide_integer_lowering_keeps_scalar_instructions_on_limbs() {
	source := 'module main
fn probe(signed i64, unsigned u64) {
    value := i128(signed)
    other := u128(unsigned)
    arithmetic := value >> 65
    logical := value >>> 65
    combined := (value + i128(1)) ^ i128(other)
    pointer := &value
    same_pointer := pointer == pointer
    copy := unsafe { *pointer }
}
fn main() {}
'
	path := os.join_path(os.vtmp_dir(), 'ssa_wide_limbs_${os.getpid()}.v')
	defer { os.rm(path) or {} }
	os.write_file(path, source) or { panic(err) }
	mut p := parser.Parser.new(pref.new_preferences())
	mut a := p.parse_file(path)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.annotate_types()
	assert tc.errors.len == 0, tc.errors.str()
	transform.transform(mut a, tc)
	m := build_with_used(a, map[string]bool{}, tc)
	mut functions := 0
	mut arithmetic_shifts := 0
	mut logical_shifts := 0
	mut pointer_comparisons := 0
	for function in m.funcs {
		if function.name != 'probe' { continue }
		functions++
		for block in function.blocks {
			for id in m.blocks[block].instrs {
				instruction := m.instrs[m.values[id].index]
				if instruction.op in [.add, .sub, .mul, .sdiv, .udiv, .srem, .urem, .shl, .ashr,
					.lshr, .and_, .or_, .xor, .eq, .ne, .lt, .le, .gt, .ge, .ult, .ule, .ugt, .uge] {
					for operand in instruction.operands {
						typ := m.type_store.types[m.values[operand].typ]
						assert typ.kind != .struct_t, '${instruction.op} consumed an aggregate'
					}
				}
				if instruction.op == .ashr { arithmetic_shifts++ }
				if instruction.op == .lshr { logical_shifts++ }
				if instruction.op == .eq
					&& m.type_store.types[m.values[instruction.operands[0]].typ].kind == .ptr_t {
					pointer_comparisons++
				}
			}
		}
	}
	assert functions == 1
	assert arithmetic_shifts >= 3
	assert logical_shifts >= 1
	assert pointer_comparisons == 1
}

fn test_unsupported_wide_integer_operations_fail_before_scalar_lowering() {
	// Build one small source-pipeline driver, then isolate each expected panic in
	// its own process. Every fixture must pass parsing and type checking first.
	dir := os.join_path(os.vtmp_dir(), 'ssa_wide_diagnostics_${os.getpid()}')
	os.mkdir_all(dir) or { panic(err) }
	defer { os.rmdir_all(dir) or {} }
	driver_source := os.join_path(dir, 'driver.v')
	driver := os.join_path(dir, 'driver')
	os.write_file(driver_source, r'module main
import os
import v.parser
import v.pref
import v.ssa
import v.transform
import v.types
fn main() {
    mut p := parser.Parser.new(pref.new_preferences())
    mut a := p.parse_file(os.args[1])
    assert p.diagnostics.len == 0, p.diagnostics.str()
    mut tc := types.TypeChecker.new(a)
    tc.collect(a)
    tc.annotate_types()
    assert tc.errors.len == 0, tc.errors.str()
    transform.transform(mut a, tc)
    m := ssa.build_with_used(a, map[string]bool{}, tc)
    assert m.funcs.any(it.name == "probe")
}
') or { panic(err) }
	built := cmdexec.run(@VEXE, ['-gc', 'none', '-o', driver, driver_source])
	assert built.exit_code == 0, built.output
	mut cases := [
		'fn probe() bool { high := u128(1) << 80\nreturn high == u128(0) }',
		'fn probe(x i128) i128 { return -x }',
		'fn probe(x u128) u128 { return ~x }',
		'fn probe(x u128, y u128) u128 { return x * y }',
		'fn probe(x u128, y u128) u128 { return x / y }',
		'fn probe(x u128, y u128) u128 { return x % y }',
		'fn probe(x u128, shift int) u128 { return x << shift }',
		'fn probe(x i128, shift int) i128 { return x >> shift }',
		'fn probe(x i128, shift int) i128 { return x >>> shift }',
		'fn probe() { mut x := u128(1)\nx += u128(1) }',
		'struct Box { mut: value u128 }\nfn probe() { mut x := Box{value: u128(1)}\nx.value += u128(1) }',
		'fn probe() { mut x := [u128(1)]\nx[0] += u128(1) }',
		'fn probe(mut x u128) { x += u128(1) }',
		'fn probe() { mut x := u128(1)\np := &x\nunsafe { *p += u128(1) } }',
		'fn probe() { mut x := u128(1)\nx++ }',
		'fn probe() { mut x := u128(1)\nx-- }',
		'fn probe(x f64) i128 { return i128(x) }',
		'fn probe(x i128) f64 { return f64(x) }',
		'fn probe(x u128) u64 { return u64(x) }',
		'fn probe(x u128) int { return match x { u128(1) { 1 } else { 0 } } }',
		'fn probe(x u128) { match x { u128(1) {} else {} } }',
		'fn probe(x u128) voidptr { return voidptr(x) }',
		'fn probe(x voidptr) u128 { return u128(x) }',
		'fn probe(x u128) bool { return x in u128(1)..u128(3) }',
		'fn probe() { for n in u128(1)..u128(3) {} }',
		'fn probe() u128 { return u128(0x10000000000000000) }',
		'fn probe() u128 { return u128(18446744073709551616) }',
		'struct Box { value u128 }\nfn probe(x u128) Box { return Box{value: ~x} }',
		'fn probe(i u128) int { a := [1, 2, 3]!\nreturn a[i] }',
		'fn probe(i u128) int { a := [1, 2, 3]\nreturn a[i] }',
		'fn probe(i u128) u8 { return "abc"[i] }',
		'fn probe(i u128) []int { a := [1, 2, 3]!\nreturn a[i..] }',
		'fn probe(i u128) []int { a := [1, 2, 3]\nreturn a[..i] }',
		'fn probe(i u128) string { return "abc"[i..] }',
		'fn probe(i u128) { mut a := [1, 2, 3]!\na[i] = 4 }',
		'fn probe(i u128) int { a := [1, 2, 3]!\nreturn unsafe { &a[0] }[i] }',
		'fn probe(x u128) int { a := [u128(1), u128(2)]\nreturn a.index(x) }',
	]
	for operator in ['!=', '<', '<=', '>', '>='] {
		cases << 'fn probe(x u128, y u128) bool { return x ${operator} y }'
	}
	for index, source in cases {
		path := os.join_path(dir, 'case_${index}.v')
		os.write_file(path, 'module main\n${source}\nfn main() {}\n') or { panic(err) }
		result := os.exec([driver, path])
		assert result.exit_code != 0, source
		assert result.output.contains('ssa: unsupported 128-bit integer'), '${source}\n${result.output}'
	}
}
