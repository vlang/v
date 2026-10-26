import os

// A backed enum (`enum E as u8`) is stored in its backing type, also as the element of an
// array, fixed array or map. The assert failure message stringifies such values with its
// own helpers, which must read the elements with that type, not as a 4 byte `int`.
enum Small as u8 {
	a
	b
	c
}

enum Wide as u64 {
	lo = 1
	hi = 0x1_0000_0000
}

fn test_backed_enum_collections_str() {
	small := [Small.a, .b, .c]!
	assert '${small}' == '[a, b, c]'
	assert small.str() == '[a, b, c]'
	wide := [Wide.hi, .lo]!
	assert '${wide}' == '[hi, lo]'
	nested := [[Small.b, .c]!, [Small.a, .b]!]!
	assert '${nested}' == '[[b, c], [a, b]]'
	rows := [[Small.c, .b]!]
	assert '${rows}' == '[[c, b]]'
	by_key := {
		Small.c: [Small.b, .a]!
	}
	assert '${by_key}' == '{c: [b, a]}'
}

const enum_decls = 'enum Small as u8 {
	a
	b
	c
}

enum Wide as u64 {
	lo = 1
	hi = 0x1_0000_0000
}
'

// The elements of these were read as `int`s: the messages showed other enum values.
const flat_checks = '
@[assert_continues]
fn check() {
	small := [Small.a, .b, .c]!
	assert small == [Small.c, .b, .a]!
	wide := [Wide.hi, .lo]!
	assert wide == [Wide.lo, .hi]!
	shared sh := [Small.c, .b]!
	rlock sh {
		assert sh == [Small.a, .a]!
	}
}
'

// These named an undeclared `Array_fixed_int_2` element type: the C did not compile.
const nested_checks = '
@[assert_continues]
fn check() {
	nested := [[Small.b, .c]!, [Small.a, .b]!]!
	assert nested == [[Small.a, .a]!, [Small.c, .c]!]!
	rows := [[Small.c, .b]!]
	assert rows == [[Small.a, .a]!]
	by_key := {
		Small.c: [Small.b, .a]!
	}
	assert by_key == {
		Small.a: [Small.a, .a]!
	}
}
'

fn run_assert_program(name string, checks string) string {
	path := os.join_path(os.vtmp_dir(), 'v3_backed_enum_assert_${name}_${os.getpid()}.v')
	os.write_file(path, enum_decls + checks + '\nfn main() {\n\tcheck()\n}\n') or { panic(err) }
	defer {
		os.rm(path) or {}
	}
	result := os.exec([@VEXE, 'run', path])
	assert result.exit_code == 0, result.output
	return result.output
}

fn test_backed_enum_fixed_arrays_in_assert_messages() {
	output := run_assert_program('flat', flat_checks)
	for line in [
		'left value: small = [a, b, c]',
		'right value: [Small.c, .b, .a]! = [c, b, a]',
		'left value: wide = [hi, lo]',
		'right value: [Wide.lo, .hi]! = [lo, hi]',
		'left value: sh = [c, b]',
		'right value: [Small.a, .a]! = [a, a]',
	] {
		assert output.contains(line), output
	}
}

fn test_nested_backed_enum_fixed_arrays_in_assert_messages() {
	output := run_assert_program('nested', nested_checks)
	for line in [
		'left value: nested = [[b, c], [a, b]]',
		'right value: [[Small.a, .a]!, [Small.c, .c]!]! = [[a, a], [c, c]]',
		'left value: rows = [[c, b]]',
		'right value: [[Small.a, .a]!] = [[a, a]]',
		'left value: by_key = {c: [b, a]}',
	] {
		assert output.contains(line), output
	}
}
