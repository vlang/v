module main

import os

fn testsuite_begin() {
	// The MSVC that a Windows job selects through VFLAGS cannot target Linux, and
	// this file generates Linux C; let those builds use the default compiler.
	if os.getenv('VFLAGS').contains('msvc') {
		os.unsetenv('VFLAGS')
	}
}

const nested_map_append_staged_rhs_source = 'fn append_rows() int {
	mut rows := map[string]map[string][][]int{}
	rows["a"]["b"] << [5, 6]
	mut words := map[string]map[string][][]string{}
	words["a"]["b"] << ["x", "y"]
	mut deep := map[string]map[string]map[string][][]int{}
	deep["a"]["b"]["c"] << [7, 8]
	return rows["a"]["b"].len + words["a"]["b"].len + deep["a"]["b"]["c"].len
}

fn append_spread() int {
	mut values := map[string]map[string][]int{}
	values["a"]["b"] << [1, 2]
	return values["a"]["b"].len
}

fn main() {
	assert append_rows() == 3
	assert append_spread() == 2
}
'

fn generated_fn_body(code string, name string) string {
	return code.all_after(' ${name}(void) {\n').all_before('\n}')
}

fn staged_append_rhs_names(body string) []string {
	mut names := []string{}
	prefix := '__map_append_rhs_'
	mut rest := body
	for rest.contains(prefix) {
		rest = rest.all_after(prefix)
		mut digits := 0
		for digits < rest.len && rest[digits].is_digit() {
			digits++
		}
		name := prefix + rest[..digits]
		if digits > 0 && name !in names {
			names << name
		}
	}
	return names
}

// A nested map append stages its RHS in a temporary before it reads the inner map.
// When the destination elements are arrays, `<< [5, 6]` pushes that temporary as one
// element, which shares its buffer. Releasing the temporary after the push left the
// element dangling (AddressSanitizer: heap-use-after-free with `-gc none`). Only an
// element push that cloned the temporary, or a bulk append that copied its elements,
// may release it.
fn test_nested_map_append_keeps_staged_rhs_pushed_as_one_element() {
	root := os.join_path(os.vtmp_dir(), 'nested_map_append_staged_rhs_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'input.v')
	c_path := os.join_path(root, 'output.c')
	os.write_file(path, nested_map_append_staged_rhs_source)!
	// V1-hosted unit tests get a shared V3 compiler from their runner.
	shared_v3 := os.getenv('V3_TEST_UNIT_SHARED_V3')
	mut args := if shared_v3 != '' { [shared_v3] } else { [@VEXE] }
	args << ['-gc', 'none', '-os', 'linux', '-arch', 'amd64', '-o', c_path, path]
	compiled := os.exec(args)
	assert compiled.exit_code == 0, compiled.output
	code := os.read_file(c_path)!
	rows := generated_fn_body(code, 'append_rows').replace_each(['\t', '', ' ', '', '\n', ''])
	names := staged_append_rhs_names(rows)
	assert names.len == 3, rows
	assert rows.count('array_push(') >= 3, rows
	for name in names {
		// `-autofree` clones the pushed element, which makes releasing the temporary valid.
		pushed_as_is := rows.contains('=${name};')
		released := rows.contains('array__free(&(${name}))')
			|| rows.contains('array__free(&${name})')
		assert !(pushed_as_is && released), '${name}: ${rows}'
	}
	spread := generated_fn_body(code, 'append_spread').replace_each(['\t', '', ' ', '', '\n', ''])
	spread_names := staged_append_rhs_names(spread)
	assert spread_names.len == 1, spread
	assert spread.contains('array__push_many('), spread
	assert spread.contains('array__free(&${spread_names[0]})'), spread
}
