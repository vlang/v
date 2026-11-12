module c

import os

const range_len_index_source = "@[noinline]
fn param_sum(a []int) int {
	mut s := 0
	for i in 0 .. a.len {
		s += a[i]
	}
	return s
}

@[noinline]
fn local_tail_sum() int {
	a := [1, 2, 3]
	mut s := 0
	for i in 1 .. a.len {
		for j in 0 .. a.len {
			s += a[i] * a[j]
		}
	}
	return s
}

@[noinline]
fn string_count(s string) int {
	mut n := 0
	for i in 0 .. s.len {
		if s[i] == `x` {
			n++
		}
	}
	return n
}

@[noinline]
fn mut_local_sum() int {
	mut a := [1, 2, 3]
	mut s := 0
	for i in 0 .. a.len {
		s += a[i]
	}
	a << s
	return a.len
}

@[noinline]
fn mut_param_sum(mut a []int) int {
	mut s := 0
	for i in 0 .. a.len {
		s += a[i]
	}
	return s
}

@[noinline]
fn other_array_sum(a []int, b []int) int {
	mut s := 0
	for i in 0 .. a.len {
		s += b[i]
	}
	return s
}

@[noinline]
fn shifted_index_sum(a []int) int {
	mut s := 0
	for i in 0 .. a.len {
		s += a[i + 1]
	}
	return s
}

@[noinline]
fn sibling_mut_sum(c bool) int {
	if c {
		a := [9]
		println(a)
	}
	mut a := [1, 2, 3]
	mut s := 0
	for i in 0 .. a.len {
		s += a[i]
	}
	a << s
	return s
}

@[noinline]
fn header_address_sum(a []int) int {
	mut s := 0
	for i in 0 .. a.len {
		len_ptr := &a.len
		s += a[i] + unsafe { *len_ptr }
	}
	return s
}

fn main() {
	mut a := [1, 2, 3]
	println(param_sum(a))
	println(local_tail_sum())
	println(string_count('axbxx'))
	println(mut_local_sum())
	println(mut_param_sum(mut a))
	println(other_array_sum(a, a))
	println(shifted_index_sum([1, 2]))
	println(sibling_mut_sum(true))
	println(header_address_sum(a))
}
"

// range_len_index_body returns the body of the C definition of `name`, skipping its prototype.
fn range_len_index_body(generated string, name string) string {
	mut rest := generated
	for {
		rest = rest.all_after(' ${name}(')
		signature := rest.all_before('\n')
		if signature.ends_with(') {') || rest.len == 0 {
			break
		}
	}
	return rest.all_after('{\n').all_before('\n}')
}

fn test_range_len_loop_indexes_skip_proven_bounds_checks() {
	path := os.join_path(os.vtmp_dir(), 'range_len_index_${os.getpid()}.v')
	output := path + '.c'
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, range_len_index_source)!
	result := os.exec([@VEXE, '-o', output, path])
	assert result.exit_code == 0, result.output
	generated := os.read_file(output)!
	for name in ['param_sum', 'local_tail_sum'] {
		body := range_len_index_body(generated, name)
		assert body.contains('.data'), '${name}:\n${body}'
		assert !body.contains('array_get'), '${name}:\n${body}'
	}
	string_body := range_len_index_body(generated, 'string_count')
	assert string_body.contains('.str['), string_body
	assert !string_body.contains('string__at'), string_body
	// The array can change, or the index is not the loop variable of its length.
	for name in ['mut_local_sum', 'mut_param_sum', 'other_array_sum', 'shifted_index_sum',
		'sibling_mut_sum', 'header_address_sum'] {
		body := range_len_index_body(generated, name)
		assert body.contains('array_get'), '${name}:\n${body}'
	}
}

fn test_force_bounds_checking_keeps_range_len_loop_checks() {
	path := os.join_path(os.vtmp_dir(), 'range_len_index_forced_${os.getpid()}.v')
	output := path + '.c'
	defer {
		os.rm(path) or {}
		os.rm(output) or {}
	}
	os.write_file(path, range_len_index_source)!
	result := os.exec([@VEXE, '-force-bounds-checking', '-o', output, path])
	assert result.exit_code == 0, result.output
	generated := os.read_file(output)!
	assert range_len_index_body(generated, 'param_sum').contains('array_get')
	assert range_len_index_body(generated, 'string_count').contains('string__at')
}

fn test_range_len_loop_lambda_shadowing_keeps_bounds_checks() {
	root := os.join_path(os.vtmp_dir(), 'range_len_lambda_shadowing_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	path := os.join_path(root, 'main.v')
	output := os.join_path(root, 'program' + if os.user_os() == 'windows' { '.exe' } else { '' })
	os.write_file(path, "import os

fn array_map_index(a []int) {
	for i in 0 .. a.len {
		println([a.len].map(|i| a[i]))
	}
}

fn array_filter_index(a []int) {
	for i in 0 .. a.len {
		println([a.len].filter(|i| a[i] > 0))
	}
}

fn array_map_base(a []int) {
	for i in 0 .. a.len {
		println([[]int{}].map(|a| a[i]))
	}
}

fn array_filter_base(a []int) {
	for i in 0 .. a.len {
		println([[]int{}].filter(|a| a[i] > 0))
	}
}

fn string_map_index(s string) {
	for i in 0 .. s.len {
		println([s.len].map(|i| s[i]))
	}
}

fn string_filter_index(s string) {
	for i in 0 .. s.len {
		println([s.len].filter(|i| s[i] > 0))
	}
}

fn string_map_base(s string) {
	for i in 0 .. s.len {
		println([''].map(|s| s[i]))
	}
}

fn string_filter_base(s string) {
	for i in 0 .. s.len {
		println([''].filter(|s| s[i] > 0))
	}
}

fn main() {
	match os.args[1] {
		'array_map_index' { array_map_index([41]) }
		'array_filter_index' { array_filter_index([41]) }
		'array_map_base' { array_map_base([41]) }
		'array_filter_base' { array_filter_base([41]) }
		'string_map_index' { string_map_index('x') }
		'string_filter_index' { string_filter_index('x') }
		'string_map_base' { string_map_base('x') }
		'string_filter_base' { string_filter_base('x') }
		else { panic('unknown case') }
	}
	println('missing bounds check')
}
")!
	compiled := os.exec([@VEXE, '-gc', 'none', '-no-retry-compilation', '-o', output, path])
	assert compiled.exit_code == 0, compiled.output
	for kind in ['array', 'string'] {
		panic_text := if kind == 'array' {
			'array.get: index out of range'
		} else {
			'string index out of range'
		}
		for method in ['map', 'filter'] {
			for binding in ['index', 'base'] {
				name := '${kind}_${method}_${binding}'
				result := os.exec([output, name])
				assert result.exit_code != 0, '${name}: ${result.output}'
				assert result.output.contains(panic_text), '${name}: ${result.output}'
				assert !result.output.contains('missing bounds check'), '${name}: ${result.output}'
			}
		}
	}
}
