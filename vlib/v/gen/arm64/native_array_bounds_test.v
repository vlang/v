module arm64

import os

struct NativeArrayBoundsCase {
	name     string
	body     string
	expected string
	panics   bool
}

fn exec_native_bounds(args []string) os.Result {
	result := os.exec(args)
	if !result.output.contains('ARM64 support is not compiled into this executable') {
		return result
	}
	compiler := os.join_path(os.vtmp_dir(), 'arm64_bounds_compiler_${os.getpid()}')
	defer {
		os.rm(compiler) or {}
	}
	bootstrap := os.exec([@VEXE, '-gc', 'none', '-d', 'skip_fastc', '-compile-backend', 'arm64',
		'-o', compiler, os.join_path(@VEXEROOT, 'cmd', 'v')])
	assert bootstrap.exit_code == 0, bootstrap.output
	return os.exec(args.map(if it == @VEXE { compiler } else { it }))
}

fn macos_native_compile_env() []string {
	developer_dir := os.exec(['/usr/bin/xcode-select', '-p'])
	assert developer_dir.exit_code == 0, developer_dir.output
	sdk_root := os.exec(['/usr/bin/xcrun', '--sdk', 'macosx', '--show-sdk-path'])
	assert sdk_root.exit_code == 0, sdk_root.output
	return [
		'env',
		'VFLAGS=',
		'DEVELOPER_DIR=${developer_dir.output.trim_space()}',
		'SDKROOT=${sdk_root.output.trim_space()}',
		'CC=/usr/bin/clang',
	]
}

fn compile_native_arm64(output string, source_path string, extra_flags []string) os.Result {
	mut args := macos_native_compile_env()
	args << @VEXE
	args << '-gc'
	args << 'none'
	args << '-nocache'
	args << '-b'
	args << 'arm64'
	args << '-os'
	args << 'macos'
	args << '-cc'
	args << '/usr/bin/clang'
	args << extra_flags
	args << '-o'
	args << output
	args << source_path
	return exec_native_bounds(args)
}

fn native_bounds_source(body string) string {
	prefix := 'const constant_values = [i32(10), i32(20), i32(30)]

@[noinline]
fn invalid_index() int { return 5 }

@[noinline]
fn negative_index() int { return -1 }

@[noinline]
fn valid_index() int { return 2 }

@[noinline]
fn unsigned_index() u64 { return u64(18446744073709551615) }

@[direct_array_access]
fn generic_direct_get[T](values []T, index int) T {
	return values[index]
}

@[direct_array_access]
fn generic_direct_set[T](mut values []T, index int, value T) {
	values[index] = value
}

struct Holder {
mut:
	items []int
}

fn grow(mut values []int) int {
	values << 44
	return 55
}

fn grow_outer(mut matrix [][]int) int {
	matrix[0] << 44
	matrix << [40]
	return 55
}

fn grow_and_replace_holder(mut current &Holder, next &Holder) int {
	current.items << 44
	current = next
	return 55
}

fn main() {
'
	return prefix + body + '\n}\n'
}

fn run_native_array_bounds_case(root string, case NativeArrayBoundsCase, extra_flags []string) {
	source_path := os.join_path(root, '${case.name}.v')
	c_output := os.join_path(root, '${case.name}_c')
	arm64_output := os.join_path(root, '${case.name}_arm64')
	os.write_file(source_path, native_bounds_source(case.body)) or { panic(err) }
	mut c_args := ['env', 'VFLAGS=', @VEXE, '-gc', 'none', '-nocache']
	c_args << extra_flags
	c_args << ['-b', 'c', '-o', c_output, source_path]
	c_compile := os.exec(c_args)
	assert c_compile.exit_code == 0, '${case.name} C compile: ${c_compile.output}'
	arm64_compile := compile_native_arm64(arm64_output, source_path, extra_flags)
	assert arm64_compile.exit_code == 0, '${case.name} ARM64 compile: ${arm64_compile.output}'
	for output in [c_output, arm64_output] {
		result := os.exec([output])
		if case.panics {
			assert result.exit_code != 0, '${case.name} unexpectedly succeeded: ${result.output}'
			if case.expected.len > 0 {
				assert result.output.contains(case.expected), '${case.name}: ${result.output}'
			}
		} else {
			assert result.exit_code == 0, '${case.name}: ${result.output}'
			if case.expected.len > 0 {
				assert result.output.contains(case.expected), '${case.name}: ${result.output}'
			}
		}
	}
}

fn test_native_array_index_and_slice_bounds_panic() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_array_bounds_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		cases := [
			NativeArrayBoundsCase{
				name:     'dynamic_read_oob'
				body:     'values := [10, 20, 30]\nprintln(values[invalid_index()])'
				expected: 'array.get: index out of range'
				panics:   true
			},
			NativeArrayBoundsCase{
				name:   'const_read_oob'
				body:   'println(constant_values[invalid_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'array_write_oob'
				body:   'mut values := [10, 20, 30]\nvalues[invalid_index()] = 40'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_array_write'
				body:   'mut values := [10, 20, 30]\nvalues[negative_index()] = 40'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'fixed_read_oob'
				body:   'values := [3]int{init: 10}\nprintln(values[invalid_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'fixed_write_oob'
				body:   'mut values := [3]int{init: 10}\nvalues[invalid_index()] = 40'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_fixed_write'
				body:   'mut values := [3]int{init: 10}\nvalues[negative_index()] = 40'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'string_read_oob'
				body:   'value := "abc"\nprintln(value[invalid_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'unsigned_read_oob'
				body:   'values := [10, 20, 30]\nprintln(values[unsigned_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_array_read'
				body:   'values := [10, 20, 30]\nprintln(values[negative_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_fixed_read'
				body:   'values := [3]int{init: 10}\nprintln(values[negative_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_string_read'
				body:   'value := "abc"\nprintln(value[negative_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'array_slice_oob'
				body:   'values := [10, 20, 30]\nprintln(values[0..invalid_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'fixed_slice_oob'
				body:   'values := [3]int{init: 10}\nprintln(values[0..invalid_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_array_slice_start'
				body:   'values := [10, 20, 30]\nprintln(values[negative_index()..])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_array_slice_end'
				body:   'values := [10, 20, 30]\nprintln(values[..negative_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_fixed_slice_start'
				body:   'values := [3]int{init: 10}\nprintln(values[negative_index()..])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_fixed_slice_end'
				body:   'values := [3]int{init: 10}\nprintln(values[..negative_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_string_slice_start'
				body:   'value := "abc"\nprintln(value[negative_index()..])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'negative_string_slice_end'
				body:   'value := "abc"\nprintln(value[..negative_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'empty_read'
				body:   'values := []int{}\nprintln(values[invalid_index()])'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'empty_first'
				body:   'values := []int{}\nprintln(values.first())'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'empty_last'
				body:   'values := []int{}\nprintln(values.last())'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'empty_pop'
				body:   'mut values := []int{}\nprintln(values.pop())'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'empty_pop_left'
				body:   'mut values := []int{}\nprintln(values.pop_left())'
				panics: true
			},
			NativeArrayBoundsCase{
				name:   'empty_delete_last'
				body:   'mut values := []int{}\nvalues.delete_last()'
				panics: true
			},
			NativeArrayBoundsCase{
				name:     'dynamic_read_in_range'
				body:     'values := [10, 20, 30]\nprintln(values[valid_index()])'
				expected: '30'
			},
			NativeArrayBoundsCase{
				name:     'const_read_in_range'
				body:     'println(constant_values[valid_index()])'
				expected: '30'
			},
			NativeArrayBoundsCase{
				name:     'gated_negative_slice'
				body:     'values := [10, 20, 30]\nprintln(values#[negative_index()..])'
				expected: '30'
			},
			NativeArrayBoundsCase{
				name:     'direct_generic_read'
				body:     'mut values := [10, 20, 30, 40]\nvalues.pop()\ngeneric_direct_set(mut values, 3, 99)\nprintln(generic_direct_get(values, 3))'
				expected: '99'
			},
		]
		for case in cases {
			run_native_array_bounds_case(root, case, []string{})
		}
	}
}

fn test_native_array_store_checks_after_rhs_can_grow_array() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_array_store_growth_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		case := NativeArrayBoundsCase{
			name:     'array_store_growth'
			body:     'mut values := [10, 20, 30]
values[3] = grow(mut values)
if values.len != 4 || values[3] != 55 { panic("store did not use resized array") }
mut matrix := [[10, 20, 30]]
matrix[0][3] = grow(mut matrix[0])
if matrix[0].len != 4 || matrix[0][3] != 55 { panic("nested store used stale array data") }
mut outer_matrix := [[10, 20, 30]]
outer_matrix[0][3] = grow_outer(mut outer_matrix)
if outer_matrix.len != 2 || outer_matrix[0].len != 4 || outer_matrix[0][3] != 55 {
	panic("nested store used stale outer array data")
}
mut first := &Holder{items: [10, 20, 30]}
second := &Holder{items: [1, 2, 3, 4]}
mut current := first
current.items[3] = grow_and_replace_holder(mut current, second)
println(first.items.len.str() + ", " + first.items[3].str() + ", " + second.items[3].str())'
			expected: '4, 55, 4'
		}
		run_native_array_bounds_case(root, case, []string{})
	}
}

fn test_native_force_bounds_checking_overrides_direct_access() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_force_bounds_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		case := NativeArrayBoundsCase{
			name:   'force_direct_generic_write'
			body:   'mut values := [10, 20, 30, 40]\nvalues.pop()\ngeneric_direct_set(mut values, 3, 99)'
			panics: true
		}
		run_native_array_bounds_case(root, case, ['-force-bounds-checking'])
	}
}

fn test_native_no_bounds_and_unsafe_allow_logical_oob_within_capacity() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_no_bounds_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		no_bounds_case := NativeArrayBoundsCase{
			name:     'no_bounds_stale_capacity_read'
			body:     'mut values := [10, 20, 30, 40]\nvalues.pop()\nprintln(values[3])'
			expected: '40'
		}
		run_native_array_bounds_case(root, no_bounds_case, ['-no-bounds-checking'])
		no_bounds_first_case := NativeArrayBoundsCase{
			name:     'no_bounds_first_stale_capacity_read'
			body:     'mut values := [40]\nvalues.pop()\nprintln(values.first())'
			expected: '40'
		}
		run_native_array_bounds_case(root, no_bounds_first_case, ['-no-bounds-checking'])
		unsafe_case := NativeArrayBoundsCase{
			name:     'unsafe_stale_capacity_read'
			body:     'mut values := [10, 20, 30, 40]\nvalues.pop()\nunsafe { println(values[3]) }'
			expected: '40'
		}
		run_native_array_bounds_case(root, unsafe_case, []string{})
	}
}

fn test_native_prod_array_bounds_check() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_prod_bounds_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		case := NativeArrayBoundsCase{
			name:     'prod_array_read_oob'
			body:     'values := [10, 20, 30]\nprintln(values[invalid_index()])'
			expected: 'array.get: index out of range'
			panics:   true
		}
		run_native_array_bounds_case(root, case, ['-prod'])
	}
}

fn test_native_wide_index_read_and_assignment_are_rejected() {
	$if macos && arm64 {
		root := os.join_path(os.vtmp_dir(), 'arm64_wide_index_${os.getpid()}')
		os.mkdir_all(root)!
		defer { os.rmdir_all(root) or {} }
		for mode in ['read', 'assignment'] {
			source_path := os.join_path(root, '${mode}.v')
			output := os.join_path(root, '${mode}_arm64')
			body := if mode == 'read' {
				'values := [10, 20, 30]\nindex := i128(0)\nprintln(values[index])'
			} else {
				'mut values := [10, 20, 30]\nindex := i128(0)\nvalues[index] = 40'
			}
			os.write_file(source_path, native_bounds_source(body))!
			compiled := compile_native_arm64(output, source_path, []string{})
			assert compiled.exit_code != 0, '${mode} compiled despite using an i128 index'
			assert compiled.output.contains('unsupported 128-bit integer operation: index or slice bound'), '${mode}: ${compiled.output}'
		}
	}
}
