// vtest ownership
import os
import v.cmdexec
import v.tempname

const capture_root = os.dir(os.dir(os.dir(os.dir(@FILE))))
const capture_tmp = os.join_path(os.vtmp_dir(), 'ownership_capture_${tempname.unique_token()}')

fn testsuite_begin() {
	os.mkdir_all(capture_tmp) or { panic(err) }
}

fn testsuite_end() {
	os.rmdir_all(capture_tmp) or {}
}

fn check_capture(name string, code string, serial bool) os.Result {
	return check_capture_in_mode(name, code, serial, '-ownership')
}

fn check_capture_in_mode(name string, code string, serial bool, mode string) os.Result {
	source := os.join_path(capture_tmp, '${name}.v')
	os.write_file(source, code) or { panic(err) }
	return run_capture(source, serial, mode)
}

fn run_capture(source string, serial bool, mode string, flags ...string) os.Result {
	shared := os.getenv('V3_TEST_UNIT_SHARED_V3_OWNERSHIP')
	v3_src := os.join_path(os.dir(@FILE), '../v.v')
	compiler := if os.is_file(shared) && os.is_executable(shared)
		&& os.real_path(os.getenv('V3_TEST_UNIT_V3_SRC')) == os.real_path(v3_src) {
		os.real_path(shared)
	} else {
		@VEXE
	}
	vlib := os.join_path(capture_root, 'vlib')
	mut args := ['-nocache', mode, '-d', 'ownership', '-path', '${vlib}|@vlib|@vmodules']
	if serial {
		args << '-no-parallel'
	}
	args << [...flags, 'run', source]
	return cmdexec.run_in_merged_with_env(compiler, args, '', {
		'VFLAGS':               ''
		'VTOOLS_CACHE_VERBOSE': ''
	})
}

fn nested_array_lambda_capture(name string) string {
	return 'fn ${name}() {
 first := [["one".to_owned()], ["two".to_owned()]]
 assert invoke(|| first[0][0] + first[1][0]) == "onetwo"
 second := [["three".to_owned()], ["four".to_owned()]]
 assert invoke(|| second[0][0] + second[1][0]) == "threefour"
}'
}

fn test_autofree_nested_array_lambda_capture_can_be_regenerated_for_assertions() {
	code := 'fn invoke(callback fn () string) string { return callback() }\n' +
		nested_array_lambda_capture('main')
	for serial in [true, false] {
		result := check_capture_in_mode('nested_array_lambda', code, serial, '-autofree')
		assert result.exit_code == 0, result.output
	}
}

fn test_aggregate_capture_checks_the_whole_source_before_transferring_it() {
	declarations := r'
fn take_map(entries map[string][]string) { assert entries.len == 2 }
fn take_items(items []string) { assert items.len == 1 }
fn invoke(callback fn () string) string { return callback() }
'
	mut accepted := declarations
	mut calls := []string{}
	mut rejections := []CaptureRejection{}
	for lambda in [false, true] {
		capture := if lambda {
			'assert invoke(|| entries["one"][0]) == "one"'
		} else {
			'callback := fn [entries] () string { return entries["one"][0] }
assert callback() == "one"'
		}
		for kind, before in ['', 'take_map(entries)', 'take_items(entries["one"])',
			'borrowed := &entries', ''] {
			after := match kind {
				3 { 'assert borrowed.len == 2' }
				4 { 'assert entries.len == 2' }
				else { '' }
			}
			body := 'entries := {"one": ["one".to_owned()], "two": ["two".to_owned()]}
${before}
${capture}
${after}'
			if kind == 0 {
				name := if lambda { 'check_lambda_capture' } else { 'check_literal_capture' }
				accepted += '\nfn ${name}() {\n${body}\n}\n'
				calls << '\t${name}()'
				continue
			}
			expected := if kind == 3 {
				'cannot move `entries` because it is borrowed by `borrowed`'
			} else {
				'use of moved value: `entries'
			}
			rejections << CaptureRejection{'source_${lambda}_${kind}', body, expected}
		}
	}
	accepted += '\n' + nested_array_lambda_capture('check_nested_array_lambda_capture') + '\n'
	calls << '\tcheck_nested_array_lambda_capture()'
	accepted += '\nfn main() {\n${calls.join('\n')}\n}\n'
	for serial in [true, false] {
		result := check_capture('aggregate_capture', accepted, serial)
		assert result.exit_code == 0, result.output
	}
	rejections << capture_descendant_rejections()
	check_capture_rejections(declarations + '
struct Bundle { values []string }
fn invoke_int(callback fn () int) int { return callback() }
', rejections)!
}

fn test_captured_aggregate_descendants_are_released_once() {
	code := r'@[has_globals]
module main
import builtin.closure

__global next_id = 0
__global dropped = map[int]bool{}

struct Resource implements IClone, Drop {
 id int
}

fn fresh() Resource {
 next_id++
 return Resource{next_id}
}

fn (resource &Resource) clone() Resource { return fresh() }

fn (mut resource Resource) drop() {
 assert !dropped[resource.id], "resource dropped twice"
 dropped[resource.id] = true
}

fn invoke(callback fn ()) { callback() }

fn literal_capture() {
 entries := {"one": [fresh()], "two": [fresh()]}
 callback := fn [entries] () {
  assert !dropped[entries["one"][0].id]
  assert !dropped[entries["two"][0].id]
 }
 callback()
}

fn lambda_capture() {
 entries := {"one": [fresh()], "two": [fresh()]}
 invoke(|| {
  assert !dropped[entries["one"][0].id]
  assert !dropped[entries["two"][0].id]
 })
}

fn main() {
 mut lifetime := closure.new_lifetime()
 lifetime.frame(fn () { literal_capture(); lambda_capture() }) or { panic(err) }
 lifetime.dispose() or { panic(err) }
 assert next_id >= 4
 assert dropped.len == next_id
 println("ok")
}
'
	for serial in [true, false] {
		result := check_capture('drop_descendants', code, serial)
		assert result.exit_code == 0, result.output
		assert result.output.trim_space().ends_with('ok'), result.output
	}
}

struct CaptureRejection {
	name     string
	body     string
	expected string
}

fn capture_descendant_rejections() []CaptureRejection {
	mut cases := []CaptureRejection{}
	for lambda in [false, true] {
		for map_root in [false, true] {
			initial := if map_root {
				'{"one": ["one".to_owned()]}'
			} else {
				'Bundle{["one".to_owned()]}'
			}
			member := if map_root { 'entries["one"]' } else { 'entries.values' }
			capture := if lambda {
				'assert invoke_int(|| ${member}.len) == 1'
			} else {
				// Explicitly capture without reading the moved child inside the body.
				'callback := fn [entries] () {}\ncallback()'
			}
			body := 'entries := ${initial}\ntake_items(${member})\n${capture}'
			cases << CaptureRejection{'last_descendant_${lambda}_${map_root}', body, 'use of moved value: `entries'}
		}
		capture := if lambda {
			'assert invoke_int(|| entries.values.len) == 1'
		} else {
			'callback := fn [entries] () {}\ncallback()'
		}
		for borrowed in [false, true] {
			before := if borrowed {
				'borrowed := &entries.values'
			} else {
				capture.replace('callback', 'first')
			}
			after := if borrowed { 'assert borrowed.len == 1' } else { '' }
			body := 'entries := Bundle{["one".to_owned()]}\n${before}\n${capture}\n${after}'
			expected := if borrowed {
				'is borrowed by `borrowed`'
			} else {
				'use of moved value: `entries'
			}
			cases << CaptureRejection{'overlap_${lambda}_${borrowed}', body, expected}
		}
	}
	return cases
}

fn check_capture_rejections(declarations string, cases []CaptureRejection) ! {
	batch := os.join_path(capture_tmp, 'rejections')
	os.mkdir(batch)!
	mut calls := []string{cap: cases.len}
	for case in cases {
		os.write_file(os.join_path(batch, '${case.name}.v'), 'module main\n\nfn ${case.name}() {\n${case.body}\n}\n')!
		calls << '\t${case.name}()'
	}
	os.write_file(os.join_path(batch, 'main.v'), 'module main\n\n${declarations}\nfn main() {\n${calls.join('\n')}\n}\n')!
	for serial in [true, false] {
		result := run_capture(batch, serial, '-ownership', '-nocolor')
		assert result.exit_code != 0, result.output
		diagnostics := result.output.split_into_lines().filter(it.contains(': error: '))
		assert diagnostics.len == cases.len, result.output
		for case in cases {
			assert diagnostics.any(it.all_before(': error: ').contains('${case.name}.v:')
				&& it.all_after(': error: ').contains(case.expected)), '${case.name}: ${result.output}'
		}
	}
}

fn test_nested_local_shadowing_remains_an_error() {
	for serial in [true, false] {
		result := check_capture('local_shadow', r'fn main() {
 value := "outer"
 { value := "inner"; assert value == "inner" }
 assert value == "outer"
}', serial)
		assert result.exit_code != 0, result.output
		assert result.output.contains('redefinition of `value`'), result.output
	}
}
