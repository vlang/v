// vtest ownership
import os

const capture_root = os.dir(os.dir(os.dir(os.dir(@FILE))))
const capture_tmp = os.join_path(os.vtmp_dir(), 'ownership_capture_${os.getpid()}')
const capture_compiler = os.join_path(capture_tmp, 'v-ownership')

fn testsuite_begin() {
	os.mkdir_all(capture_tmp) or { panic(err) }
	vlib := os.join_path(capture_root, 'vlib')
	build := os.exec([@VEXE, '-gc', 'none', '-d', 'ownership', '-path', '${vlib}|@vlib|@vmodules',
		'-o', capture_compiler, os.join_path(capture_root, 'vlib', 'v', 'v.v')])
	assert build.exit_code == 0, build.output
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
	vlib := os.join_path(capture_root, 'vlib')
	mut command := [capture_compiler, '-nocache', mode, '-d', 'ownership', '-path',
		'${vlib}|@vlib|@vmodules']
	if serial {
		command << '-no-parallel'
	}
	command << ['run', source]
	return os.exec(command)
}

fn test_nested_array_lambda_capture_can_be_regenerated_for_assertions() {
	code := r'fn invoke(callback fn () string) string { return callback() }
fn main() {
 first := [["one".to_owned()], ["two".to_owned()]]
 assert invoke(|| first[0][0] + first[1][0]) == "onetwo"
 second := [["three".to_owned()], ["four".to_owned()]]
 assert invoke(|| second[0][0] + second[1][0]) == "threefour"
}'
	for serial in [true, false] {
		for mode in ['-ownership', '-autofree'] {
			result := check_capture_in_mode('nested_array_lambda', code, serial, mode)
			assert result.exit_code == 0, '${mode}: ${result.output}'
		}
	}
}

fn test_aggregate_capture_checks_the_whole_source_before_transferring_it() {
	declarations := r'
fn take_map(entries map[string][]string) { assert entries.len == 2 }
fn take_items(items []string) { assert items.len == 1 }
fn invoke(callback fn () string) string { return callback() }
'
	for serial in [true, false] {
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
				code := declarations + '\nfn main() {
entries := {"one": ["one".to_owned()], "two": ["two".to_owned()]}
${before}
${capture}
${after}
}\n'
				result := check_capture('source_${lambda}_${kind}', code, serial)
				if kind == 0 {
					assert result.exit_code == 0, result.output
				} else {
					assert result.exit_code != 0, result.output
					expected := if kind == 3 {
						'cannot move `entries` because it is borrowed by `borrowed`'
					} else {
						'use of moved value: `entries'
					}
					assert result.output.contains(expected), result.output
				}
			}
		}
	}
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

fn test_capture_rejects_the_last_moved_descendant() {
	declarations := 'struct Bundle { values []string }\nfn take_items(items []string) { assert items.len == 1 }\nfn invoke(callback fn () int) int { return callback() }\n'
	for serial in [true, false] {
		for lambda in [false, true] {
			for map_root in [false, true] {
				initial := if map_root {
					'{"one": ["one".to_owned()]}'
				} else {
					'Bundle{["one".to_owned()]}'
				}
				member := if map_root { 'entries["one"]' } else { 'entries.values' }
				capture := if lambda {
					'assert invoke(|| ${member}.len) == 1'
				} else {
					// Explicitly capture without reading the moved child inside the body.
					'callback := fn [entries] () {}\ncallback()'
				}
				result := check_capture('last_descendant_${lambda}_${map_root}', declarations + '\nfn main() {\nentries := ${initial}\ntake_items(${member})\n${capture}\n}', serial)
				assert result.exit_code != 0, result.output
				assert result.output.contains('use of moved value: `entries'), result.output
				assert result.output.count(': error:') == 1, result.output
			}
		}
	}
}

fn test_capture_rejects_borrowed_descendants_and_a_second_capture() {
	for serial in [true, false] {
		for lambda in [false, true] {
			capture := if lambda {
				'assert invoke(|| entries.values.len) == 1'
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
				code := 'struct Bundle { values []string }\nfn invoke(callback fn () int) int { return callback() }\nfn main() {\nentries := Bundle{["one".to_owned()]}\n${before}\n${capture}\n${after}\n}'
				result := check_capture('overlap_${lambda}_${borrowed}', code, serial)
				assert result.exit_code != 0, result.output
				expected := if borrowed {
					'is borrowed by `borrowed`'
				} else {
					'use of moved value: `entries'
				}
				assert result.output.contains(expected), result.output
				assert result.output.count(': error:') == 1, result.output
			}
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
