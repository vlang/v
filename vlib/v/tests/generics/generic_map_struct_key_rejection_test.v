import os

fn test_generic_maps_reject_struct_keys_before_codegen() {
	root := os.join_path(os.vtmp_dir(), 'generic_map_keys_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for key_type in ['Key', 'Alias'] {
		for init in ['map[K]int{}', 'map[K]int{keys[0]: 1}', '{keys[0]: 1}'] {
			for in_comptime_loop in [false, true] {
				statements := 'mut m := ${init}\nfor key in keys { m[key] = 1 }\nreturn m.len'
				body := if in_comptime_loop {
					'\$for field in K.fields {\n${statements}\n}\nreturn 0'
				} else {
					statements
				}
				source := os.join_path(root, 'main.v')
				os.write_file(source, 'struct Key { a string; b string }
type Alias = Key
fn count[K](keys []K) int {
    ${body}
}
fn main() { println(count([${key_type}{a: "x", b: "a"}, ${key_type}{a: "x", b: "b"}])) }
')!
				result := os.exec([@VEXE, '-cc', 'clang', '-gc', 'none', '-no-retry-compilation',
					'run', source])
				assert result.exit_code != 0, '${key_type}, ${init}, comptime=${in_comptime_loop}: ${result.output}'
				assert result.output.contains('map key type `Key` not supported'), result.output
				assert !result.output.contains('C compilation error'), result.output
			}
		}
	}
}

fn test_inactive_comptime_map_keys_are_not_rejected() {
	assert inactive_struct_map[StructWithoutFields]() == 1
	assert inactive_struct_map[KeyWithoutStringFields]() == 1
	assert inactive_inferred_struct_map([StructWithoutFields{}]) == 1
	assert inactive_inferred_struct_map([KeyWithoutStringFields{ value: 1 }]) == 1
}

struct StructWithoutFields {}

struct KeyWithoutStringFields {
	value int
}

fn inactive_struct_map[K]() int {
	$for field in K.fields {
		$if field.typ is string {
			_ = map[K]int{}
		}
	}
	return 1
}

fn inactive_inferred_struct_map[K](keys []K) int {
	$for field in K.fields {
		$if field.typ is string {
			mut counts := {
				keys[0]: 1
			}
			counts[keys[0]] = 2
			assert counts.len == 1
		}
	}
	return 1
}

fn supported_key_count[K](keys []K) int {
	mut counts := map[K]int{}
	for key in keys { counts[key] = 1 }
	return counts.len
}

fn supported_inferred_key_count[K](keys []K) int {
	mut counts := {
		keys[0]: 1
	}
	for key in keys { counts[key] = 1 }
	return counts.len
}

fn supported_comptime_key_count[K](keys []K) int {
	$for field in KeyWithoutStringFields.fields {
		mut counts := {
			keys[0]: 1
		}
		for key in keys { counts[key] = 1 }
		return counts.len
	}
	return 0
}

enum Color {
	red
	blue
}

type NumberKey = int

fn test_generic_maps_keep_supported_key_types() {
	assert supported_key_count([1, 2]) == 2
	assert supported_key_count(['x', 'y']) == 2
	assert supported_key_count([Color.red, .blue]) == 2
	assert supported_key_count([NumberKey(1), NumberKey(2)]) == 2
	assert supported_key_count([[1, 2]!, [1, 3]!]) == 2
	assert supported_inferred_key_count([1, 2]) == 2
	assert supported_inferred_key_count(['x', 'y']) == 2
	assert supported_inferred_key_count([Color.red, .blue]) == 2
	assert supported_inferred_key_count([NumberKey(1), NumberKey(2)]) == 2
	assert supported_inferred_key_count([[1, 2]!, [1, 3]!]) == 2
	assert supported_comptime_key_count([1, 2]) == 2
	assert supported_comptime_key_count(['x', 'y']) == 2
	assert supported_comptime_key_count([Color.red, .blue]) == 2
	assert supported_comptime_key_count([NumberKey(1), NumberKey(2)]) == 2
	assert supported_comptime_key_count([[1, 2]!, [1, 3]!]) == 2
}
