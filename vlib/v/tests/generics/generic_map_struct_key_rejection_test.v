import os

fn test_generic_maps_reject_struct_keys_before_codegen() {
	root := os.join_path(os.vtmp_dir(), 'generic_map_keys_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for key_type in ['Key', 'Alias'] {
		for init in ['map[K]int{}', 'map[K]int{keys[0]: 1}'] {
			source := os.join_path(root, 'main.v')
			os.write_file(source, 'struct Key { a string; b string }
type Alias = Key
fn count[K](keys []K) int {
    mut m := ${init}
    for key in keys { m[key] = 1 }
    return m.len
}
fn main() { println(count([${key_type}{a: "x", b: "a"}, ${key_type}{a: "x", b: "b"}])) }
')!
			result := os.exec([@VEXE, '-cc', 'clang', '-gc', 'none', '-no-retry-compilation', 'run',
				source])
			assert result.exit_code != 0, result.output
			assert result.output.contains('map key type `Key` not supported'), result.output
			assert !result.output.contains('C compilation error'), result.output
		}
	}
}

fn supported_key_count[K](keys []K) int {
	mut counts := map[K]int{}
	for key in keys { counts[key] = 1 }
	return counts.len
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
}
