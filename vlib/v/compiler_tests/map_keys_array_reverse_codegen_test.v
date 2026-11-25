import os
import v.tempname

fn test_map_keys_and_array_reverse_keep_concrete_array_types() {
	dir := os.join_path(os.vtmp_dir(), 'v3_map_keys_array_reverse_${tempname.unique_token()}')
	os.mkdir(dir)!
	defer { os.rmdir_all(dir) or {} }
	bin := os.join_path(dir, 'v3_map_keys_array_reverse_input')
	src := bin + '.v'
	os.write_file(src, "fn first_key(m map[string][]string) string {
	keys := m.keys()
	key := keys.first()
	return key
}

fn main() {
	m := {
		'A':  ['B', 'C']
		'DD': ['E']
	}
	mut key_score := 0
	for k in m.keys() {
		key_score += k.len
	}
	assert key_score == 3

	keys := m.keys()
	mut keys_score := 0
	for k in keys {
		keys_score += k.len
	}
	assert keys_score == 3

	mut values := ''
	for v in m['A'] {
		values += v
	}
	assert values == 'BC'

	int_map := {
		1:  'one'
		20: 'twenty'
	}
	mut int_key_total := 0
	for k in int_map.keys() {
		int_key_total += k
	}
	assert int_key_total == 21

	mut arr := []string{}
	arr << 'one'
	arr << 'two'
	arr << 'three'
	mut rev := ''
	for v in arr.reverse() {
		rev += v + ';'
	}
	assert rev == 'three;two;one;'

	mut nums := []int{}
	nums << 3
	nums << 5
	nums << 8
	mut reversed_number := 0
	for v in nums.reverse() {
		reversed_number = reversed_number * 10 + v
	}
	assert reversed_number == 853

	single := {
		'z': ['payload']
	}
	assert first_key(single) == 'z'
	println('ok')
}
") or {
		panic(err)
	}

	compile := os.exec([@VEXE, src, '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output

	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'ok'
}
