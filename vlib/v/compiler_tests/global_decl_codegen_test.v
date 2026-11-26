import os
import v.tempname

fn global_decl_fixture_dir() string {
	root := os.join_path(os.vtmp_dir(), 'v3_global_decl_codegen_${tempname.unique_token()}')
	os.mkdir(root) or { panic(err) }
	return root
}

fn global_decl_run_good(name string, source string) string {
	root := global_decl_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(root, 'v3_${name}')
	compile := os.exec([@VEXE, '-enable-globals', '${src}', '-b', 'c', '-o', bin])
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.exec([bin])
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn global_decl_generate_c(name string, source string) string {
	root := global_decl_fixture_dir()
	defer { os.rmdir_all(root) or {} }
	src := os.join_path(root, 'v3_${name}.v')
	c_path := os.join_path(root, 'v3_${name}.c')
	os.write_file(src, source) or { panic(err) }
	generate := os.exec([@VEXE, '-enable-globals', '-cc', 'clang', '-o', c_path, '${src}'])
	assert generate.exit_code == 0, generate.output
	return os.read_file(c_path) or { panic(err) }
}

fn test_global_runtime_initializer_fixtures() {
	out := global_decl_run_good('global_initializer_cases', 'import sync.stdatomic

__global (
	first_flag &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
	second_flag &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)
)

fn check_typed_atomic_globals() {
	first_flag.store(true)
	println(first_flag.load())
	println(second_flag.load())
	second_flag.store(true)
	println(second_flag.load())
}

struct Entry {
	name string
}

__global entries []Entry

fn check_implicit_global_dynamic_array() {
	entries << Entry{name: \'ok\'}
	println(entries[0].name)
}

__global (
	ch chan int
	values = []int{len: 3, init: 7}
	callback = fn (n int) int {
		return n + 1
	}
)

fn send_value() {
	ch <- 9
}

fn check_grouped_global_initializers() {
	t := spawn send_value()
	got := <-ch
	t.wait()
	println(int_str(got))
	println(int_str(values.len) + \':\' + int_str(values[2]))
	println(int_str(callback(4)))
}

type Pair = [2]int

__global pair Pair = [2]int{init: 7}

fn check_aliased_fixed_array_initializer() {
	println(int_str(pair[0]) + ":" + int_str(pair[1]))
}

fn main() {
	check_typed_atomic_globals()
	check_implicit_global_dynamic_array()
	check_grouped_global_initializers()
	check_aliased_fixed_array_initializer()
}
')
	assert out == 'true\nfalse\ntrue\nok\n9\n3:7\n5\n7:7'
}

fn test_implicit_global_containers_keep_synthesized_runtime_helpers() {
	out := global_decl_run_good('implicit_global_container_helpers', "__global names []string\n__global lookup map[string]int\n\nfn main() {\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_explicit_shared_and_fixed_array_global_initializers() {
	out := global_decl_run_good('shared_and_fixed_array_global_initializers', "struct Counter {\n\tvalue int\n}\n\n__global counter shared Counter = Counter{value: 7}\n__global fixed_values shared [2]int = [3, 4]!\n__global values = [][2]int{len: 1, init: [1, 2]!}\n\nfn main() {\n\tvalue := rlock counter {\n\t\tcounter.value\n\t}\n\tfixed_value := rlock fixed_values {\n\t\tfixed_values[0] * 10 + fixed_values[1]\n\t}\n\tprintln(int_str(value))\n\tprintln(int_str(fixed_value))\n\tprintln(int_str(values[0][0]) + ':' + int_str(values[0][1]))\n}\n")
	assert out == '7\n34\n1:2'
}

fn test_explicit_shared_array_constructor_preserves_length_capacity_and_initializer() {
	out := global_decl_run_good('shared_array_constructor_initializer', "__global values shared []int = []int{len: 2, cap: 4, init: 7}

fn main() {
	summary := rlock values {
		int_str(values.len) + ':' + int_str(values.cap) + ':' + int_str(values[0]) + ':' + int_str(values[1])
	}
	println(summary)
}
")
	assert out == '2:4:7:7'
}

fn test_shared_array_literal_preserves_left_to_right_evaluation() {
	source := '__global sequence int
__global values shared []int = [next(1), next(2)]

fn next(digit int) int {
	sequence = sequence * 10 + digit
	return sequence
}

fn main() {
	summary := rlock values {
		int_str(values[0]) + ":" + int_str(values[1])
	}
	println(summary + ":" + int_str(sequence))
}
'
	c_source := global_decl_generate_c('shared_array_literal_eval_order', source)
	assert c_source.contains('array_get(values->val, 0)) = next(1);'), c_source
	assert c_source.contains('array_get(values->val, 1)) = next(2);'), c_source
	out := global_decl_run_good('shared_array_literal_eval_order', source)
	assert out == '1:12:12'
}

fn test_implicit_shared_fixed_array_container_elements_are_initialized() {
	out := global_decl_run_good('implicit_shared_fixed_array_containers', "struct Config {
	value int = 7
}

__global slots shared [2]map[string]int
__global lists shared [2][]int
__global configs shared [2]Config

fn main() {
	lock slots {
		slots[0]['k'] = 1
	}
	lock lists {
		lists[1] << 2
	}
	map_value := rlock slots {
		slots[0]['k']
	}
	array_value := rlock lists {
		lists[1][0]
	}
	default_value := rlock configs {
		configs[0].value
	}
	println(int_str(map_value) + ':' + int_str(array_value) + ':' + int_str(default_value))
}
")
	assert out == '1:2:7'
}

fn test_global_array_initializers_fill_runtime_defaults() {
	out := global_decl_run_good('global_array_runtime_defaults', "struct Config {\nmut:\n\tretries int = 7\n\tnames []string\n\tscores map[string]int\n}\n\n__global configs = []Config{len: 2}\n__global nested = [][]int{len: 2}\n__global lookups = []map[string]int{len: 2}\n\nfn main() {\n\tconfigs[0].names << 'ok'\n\tconfigs[0].scores['x'] = 5\n\tnested[0] << 9\n\tlookups[0]['x'] = 11\n\tprintln(int_str(configs[0].retries))\n\tprintln(configs[0].names[0])\n\tprintln(int_str(configs[0].scores['x']))\n\tprintln(int_str(nested[0][0]))\n\tprintln(int_str(lookups[0]['x']))\n\tprintln(int_str(configs[1].names.len) + ':' + int_str(configs[1].scores.len) + ':' + int_str(nested[1].len) + ':' + int_str(lookups[1].len))\n}\n")
	assert out == '7\nok\n5\n9\n11\n0:0:0:0'
}

fn test_global_arrays_fill_nested_fixed_array_defaults() {
	out := global_decl_run_good('global_nested_fixed_array_defaults', "struct Config {\nmut:\n\tretries int = 7\n\tnames []string\n\tscores map[string]int\n}\n\n__global rows = [][2]map[string]int{len: 1}\n__global config_rows = [][2]Config{len: 1}\n__global grids = [][2][2]map[string]int{len: 1}\n\nfn main() {\n\trows[0][0]['x'] = 13\n\tconfig_rows[0][0].names << 'fixed'\n\tconfig_rows[0][0].scores['x'] = 17\n\tgrids[0][1][1]['x'] = 19\n\tprintln(int_str(rows[0][0]['x']))\n\tprintln(int_str(config_rows[0][0].retries) + ':' + config_rows[0][0].names[0] + ':' + int_str(config_rows[0][0].scores['x']))\n\tprintln(int_str(grids[0][1][1]['x']))\n\tprintln(int_str(rows[0][1].len) + ':' + int_str(config_rows[0][1].retries) + ':' + int_str(grids[0][0][0].len))\n}\n")
	assert out == '13\n7:fixed:17\n19\n0:7:0'
}

fn test_global_array_initializer_call_preserves_fixed_array_elements() {
	out := global_decl_run_good('global_fixed_array_elements_from_call', "fn make_rows() [][2]int {\n\treturn [[1, 2]!, [3, 4]!]\n}\n\n__global rows = make_rows()\n\nfn main() {\n\tprintln(int_str(rows[0][0]) + ':' + int_str(rows[0][1]))\n\tprintln(int_str(rows[1][0]) + ':' + int_str(rows[1][1]))\n}\n")
	assert out == '1:2\n3:4'
}

fn test_global_channel_containers_are_initialized() {
	out := global_decl_run_good('global_channel_containers', '__global channels = []chan int{len: 1}
__global events shared chan int

fn send_value_to(channel chan int, value int) {
	channel <- value
}

fn main() {
	array_channel := channels[0]
	array_thread := spawn send_value_to(array_channel, 11)
	println(int_str(<-array_channel))
	array_thread.wait()

	shared_channel := rlock events {
		events
	}
	shared_thread := spawn send_value_to(shared_channel, 13)
	println(int_str(<-shared_channel))
	shared_thread.wait()
}
')
	assert out == '11\n13'
}

fn test_global_enum_and_sum_values_use_v_defaults() {
	out := global_decl_run_good('global_enum_and_sum_defaults', "enum Mode {
	ready = 7
	waiting
}

type Payload = string | int

__global modes = []Mode{len: 2}
__global shared_mode shared Mode
__global payloads = []Payload{len: 1}
__global payload shared Payload

fn describe(value Payload) string {
	return match value {
		string { 'string:' + value }
		int { 'int:' + int_str(value) }
	}
}

fn main() {
	println(if modes[0] == .ready && modes[1] == .ready { 'ready' } else { 'wrong' })
	// A local array gets the same default elements as a global one.
	local_modes := []Mode{len: 2}
	println(if local_modes[0] == .ready && local_modes[1] == .ready { 'local-ready' } else { 'local-wrong' })
	shared_mode_description := rlock shared_mode {
		if shared_mode == .ready { 'shared-ready' } else { 'shared-wrong' }
	}
	println(shared_mode_description)
	println(describe(payloads[0]))
	shared_description := rlock payload {
		describe(payload)
	}
	println(shared_description)
}
")
	assert out == 'ready\nlocal-ready\nshared-ready\nstring:\nstring:'
}

fn test_inferred_generic_atomic_globals_keep_concrete_types() {
	out := global_decl_run_good('inferred_generic_atomic_globals', 'import sync.stdatomic\n\n__global flag = stdatomic.new_atomic(false)\n__global number = stdatomic.new_atomic(7)\n\nfn main() {\n\tprintln(int_str(number.load()))\n\tprintln(flag.load())\n}\n')
	assert out == '7\nfalse'
}

fn test_volatile_global_keeps_its_name_and_qualifier() {
	// `volatile` is a qualifier on the global, not its name. The parser used to
	// read it as the name and the real name as the type, so every use of the
	// global reported an undefined identifier.
	source := 'struct Rev {\npub mut:\n\trevision u64\n}\n\n__global (\n\tvolatile base_revision = Rev{\n\t\trevision: 2\n\t}\n)\n\nfn main() {\n\tif base_revision.revision != 0 {\n\t\tprintln(u64__str(base_revision.revision))\n\t}\n}\n'
	out := global_decl_run_good('volatile_global_name', source)
	assert out == '2'
	// A kernel writes these where the hardware or the bootloader can see them,
	// so the qualifier has to reach the C declaration too.
	c_code := global_decl_generate_c('volatile_global_name_c', source)
	assert c_code.contains('volatile main__Rev base_revision'), c_code
}

fn test_volatile_global_in_ungrouped_declaration_resolves() {
	out := global_decl_run_good('volatile_global_ungrouped', '__global volatile ticks = u64(3)\n\nfn main() {\n\tticks = ticks + 1\n\tprintln(u64__str(ticks))\n}\n')
	assert out == '4'
}
