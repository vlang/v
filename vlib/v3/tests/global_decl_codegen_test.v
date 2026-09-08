import os

const global_decl_vexe = @VEXE
const global_decl_tests_dir = os.dir(@FILE)
const global_decl_v3_dir = os.dir(global_decl_tests_dir)
const global_decl_vlib_dir = os.dir(global_decl_v3_dir)
const global_decl_v3_src = os.join_path(global_decl_v3_dir, 'v3.v')

fn global_decl_build_v3() string {
	v3_bin := os.join_path(os.temp_dir(), 'v3_global_decl_codegen_test')
	os.rm(v3_bin) or {}
	build :=
		os.execute('${global_decl_vexe} -gc none -path "${global_decl_vlib_dir}|@vlib|@vmodules" -o ${v3_bin} ${global_decl_v3_src}')
	assert build.exit_code == 0, build.output
	return v3_bin
}

fn global_decl_run_good(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	os.write_file(src, source) or { panic(err) }
	bin := os.join_path(os.temp_dir(), 'v3_${name}')
	compile := os.execute('${v3_bin} -enable-globals ${src} -b c -o ${bin}')
	assert compile.exit_code == 0, compile.output
	assert !compile.output.contains('C compilation failed'), compile.output
	run := os.execute(bin)
	assert run.exit_code == 0, run.output
	return run.output.trim_space()
}

fn global_decl_generate_c(v3_bin string, name string, source string) string {
	src := os.join_path(os.temp_dir(), 'v3_${name}.v')
	c_path := os.join_path(os.temp_dir(), 'v3_${name}.c')
	os.write_file(src, source) or { panic(err) }
	generate := os.execute('${v3_bin} -enable-globals -cc clang -o ${c_path} ${src}')
	assert generate.exit_code == 0, generate.output
	return os.read_file(c_path) or { panic(err) }
}

fn test_typed_global_initializers_in_group_keep_type_and_value() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'typed_global_initializers_in_group', 'import sync.stdatomic\n\n__global (\n\tfirst_flag &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)\n\tsecond_flag &stdatomic.AtomicVal[bool] = stdatomic.new_atomic(false)\n)\n\nfn main() {\n\tfirst_flag.store(true)\n\tprintln(first_flag.load())\n\tprintln(second_flag.load())\n\tsecond_flag.store(true)\n\tprintln(second_flag.load())\n}\n')
	assert out == 'true\nfalse\ntrue'
}

fn test_implicit_global_dynamic_array_is_initialized_before_append() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'implicit_global_dynamic_array', "struct Entry {\n\tname string\n}\n\n__global entries []Entry\n\nfn main() {\n\tentries << Entry{name: 'ok'}\n\tprintln(entries[0].name)\n}\n")
	assert out == 'ok'
}

fn test_implicit_global_containers_keep_synthesized_runtime_helpers() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'implicit_global_container_helpers', "__global names []string\n__global lookup map[string]int\n\nfn main() {\n\tprintln('ok')\n}\n")
	assert out == 'ok'
}

fn test_global_runtime_initializers_preserve_channels_arrays_and_fn_values() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'global_runtime_initializers', "__global (\n\tch chan int\n\tvalues = []int{len: 3, init: 7}\n\tcallback = fn (n int) int {\n\t\treturn n + 1\n\t}\n)\n\nfn send_value() {\n\tch <- 9\n}\n\nfn main() {\n\tt := spawn send_value()\n\tgot := <-ch\n\tt.wait()\n\tprintln(int_str(got))\n\tprintln(int_str(values.len) + ':' + int_str(values[2]))\n\tprintln(int_str(callback(4)))\n}\n")
	assert out == '9\n3:7\n5'
}

fn test_explicit_shared_and_fixed_array_global_initializers() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'shared_and_fixed_array_global_initializers', "struct Counter {\n\tvalue int\n}\n\n__global counter shared Counter = Counter{value: 7}\n__global fixed_values shared [2]int = [3, 4]!\n__global values = [][2]int{len: 1, init: [1, 2]!}\n\nfn main() {\n\tvalue := rlock counter {\n\t\tcounter.value\n\t}\n\tfixed_value := rlock fixed_values {\n\t\tfixed_values[0] * 10 + fixed_values[1]\n\t}\n\tprintln(int_str(value))\n\tprintln(int_str(fixed_value))\n\tprintln(int_str(values[0][0]) + ':' + int_str(values[0][1]))\n}\n")
	assert out == '7\n34\n1:2'
}

fn test_aliased_fixed_array_global_initializer_uses_copy() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'aliased_fixed_array_global_initializer', 'type Pair = [2]int\n\n__global pair Pair = [2]int{init: 7}\n\nfn main() {\n\tprintln(int_str(pair[0]) + ":" + int_str(pair[1]))\n}\n')
	assert out == '7:7'
}

fn test_explicit_shared_array_constructor_preserves_length_capacity_and_initializer() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'shared_array_constructor_initializer', "__global values shared []int = []int{len: 2, cap: 4, init: 7}

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
	v3_bin := global_decl_build_v3()
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
	c_source := global_decl_generate_c(v3_bin, 'shared_array_literal_eval_order', source)
	assert c_source.contains('array_get(values->val, 0)) = next(1);'), c_source
	assert c_source.contains('array_get(values->val, 1)) = next(2);'), c_source
	out := global_decl_run_good(v3_bin, 'shared_array_literal_eval_order', source)
	assert out == '1:12:12'
}

fn test_implicit_shared_fixed_array_container_elements_are_initialized() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'implicit_shared_fixed_array_containers', "struct Config {
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
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'global_array_runtime_defaults', "struct Config {\nmut:\n\tretries int = 7\n\tnames []string\n\tscores map[string]int\n}\n\n__global configs = []Config{len: 2}\n__global nested = [][]int{len: 2}\n__global lookups = []map[string]int{len: 2}\n\nfn main() {\n\tconfigs[0].names << 'ok'\n\tconfigs[0].scores['x'] = 5\n\tnested[0] << 9\n\tlookups[0]['x'] = 11\n\tprintln(int_str(configs[0].retries))\n\tprintln(configs[0].names[0])\n\tprintln(int_str(configs[0].scores['x']))\n\tprintln(int_str(nested[0][0]))\n\tprintln(int_str(lookups[0]['x']))\n\tprintln(int_str(configs[1].names.len) + ':' + int_str(configs[1].scores.len) + ':' + int_str(nested[1].len) + ':' + int_str(lookups[1].len))\n}\n")
	assert out == '7\nok\n5\n9\n11\n0:0:0:0'
}

fn test_global_arrays_fill_nested_fixed_array_defaults() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'global_nested_fixed_array_defaults', "struct Config {\nmut:\n\tretries int = 7\n\tnames []string\n\tscores map[string]int\n}\n\n__global rows = [][2]map[string]int{len: 1}\n__global config_rows = [][2]Config{len: 1}\n__global grids = [][2][2]map[string]int{len: 1}\n\nfn main() {\n\trows[0][0]['x'] = 13\n\tconfig_rows[0][0].names << 'fixed'\n\tconfig_rows[0][0].scores['x'] = 17\n\tgrids[0][1][1]['x'] = 19\n\tprintln(int_str(rows[0][0]['x']))\n\tprintln(int_str(config_rows[0][0].retries) + ':' + config_rows[0][0].names[0] + ':' + int_str(config_rows[0][0].scores['x']))\n\tprintln(int_str(grids[0][1][1]['x']))\n\tprintln(int_str(rows[0][1].len) + ':' + int_str(config_rows[0][1].retries) + ':' + int_str(grids[0][0][0].len))\n}\n")
	assert out == '13\n7:fixed:17\n19\n0:7:0'
}

fn test_global_array_initializer_call_preserves_fixed_array_elements() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'global_fixed_array_elements_from_call', "fn make_rows() [][2]int {\n\treturn [[1, 2]!, [3, 4]!]\n}\n\n__global rows = make_rows()\n\nfn main() {\n\tprintln(int_str(rows[0][0]) + ':' + int_str(rows[0][1]))\n\tprintln(int_str(rows[1][0]) + ':' + int_str(rows[1][1]))\n}\n")
	assert out == '1:2\n3:4'
}

fn test_global_channel_containers_are_initialized() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'global_channel_containers', '__global channels = []chan int{len: 1}
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
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'global_enum_and_sum_defaults', "enum Mode {
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
	println(if modes[0] == .ready { 'ready' } else { 'wrong' })
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
	assert out == 'ready\nshared-ready\nstring:\nstring:'
}

fn test_inferred_generic_atomic_globals_keep_concrete_types() {
	v3_bin := global_decl_build_v3()
	out := global_decl_run_good(v3_bin, 'inferred_generic_atomic_globals', 'import sync.stdatomic\n\n__global flag = stdatomic.new_atomic(false)\n__global number = stdatomic.new_atomic(7)\n\nfn main() {\n\tprintln(int_str(number.load()))\n\tprintln(flag.load())\n}\n')
	assert out == '7\nfalse'
}
