module fastc

import os
import v3.pref

fn test_fastc_parser_emits_arm64_without_c() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, "\$if arm64 ? {\n\tfn selected() int {\n\t\treturn 2\n\t}\n} \$else {\n\tfn selected() int {\n\t\treturn 100\n\t}\n}\n\nfn add(a int, b int) int {\n\treturn a + b\n}\n\nfn is_letter(c u8) bool {\n\treturn (c >= `a` && c <= `z`) || (c >= `A` && c <= `Z`)\n}\n\nfn main() {\n\tmut sum := 0\n\tfor sum < 3 {\n\t\tsum += 1\n\t}\n\tif add(sum, selected()) == 5 && is_letter(111) && native_features() {\n\t\t\$if arm64 ? {\n\t\t\tprintln('native')\n\t\t} \$else {\n\t\t\tprintln('wrong')\n\t\t}\n\t} else {\n\t\tprintln('wrong')\n\t}\n}\n\nenum FastArm64Mode {\n\tcold\n\twarm\n}\n\nstruct FastArm64Counter {\nmut:\n\tvalue int\n}\n\nfn (mut counter FastArm64Counter) bump() {\n\tcounter.value += 1\n}\n\nfn native_features() bool {\n\tmut counter := FastArm64Counter{}\n\tcounter.bump()\n\tmut labels := map[string]string{}\n\tlabels['backend'] = 'native'\n\tmut modes := map[string]FastArm64Mode{}\n\tmodes['backend'] = FastArm64Mode.warm\n\treturn counter.value == 1 && labels['backend'] == 'native' && modes['backend'] == .warm && 0x100000000 > 0xffffffff && `'` == 39\n}\n") or {
			panic(err)
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		result := generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		assert result.source_paths.len > 0
		assert os.is_executable(output_path)
		assert !os.exists(output_path + '.c')
		run_result := os.execute(output_path)
		assert run_result.exit_code == 0
		assert run_result.output == 'native\n'
	}
}

fn test_fastc_arm64_lexical_scopes_and_array_mutation() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_scopes_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		output_path := os.join_path_single(test_dir, 'app')
		source := '
enum FastArm64DefaultMode {
	cold
	warm
}

struct FastArm64Defaults {
	retries int = 3
	enabled bool = true
	mode FastArm64DefaultMode = .warm
}

struct FastArm64CustomError {}

fn (err FastArm64CustomError) msg() string {
	return "custom"
}

fn (err FastArm64CustomError) code() int {
	return 0
}

fn add_after_return(mut value int) {
	defer {
		value += 7
	}
	return
}

fn maybe_value(ok bool) ?int {
	if ok {
		return 7
	}
	return none
}

fn result_value(ok bool) !int {
	if ok {
		return 9
	}
	return error("failed")
}

fn custom_error_value() !int {
	return FastArm64CustomError{}
}

fn propagated_result(ok bool) !int {
	value := result_value(ok)!
	return value + 1
}

fn propagated_option(ok bool) ?int {
	value := maybe_value(ok)?
	return value + 2
}

fn main() {
	mut shadow := 1
	if true {
		shadow := 2
		if shadow != 2 {
			println("wrong shadow inner")
			return
		}
	}
	if shadow != 1 {
		println("wrong shadow outer")
		return
	}
	mut defer_state := 0
	if false {
		defer {
			defer_state = 99
		}
	}
	if true {
		defer {
			defer_state += 2
		}
		defer {
			defer_state *= 3
		}
		defer_state = 1
	}
	if defer_state != 5 {
		println("wrong scoped defer")
		return
	}
	mut match_state := 0
	match 1 {
		1 {
			defer {
				match_state += 4
			}
		}
		else {
			defer {
				match_state = 99
			}
		}
	}
	if match_state != 4 {
		println("wrong match defer")
		return
	}
	mut break_state := 0
	for break_state == 0 {
		defer {
			break_state += 10
		}
		break
	}
	if break_state != 10 {
		println("wrong break defer")
		return
	}
	mut passes := 0
	mut continue_state := 0
	for passes < 1 {
		passes++
		defer {
			continue_state += 3
		}
		continue
	}
	if continue_state != 3 {
		println("wrong continue defer")
		return
	}
	mut returned := 0
	add_after_return(mut returned)
	if returned != 7 {
		println("wrong return defer")
		return
	}
	mut original := [1, 2]
	mut sliced := original[1..]
	sliced << 3
	if sliced.len != 2 || sliced[0] != 2 || sliced[1] != 3 || original[0] != 1 || original[1] != 2 {
		println("wrong slice growth")
		return
	}
	mut spare_base := [1, 2, 3]
	mut spare_slice := spare_base[1..2]
	spare_slice << 9
	if spare_slice.len != 2 || spare_slice[0] != 2 || spare_slice[1] != 9 || spare_base[0] != 1 || spare_base[1] != 2 || spare_base[2] != 3 {
		println("wrong spare slice append")
		return
	}
	mut delete_base := [1, 2, 3, 4]
	mut delete_slice := delete_base[1..]
	delete_slice.delete(0)
	if delete_slice.len != 2 || delete_slice[0] != 3 || delete_slice[1] != 4 || delete_base[0] != 1 || delete_base[1] != 2 || delete_base[2] != 3 || delete_base[3] != 4 {
		println("wrong sliced delete")
		return
	}
	mut inserted := [1, 3]
	insert_index := 1
	inserted.insert(insert_index, 2)
	if inserted.len != 3 || inserted[0] != 1 || inserted[1] != 2 || inserted[2] != 3 {
		println("wrong insertion")
		return
	}
	mut option_handler_ran := false
	option_fallback := maybe_value(false) or {
		option_handler_ran = true
		42
	}
	option_success := maybe_value(true) or { 99 }
	mut error_message_ok := false
	result_fallback := result_value(false) or {
		error_message_ok = err.msg() == "failed"
		43
	}
	result_success := result_value(true) or { 99 }
	mut custom_error_seen := false
	custom_error_fallback := custom_error_value() or {
		if err is FastArm64CustomError {
			custom_error_seen = true
		}
		46
	}
	propagated_result_fallback := propagated_result(false) or { 44 }
	propagated_result_success := propagated_result(true) or { 99 }
	propagated_option_fallback := propagated_option(false) or { 45 }
	propagated_option_success := propagated_option(true) or { 99 }
	if !option_handler_ran || !error_message_ok || !custom_error_seen || option_fallback != 42 || option_success != 7 || result_fallback != 43 || result_success != 9 || custom_error_fallback != 46 || propagated_result_fallback != 44 || propagated_result_success != 10 || propagated_option_fallback != 45 || propagated_option_success != 9 {
		println("wrong option result handling")
		return
	}
	mut aliased := [1, 2]
	aliased << aliased
	if aliased.len != 4 || aliased[0] != 1 || aliased[1] != 2 || aliased[2] != 1 || aliased[3] != 2 {
		println("wrong aliased append")
		return
	}
	mut alias_base := [1, 2, 3]
	alias_slice := alias_base[1..]
	alias_base << alias_slice
	if alias_base.len != 5 || alias_base[0] != 1 || alias_base[1] != 2 || alias_base[2] != 3 || alias_base[3] != 2 || alias_base[4] != 3 {
		println("wrong sliced alias append")
		return
	}
	defaults := FastArm64Defaults{}
	explicit := FastArm64Defaults{retries: 7}
	if defaults.retries != 3 || !defaults.enabled || defaults.mode != .warm || explicit.retries != 7 || !explicit.enabled || explicit.mode != .warm {
		println("wrong struct defaults")
		return
	}
	mut indexed := {"b": 2, "a": 1}
	mut keys := indexed.keys()
	keys.sort()
	if keys[0] != "a" || keys[1] != "b" || indexed["a"] != 1 || indexed["b"] != 2 {
		println("wrong map keys copy")
		return
	}
	mut values := indexed.values()
	values[0] = 99
	if indexed["a"] != 1 || indexed["b"] != 2 {
		println("wrong map values copy")
		return
	}
	println("native")
}
'
		os.write_file(source_path, source) or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'native\n'
	}
}

fn test_fastc_arm64_array_index_bounds() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_bounds_${os.getpid()}')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(test_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		mut sources := []string{}
		for index in [-1, 2] {
			sources << 'fn main() {\n\tvalues := [1, 2]\n\tindex := ${index}\n\tselected := values[index]\n\tif selected == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
			sources << 'fn main() {\n\tmut values := [1, 2]\n\tindex := ${index}\n\tvalues.delete(index)\n}\n'
			sources << 'fn main() {\n\ttext := "hi"\n\tindex := ${index}\n\tselected := text[index]\n\tif selected == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		}
		sources << 'fn main() {\n\tvalues := []int{}\n\tselected := values.last()\n\tif selected == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\tmut values := []int{}\n\tselected := values.pop()\n\tif selected == 0 {\n\t\tprintln("unused")\n\t}\n}\n'
		sources << 'fn main() {\n\tmut values := []int{}\n\tvalues.delete_last()\n\tprintln("unused")\n}\n'
		for index, source in sources {
			source_path := os.join_path_single(test_dir, 'bounds_${index}.v')
			output_path := os.join_path_single(test_dir, 'bounds_${index}')
			os.write_file(source_path, source) or {
				panic(err)
			}
			generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
			result := os.execute(output_path)
			assert result.exit_code != 0
		}
	}
}

fn test_fastc_arm64_module_lifecycle_hooks() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_lifecycle_${os.getpid()}')
		dependency_dir := os.join_path_single(test_dir, 'dependency')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(dependency_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		dependency_path := os.join_path_single(dependency_dir, 'dependency.v')
		output_path := os.join_path_single(test_dir, 'app')
		os.write_file(source_path, 'module main

import dependency

fn init() {
	println("main init")
}

fn cleanup() {
	println("main cleanup")
}

fn main() {
	println("main")
}
') or { panic(err) }
		os.write_file(dependency_path, 'module dependency

fn init() {
	println("dependency init")
}

fn cleanup() {
	println("dependency cleanup")
}
') or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		generate_arm64_files([source_path], prefs, output_path) or { panic(err) }
		result := os.execute(output_path)
		assert result.exit_code == 0
		assert result.output == 'dependency init\nmain init\nmain\nmain cleanup\ndependency cleanup\n'
	}
}

fn test_fastc_arm64_rejects_imported_source_output() {
	$if arm64? {
		test_dir := os.join_path(os.temp_dir(), 'fastc_arm64_output_alias_${os.getpid()}')
		dependency_dir := os.join_path_single(test_dir, 'dependency')
		os.rmdir_all(test_dir) or {}
		os.mkdir_all(dependency_dir) or { panic(err) }
		defer {
			os.rmdir_all(test_dir) or {}
		}
		source_path := os.join_path_single(test_dir, 'main.v')
		dependency_path := os.join_path_single(dependency_dir, 'dependency.v')
		os.write_file(source_path, 'module main\n\nimport dependency\n\nfn main() {\n\tdependency.answer()\n}\n') or {
			panic(err)
		}
		dependency_source := 'module dependency\n\npub fn answer() int {\n\treturn 42\n}\n'
		os.write_file(dependency_path, dependency_source) or { panic(err) }
		mut prefs := pref.new_preferences()
		prefs.backend = 'fastc'
		prefs.user_defines = ['arm64']
		mut rejected := false
		generate_arm64_files([source_path], prefs, dependency_path) or {
			rejected = true
			assert err.msg().contains('aliases source')
		}
		assert rejected
		assert os.read_file(dependency_path) or { '' } == dependency_source
	}
}
