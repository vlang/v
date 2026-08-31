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
fn add_after_return(mut value int) {
	defer {
		value += 7
	}
	return
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
	mut inserted := [1, 3]
	insert_index := 1
	inserted.insert(insert_index, 2)
	if inserted.len != 3 || inserted[0] != 1 || inserted[1] != 2 || inserted[2] != 3 {
		println("wrong insertion")
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
