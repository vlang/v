import os

const vsh_mode_vlib_dir = os.dir(os.dir(os.dir(@FILE)))
const vsh_mode_v3_src = os.join_path(os.dir(os.dir(@FILE)), 'v.v')

fn vsh_mode_executable_path(path string) string {
	$if windows {
		return path + '.exe'
	}
	return path
}

fn build_vsh_mode_v3(root string) string {
	bin := vsh_mode_executable_path(os.join_path(root, 'v3_vsh_script_mode'))
	build := os.exec([@VEXE, '-gc', 'none', '-path', '${'${vsh_mode_vlib_dir}|@vlib|@vmodules'}',
		'-o', bin, '${vsh_mode_v3_src}'])
	assert build.exit_code == 0, build.output
	return bin
}

fn run_vsh_script(name string, source string) os.Result {
	root := os.join_path(os.vtmp_dir(), 'v3_vsh_${name}_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_vsh_mode_v3(root)
	script := os.join_path(root, '${name}.vsh')
	os.write_file(script, source) or { panic(err) }
	// `-silent` keeps the driver's benchmark report out of the script's output.
	return os.exec([v3_bin, '-gc', 'none', '-silent', '${script}'])
}

// A `.vsh` script gets `import os` implicitly, and every `os` function, generic
// function and constant is usable without the module prefix.
fn test_vsh_script_uses_os_symbols_unqualified() {
	result := run_vsh_script('script', "script_dir := join_path(temp_dir(), 'v3_vsh_script_mode_data')
mkdir_all(script_dir)!
defer {
	rmdir_all(script_dir) or {}
}
payload := join_path(script_dir, 'payload.bin')
write_file_array(payload, [u8(1), 2, 3])!
println(read_file_array[u8](payload))
println(exists(payload))
println(args.len > 0)
println(path_separator.len)
// `getenv` must resolve to the `os` wrapper, not to the `fn C.getenv` declaration
// whose bare name is also registered while `os` is compiled.
setenv('V3_VSH_SCRIPT_MODE_VALUE', 'from-env', true)
println(getenv('V3_VSH_SCRIPT_MODE_VALUE'))
")
	assert result.exit_code == 0, result.output
	assert result.output.split_into_lines() == ['[1, 2, 3]', 'true', 'true', '1', 'from-env'], result.output
}

fn test_vsh_script_can_import_local_module_without_explicit_main() {
	root := os.join_path(os.vtmp_dir(), 'v3_vsh_import_module_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_vsh_mode_v3(root)
	module_dir := os.join_path(root, 'helper')
	os.mkdir_all(module_dir) or { panic(err) }
	os.write_file(os.join_path(module_dir, 'helper.v'), "module helper

pub fn message() string {
	return 'from helper'
}

pub fn ls(path string) ![]string {
	return [path]
}

pub fn free(values []int) ![]string {
	return values.map(it.str())
}
") or { panic(err) }
	script := os.join_path(root, 'import_module.vsh')
	os.write_file(script, 'import helper { ls, free }

println(helper.message())
println(ls("custom")!.filter(it.len > 0).join(","))
println(free([2, 3])!.filter(it == "3"))
') or { panic(err) }
	result := os.exec([v3_bin, '-gc', 'none', '-silent', '${script}'])
	assert result.exit_code == 0, result.output
	assert result.output.split_into_lines() == ['from helper', 'custom', "['3']"], result.output
	shadow_script := os.join_path(root, 'shadow_import.vsh')
	os.write_file(shadow_script, 'import helper { ls }

fn ls(path string) ![]string {
	return ["local " + path]
}

println(ls("custom")!.join(","))
') or { panic(err) }
	shadow_result := os.exec([v3_bin, '-gc', 'none', '-silent', '${shadow_script}'])
	assert shadow_result.exit_code == 0, shadow_result.output
	assert shadow_result.output.trim_space() == 'local custom', shadow_result.output
}

// Script mode is a last resort: a declaration in the script itself keeps its
// meaning even when `os` exports the same name.
fn test_vsh_script_declaration_shadows_os_symbol() {
	result := run_vsh_script('shadow', "fn exists(path string) string {
	return 'local ' + path
}

println(exists('exists'))
println(os.exists(temp_dir()))
")
	assert result.exit_code == 0, result.output
	assert result.output.split_into_lines() == ['local exists', 'true'], result.output
}

fn test_vsh_script_closure_captures_preceding_top_level_local() {
	result := run_vsh_script('closure_capture', "message := 'captured'
callback := fn [message] () {
	println(message)
}
callback()
")
	assert result.exit_code == 0, result.output
	assert result.output.trim_space() == 'captured', result.output
}

// `-raw-vsh-tmp-prefix` compiles an input without the `.vsh` extension as a V script
// (e.g. from a `#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp` shebang). Without `run`,
// the executable is kept as `<prefix>.<script name>` and reused, like a `.vsh` script.
fn test_raw_vsh_tmp_prefix_runs_extensionless_script() {
	root := os.join_path(os.vtmp_dir(), 'v3 vsh raw prefix ${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_vsh_mode_v3(root)
	script := os.join_path(root, 'myscript')
	os.write_file(script, '#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp
println(file_name(executable()))
println(arguments()[1..])
') or { panic(err) }
	kept_bin := vsh_mode_executable_path(os.join_path(root, 'tmp.myscript'))
	expected_name := os.file_name(kept_bin)
	first := os.exec([v3_bin, '-silent', '-raw-vsh-tmp-prefix', 'tmp', '${script}', 'first'])
	assert first.exit_code == 0, first.output
	assert first.output.split_into_lines() == [expected_name, "['first']"], first.output
	assert os.is_file(kept_bin)
	// An unchanged script reuses the kept executable.
	cache_stamp := os.file_last_mod_unix(kept_bin) + 3600
	os.utime(kept_bin, cache_stamp, cache_stamp) or { panic(err) }
	cached := os.exec([v3_bin, '-silent', '-raw-vsh-tmp-prefix', 'tmp', '${script}', 'cached'])
	assert cached.exit_code == 0, cached.output
	assert cached.output.split_into_lines() == [expected_name, "['cached']"], cached.output
	assert os.file_last_mod_unix(kept_bin) == cache_stamp
	// A newer script is rebuilt.
	os.write_file(script, "println('rebuilt')
") or { panic(err) }
	os.utime(script, cache_stamp + 60, cache_stamp + 60) or { panic(err) }
	rebuilt := os.exec([v3_bin, '-silent', '-raw-vsh-tmp-prefix', 'tmp', '${script}'])
	assert rebuilt.exit_code == 0, rebuilt.output
	assert rebuilt.output == 'rebuilt\n', rebuilt.output
	assert os.file_last_mod_unix(kept_bin) != cache_stamp
	os.write_file(script, '#!/usr/bin/env -S v -raw-vsh-tmp-prefix tmp run
println(file_name(executable()))
println(arguments()[1..])
') or { panic(err) }
	os.rm(kept_bin) or { panic(err) }
	result := os.exec([v3_bin, '-silent', '-raw-vsh-tmp-prefix', 'tmp', 'run', '${script}', 'third'])
	assert result.exit_code == 0, result.output
	assert result.output.split_into_lines() == [expected_name, "['third']"], result.output
	assert !os.exists(kept_bin)
	listed := os.exec([v3_bin, '-silent', '-print-v-files', '-raw-vsh-tmp-prefix', 'tmp', '${script}'])
	assert listed.exit_code == 0, listed.output
	assert os.real_path(script) in listed.output.split_into_lines(), listed.output
	missing := os.exec([v3_bin, '-raw-vsh-tmp-prefix'])
	assert missing.exit_code != 0
	assert missing.output.contains('option `-raw-vsh-tmp-prefix` requires a value'), missing.output
}

// Only the extensionless input is a script. Its imported ordinary V files still
// need to import `os` and qualify its symbols.
fn test_raw_vsh_imported_v_file_does_not_get_implicit_os_symbols() {
	root := os.join_path(os.vtmp_dir(), 'v3 vsh ordinary module ${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	v3_bin := build_vsh_mode_v3(root)
	module_dir := os.join_path(root, 'helper')
	os.mkdir_all(module_dir) or { panic(err) }
	ordinary_file := os.join_path(module_dir, 'helper.v')
	os.write_file(ordinary_file, "module helper

pub fn message() string {
	return file_name('ordinary.v')
}
") or { panic(err) }
	script := os.join_path(root, 'myscript')
	os.write_file(script, 'import helper\n\nprintln(helper.message())\n') or { panic(err) }
	result := os.exec([v3_bin, '-silent', '-raw-vsh-tmp-prefix', 'tmp', '${script}'])
	assert result.exit_code != 0, result.output
	assert result.output.contains('unknown function: file_name'), result.output
	assert result.output.contains(ordinary_file) || result.output.contains(os.real_path(ordinary_file)), result.output
}
