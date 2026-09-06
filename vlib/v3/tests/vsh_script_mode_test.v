import os

const vsh_mode_vlib_dir = os.dir(os.dir(os.dir(@FILE)))
const vsh_mode_v3_src = os.join_path(os.dir(os.dir(@FILE)), 'v3.v')

fn build_vsh_mode_v3(root string) string {
	bin := os.join_path(root, 'v3_vsh_script_mode')
	build := os.execute('${@VEXE} -gc none -path "${vsh_mode_vlib_dir}|@vlib|@vmodules" -o ${bin} ${vsh_mode_v3_src}')
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
	return os.execute('${v3_bin} -silent ${script}')
}

// A `.vsh` script gets `import os` implicitly, and every `os` function, generic
// function and constant is usable without the module prefix.
fn test_vsh_script_uses_os_symbols_unqualified() {
	result := run_vsh_script('script', "dir := join_path(temp_dir(), 'v3_vsh_script_mode_data')
mkdir_all(dir)!
defer {
	rmdir_all(dir) or {}
}
payload := join_path(dir, 'payload.bin')
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
