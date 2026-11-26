// vtest build: !musl? && !sanitized_job?
import os

fn draw_fns_api_child_flags() []string {
	$if linux_wayland_session ? {
		$if sokol_wayland ? {
			return ['-d', 'sokol_wayland']
		}
	}
	return []
}

// draw_fns_api_module_source turns the sample program `source` into the module
// `name`, whose `pub fn run()` holds what the program did: its `main`, or the
// statements of a script. That lets all samples share one build.
fn draw_fns_api_module_source(name string, source string) !string {
	mut lines := source.split_into_lines().filter(it != 'module main')
	if lines.any(it == 'fn main() {') {
		lines = lines.map(if it == 'fn main() {' { 'pub fn run() {' } else { it })
		return 'module ${name}\n\n' + lines.join('\n') + '\n'
	}
	// A script: imports and declarations come first, then the statements.
	declaration_prefixes := ['import ', 'const ', 'fn ', 'struct ', '//']
	mut start := -1
	for i, line in lines {
		if line != '' && !line.starts_with('\t') && line != '}'
			&& !declaration_prefixes.any(line.starts_with(it)) {
			start = i
			break
		}
	}
	if start < 0 {
		return error('no main function or script statements')
	}
	for line in lines[start..] {
		if declaration_prefixes.any(line.starts_with(it)) {
			return error('declarations after the first script statement are not supported')
		}
	}
	return 'module ${name}\n\n' + lines[..start].join('\n') + '\npub fn run() {\n' +
		lines[start..].join('\n') + '\n}\n'
}

fn test_all_samples_can_be_compiled() {
	vexe := @VEXE
	vroot := os.dir(vexe)
	root := os.join_path(os.vtmp_dir(), 'draw_fns_api_${os.getpid()}')
	os.rmdir_all(root) or {}
	os.mkdir_all(root) or { panic(err) }
	cache_dir := os.join_path(root, 'vcache')
	old_vcache := os.getenv_opt('VCACHE')
	os.setenv('VCACHE', cache_dir, true)
	defer {
		if vcache := old_vcache {
			os.setenv('VCACHE', vcache, true)
		} else {
			os.unsetenv('VCACHE')
		}
		os.rmdir_all(root) or {}
	}
	mut samples := os.walk_ext('${vroot}/vlib/gg/testdata', '.vv')
	samples.sort()
	// Samples that set C flags would change the build of the others, so they
	// keep their own program. The rest are modules of one program, whose
	// `main` calls every `run` so that none of their code is pruned.
	mut programs := []string{}
	mut imports := []string{}
	mut calls := []string{}
	for sample in samples {
		source := os.read_file(sample) or { panic(err) }
		if source.split_into_lines().any(it.starts_with('#')) {
			programs << sample
			continue
		}
		name := os.file_name(sample).all_before_last('.')
		module_source := draw_fns_api_module_source(name, source) or {
			assert false, '${sample}: ${err}'
			return
		}
		os.mkdir_all(os.join_path(root, name)) or { panic(err) }
		os.write_file(os.join_path(root, name, '${name}.v'), module_source) or { panic(err) }
		imports << 'import ${name}'
		calls << '\t\t${name}.run()'
	}
	assert calls.len > 0
	combined := os.join_path(root, 'samples.v')
	os.write_file(combined, imports.join('\n') + '\nimport os\n\nfn main() {\n' +
		'\t// only compiled, never run\n\tif os.args.len > 1 {\n' + calls.join('\n') + '\n\t}\n}\n') or {
		panic(err)
	}
	programs << combined
	mut fails := []string{}
	for program_source in programs {
		output := os.join_path(root, os.file_name(program_source).all_before_last('.'))
		args := [vexe, ...draw_fns_api_child_flags(), '-o', output, program_source]
		res := os.exec(args)
		if res.exit_code != 0 {
			eprintln('>>> FAIL ${args.join(' ')}')
			eprintln(res.output)
			fails << args.join(' ')
		}
		println('OK ${args.join(' ')}')
	}
	if fails.len > 0 {
		eprintln('> Failed summary:')
		for f in fails {
			eprintln('   failed cmd: ${f}')
		}
		assert false
	}
}
