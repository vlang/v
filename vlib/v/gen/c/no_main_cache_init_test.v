module c

import os
import v.flat
import v.types

fn test_exported_script_preserves_process_arguments() {
	root := os.join_path(os.vtmp_dir(), 'exported_script_args_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	script := os.join_path(root, 'script.v')
	executable := os.join_path(root, 'script')
	os.write_file(script, "import os\n
@[export: 'script_callback']
fn script_callback() int { return os.args.len }
assert os.args.len == 3
assert script_callback() == 3
assert os.args[1..] == ['alpha', 'beta']
println(os.args[1..].join(','))\n")!
	compiled := os.exec([@VEXE, '-new-compiler', '-no-retry-compilation', '-nocache', '-cc',
		@CCOMPILER, '-o', executable, script])
	assert compiled.exit_code == 0, compiled.output
	run := os.exec([executable, 'alpha', 'beta'])
	assert run.exit_code == 0, run.output
	assert run.output.trim_space() == 'alpha,beta'
}

fn test_no_main_initializer_linkage_matches_module_split() {
	for split in [false, true] {
		mut a := flat.FlatAst.new()
		a.export_fn_names['builtin.probe'] = 'probe'
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.set_cache_split(split)
		g.set_suppress_main(true)
		g.forward_decls()
		g.gen_no_main_runtime_init_caller()
		source := g.sb.str()
		storage := if split { '' } else { 'static ' }
		assert source.contains('${storage}void _vno_main_init_caller(void);')
		assert source.contains('${storage}void _vno_main_init_caller(void) {')
		assert source.contains('static bool _v3_no_main_initialized = false;')
		if split {
			assert !source.contains('static void _vno_main_init_caller')
		}
	}
}

fn test_cached_no_main_initializer_links_across_export_units() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'no_main_cache_init_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	mut a := flat.FlatAst.new()
	a.export_fn_names['builtin.probe'] = 'probe'
	mut tc := types.TypeChecker.new(&a)
	mut g := FlatGen.new()
	g.a = &a
	g.tc = &tc
	g.set_cache_split(true)
	g.set_suppress_main(true)
	g.forward_decls()
	g.gen_no_main_runtime_init_caller()
	source := g.sb.str()
	marker := source.index('/* V3CACHE_MODULE main */') or { panic('missing main unit') }
	declarations := '#include <stdbool.h>\n' + source[..marker]
	export_file := os.join_path(root, 'export.c')
	main_file := os.join_path(root, 'main.c')
	os.write_file(export_file, declarations + 'int probe(void) { _vno_main_init_caller(); return 42; }\n')!
	os.write_file(main_file, declarations + source[marker..] +
		'int probe(void);\nint main(void) { return probe() == 42 ? 0 : 1; }\n')!
	executable := os.join_path(root, 'probe')
	result := os.exec(['cc', '-o', executable, export_file, main_file])
	assert result.exit_code == 0, result.output
	run := os.exec([executable])
	assert run.exit_code == 0, run.output
}

fn test_cached_initializer_waits_for_arguments_only_in_executables() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'cached_init_readiness_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	for mode in ['executable', 'object', 'suppressed', 'shared', 'no_main', 'postinclude'] {
		mut a := flat.FlatAst.new()
		if mode == 'no_main' {
			a.add_node(flat.Node{
				kind:  .module_decl
				value: 'no_main'
			})
		}
		mut tc := types.TypeChecker.new(&a)
		mut g := FlatGen.new()
		g.a = &a
		g.tc = &tc
		g.has_builtins = true
		g.set_cache_split(true)
		g.set_object_file_mode(mode == 'object')
		g.set_suppress_main(mode == 'suppressed')
		g.set_shared(mode == 'shared')
		if mode == 'postinclude' {
			g.postinclude_directives << '#include "native_main.h"'
		}
		// Request runtime initialization; the C fixture supplies its counter.
		g.module_init_fns << 'probe_init'
		g.gen_no_main_runtime_init_caller()
		params := if mode == 'shared' { 'int argc, char** argv' } else { 'void' }
		before_main := if mode == 'executable' { 0 } else { 1 }
		source := '#include <stdbool.h>
static void* g_main_argv;
static int init_calls;
static void _vinit(${params}) { init_calls++; }
' + g.sb.str() + '
int main(void) {
	_vno_main_init_caller();
	if (init_calls != ${before_main}) { return 1; }
	char* argv[] = {"probe", 0};
	g_main_argv = argv;
	_vno_main_init_caller();
	_vno_main_init_caller();
	return init_calls == 1 ? 0 : 2;
}
'
		c_file := os.join_path(root, '${mode}.c')
		executable := os.join_path(root, mode)
		os.write_file(c_file, source)!
		compiled := os.exec(['cc', '-o', executable, c_file])
		assert compiled.exit_code == 0, '${mode}: ${compiled.output}'
		run := os.exec([executable])
		assert run.exit_code == 0, '${mode}: ${run.output}'
	}
}
