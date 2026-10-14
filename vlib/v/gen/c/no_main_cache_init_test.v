module c

import os
import v.flat
import v.types

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
