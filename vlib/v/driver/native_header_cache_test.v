module driver

import v.flat
import v.gen.c as cgen
import v.pref
import os
import time

fn native_inputs_of(a &flat.FlatAst, user_c_flags []string) cgen.CacheNativeInputs {
	return cgen.cache_native_inputs(a, @VEXEROOT, pref.host_target(), user_c_flags, map[string]string{},
		map[string]bool{})
}

fn test_user_native_inputs_require_a_compiler_dependency_manifest() {
	mut a := flat.FlatAst.new()
	assert native_inputs_of(&a, []string{}).user_supplied == ''
	assert native_inputs_of(&a, ['-include', 'native.h']).user_supplied == 'native.h'
	for directive in ['include', 'insert', 'preinclude', 'postinclude'] {
		mut included := flat.FlatAst.new()
		included.add_val(.file, '/project/main.v')
		included.add_node(flat.Node{ kind: .directive, value: directive, typ: '"native.h"' })
		assert native_inputs_of(&included, []string{}).user_supplied == '"native.h"'
	}
	for flags in [['-I', '/project/include'], ['-I/project/include'], ['-isystem/project/include'],
		['-include=/project/forced.h'], ['/project/native.o'], ['@/project/c_flags.rsp']] {
		assert native_inputs_of(&a, flags).user_supplied != '', flags.str()
	}
	mut flagged := flat.FlatAst.new()
	flagged.add_val(.file, '/project/main.v')
	flagged.add_node(flat.Node{ kind: .directive, value: 'flag', typ: '-I /project/include' })
	assert native_inputs_of(&flagged, []string{}).user_supplied == '/project/include'
	mut packaged := flat.FlatAst.new()
	packaged.add_val(.file, '/project/main.v')
	packaged.add_node(flat.Node{ kind: .directive, value: 'pkgconfig', typ: 'sdl2' })
	assert native_inputs_of(&packaged, []string{}).user_supplied == '#pkgconfig sdl2'
}

fn test_shipped_and_system_native_inputs_keep_the_caches() {
	helper := os.real_path(os.join_path(@VEXEROOT, 'vlib', 'v', 'flat', 'flat_payload_helpers.h'))
	mut a := flat.FlatAst.new()
	a.add_val(.file, '/project/main.v')
	a.add_node(flat.Node{ kind: .directive, value: 'include', typ: '<stddef.h>' })
	a.add_node(flat.Node{ kind: .directive, value: 'include', typ: '"@VEXEROOT/vlib/v/flat/flat_payload_helpers.h"' })
	a.add_node(flat.Node{ kind: .directive, value: 'flag', typ: '-I @VEXEROOT/thirdparty/stdatomic/nix -lm -DV_NATIVE_TEST=1' })
	// A module shipped with V may leave a header to the C compiler's search path.
	a.add_val(.file, os.join_path(@VEXEROOT, 'vlib', 'os', 'os.c.v'))
	a.add_val(.module_decl, 'os')
	a.add_node(flat.Node{ kind: .directive, value: 'include', typ: '"not_shipped_system_header.h"' })
	a.add_node(flat.Node{ kind: .directive, value: 'flag', typ: '-I/opt/native/include' })
	inputs := native_inputs_of(&a, ['-g', '-O2', '-D GC_THREADS=1', '-lm'])
	assert inputs.user_supplied == ''
	assert inputs.module_inputs['main'] == [helper]
	assert 'os' !in inputs.module_inputs
}

fn test_parallel_c_generation_leaves_native_header_ownership_to_c_compiler() {
	prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "not_present/native.h"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\nvoid main__main(void);\n'
	header, safe := v3_parallel_c_declaration_header(prefix, []string{}, @VEXEROOT)
	assert !safe
	assert header.contains('#include "not_present/native.h"')
	root := os.join_path(os.vtmp_dir(), 'parallel_user_header_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	user_header := os.join_path(root, 'native.h')
	os.write_file(user_header, 'static inline int native_value(void) { return 1; }\n')!
	user_prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "${user_header}"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\nvoid main__main(void);\n'
	_, user_safe := v3_parallel_c_declaration_header(user_prefix, []string{}, @VEXEROOT)
	assert !user_safe
}

fn test_parallel_c_generation_splits_with_headers_shipped_with_v() {
	helper := os.join_path(@VEXEROOT, 'vlib', 'v', 'flat', 'flat_payload_helpers.h')
	prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "${helper}"\n#include <stdio.h>\n/* V3CACHE_NATIVE_DIRECTIVES_END */\nvoid main__main(void);\n'
	header, safe := v3_parallel_c_declaration_header(prefix, []string{}, @VEXEROOT)
	assert safe
	assert !header.contains('#include "${helper}"')
	assert header.contains('v_flat_payload_ptr_get')
	assert header.contains('#include <stdio.h>')
}

fn test_native_dependency_list_uses_compiler_manifest_without_reading_headers() {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_manifest_${os.getpid()}_${time.now().unix_nano()}')
	os.mkdir_all(root)!
	header := os.join_path(root, 'native.h')
	nested := os.join_path(root, 'nested.h')
	os.write_file(header, '#include "nested.h"\n')!
	os.write_file(nested, '#define VALUE 3\n')!
	os.chmod(header, 0o000)!
	os.chmod(nested, 0o000)!
	defer {
		os.chmod(header, 0o644) or {}
		os.chmod(nested, 0o644) or {}
		os.rmdir_all(root) or { panic(err) }
	}
	compiler := os.join_path(root, 'cc')
	manifest := 'v3cache: ${os.quoted_path(header)} ${os.quoted_path(nested)}'
	os.write_file(compiler, '#!/bin/sh\nfor input do dependency_source="$input"; done\nprintf "%s \'%s\'\\n" ${os.quoted_path(manifest)} "$dependency_source"\n')!
	os.chmod(compiler, 0o755)!
	mut a := flat.FlatAst.new()
	a.add_val(.file, os.join_path(root, 'main.c.v'))
	a.add_node(flat.Node{ kind: .directive, value: 'include', typ: '"@DIR/native.h"' })
	prefs := pref.new_preferences()
	mut expected := [header, nested].map(os.real_path(it))
	expected.sort()
	assert native_build_input_paths(&a, prefs, []string{}, compiler) == expected
	empty := flat.FlatAst.new()
	assert native_build_input_paths(&empty, prefs, ['-include', header], compiler) == expected
}
