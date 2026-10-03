module driver

import v.flat
import v.pref
import os
import time

fn test_native_cache_requires_a_compiler_dependency_manifest() {
	mut a := flat.FlatAst.new()
	assert !ast_has_external_c_inputs(&a, []string{})
	assert ast_has_external_c_inputs(&a, ['-include', 'native.h'])
	for directive in ['include', 'insert', 'preinclude', 'postinclude'] {
		mut included := flat.FlatAst.new()
		included.add_node(flat.Node{ kind: .directive, value: directive, typ: '"native.h"' })
		assert ast_has_external_c_inputs(&included, []string{})
	}
}

fn test_parallel_c_generation_leaves_native_header_ownership_to_c_compiler() {
	prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "not_present/native.h"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\nvoid main__main(void);\n'
	header, safe := v3_parallel_c_declaration_header(prefix, []string{})
	assert !safe
	assert header.contains('#include "not_present/native.h"')
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
