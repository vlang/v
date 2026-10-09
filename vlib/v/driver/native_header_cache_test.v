module driver

import v.flat
import v.cmdexec
import v.gen.c as cgen
import v.modulecache
import v.pref
import os
import time
import crypto.sha256

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

fn test_parallel_c_generation_keeps_segfault_handler_state_in_owner() ! {
	$if bsd || linux {
		helper := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'segfault_handler_nix.h')
		prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "${helper}"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\n'
		header, safe := v3_parallel_c_declaration_header(prefix, []string{}, @VEXEROOT)
		assert safe
		assert !header.contains('#include "${helper}"')
		root := os.join_path(os.vtmp_dir(), 'parallel_segfault_header_${os.getpid()}_${time.now().unix_nano()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		declarations := os.join_path(root, 'declarations.h')
		linkage_cleanup := '\n#if defined(V_SEGFAULT_HANDLER_INSTALL_LINKAGE) || defined(V_SEGFAULT_HANDLER_SCOPE)\n#error leaked installer linkage macro\n#endif\n'
		os.write_file(declarations, header + linkage_cleanup)!
		runtime_header := os.join_path(root, 'runtime.h')
		os.write_file(runtime_header, '#include "${helper}"\n' + linkage_cleanup)!
		// Preprocess the actual split header: a prototype is sufficient in body units,
		// while the saved signal actions and every handler helper belong to the owner.
		non_owner := cmdexec.run('cc', ['-E', '-P', '-x', 'c', '-DV_PARALLEL_CC=1', declarations])
		assert non_owner.exit_code == 0, non_owner.output
		assert non_owner.output.contains('void v_install_segfault_handler(void* fallback, void* main_argv);'), non_owner.output
		assert !non_owner.output.contains('void v_install_segfault_handler(void* fallback, void* main_argv) {'), non_owner.output
		for name in ['v_segfault_fallback', 'v_segfault_previous', 'v_segfault_previous_consumed',
			'v_segfault_signal_handler'] {
			assert !non_owner.output.contains(name), non_owner.output
		}
		owner := cmdexec.run('cc', ['-E', '-P', '-x', 'c', '-DV_PARALLEL_CC=1',
			'-DV_PARALLEL_CC_OUT_0=1', runtime_header])
		assert owner.exit_code == 0, owner.output
		assert owner.output.contains('void v_install_segfault_handler(void* fallback, void* main_argv) {'), owner.output
		assert !owner.output.contains('static void v_install_segfault_handler('), owner.output
		for name in ['v_segfault_fallback', 'v_segfault_previous', 'v_segfault_previous_consumed'] {
			assert owner.output.contains(name), owner.output
		}
		// Monolithic builds retain the existing internal linkage.
		monolithic := cmdexec.run('cc', ['-E', '-P', '-x', 'c', runtime_header])
		assert monolithic.exit_code == 0, monolithic.output
		assert monolithic.output.contains('static void v_install_segfault_handler('), monolithic.output
	}
}

fn test_parallel_c_generation_splits_with_the_segfault_handler() {
	helper := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'segfault_handler_nix.h')
	prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "${helper}"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\n'
	header, safe := v3_parallel_c_declaration_header(prefix, []string{}, @VEXEROOT)
	assert safe
	assert !header.contains('#include "${helper}"')
	assert header.contains('void v_install_segfault_handler(void* fallback, void* main_argv);')
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

fn test_native_input_closure_tracks_nested_shipped_headers() {
	vroot := os.join_path(os.vtmp_dir(), 'native_closure_${os.getpid()}_${time.now().unix_nano()}')
	scratch := os.join_path(vroot, 'vlib', 'scratch')
	os.mkdir_all(scratch)!
	defer {
		os.rmdir_all(vroot) or {}
	}
	outer := os.join_path(scratch, 'outer.h')
	inner := os.join_path(scratch, 'inner.h')
	source := os.join_path(scratch, 'impl.c')
	os.write_file(outer, '#include <stddef.h>\n#include "inner.h"\nstatic inline int outer_value(void) { return inner_value(); }\n')!
	os.write_file(inner, '#pragma once\nstatic inline int inner_value(void) { return 101; }\n')!
	os.write_file(source, '#include "inner.h"\nint impl_value(void) { return inner_value(); }\n')!
	real_outer := os.real_path(outer)
	real_inner := os.real_path(inner)
	mut inputs := cgen.CacheNativeInputs{
		module_inputs: {
			'scratch': [real_outer]
		}
		native_paths:  {
			real_outer: true
		}
	}
	closure := v3_native_input_closure(&inputs, vroot, true, '')
	// An edit to the nested header has to reach the crun identity and the caches.
	assert closure.inputs['scratch'] == [real_inner, real_outer].sorted()
	assert closure.unassignable == ''
	// C allows blanks around `#` and `include`, as in libgc's
	// `#  include "gc_pthread_redirects.h"`.
	for directive in ['# include', '#\tinclude', '#  include', '  #include'] {
		os.write_file(outer, '${directive} "inner.h"\nstatic inline int outer_value(void) { return inner_value(); }\n')!
		spaced := v3_native_input_closure(&inputs, vroot, true, '')
		assert spaced.inputs['scratch'] == [real_inner, real_outer].sorted(), directive
		assert spaced.unassignable == '', directive
		os.write_file(outer, '${directive} "missing.h"\n')!
		assert v3_native_input_closure(&inputs, vroot, true, '').unassignable == real_outer, directive
	}
	os.write_file(outer, '#include <stddef.h>\n#include "inner.h"\nstatic inline int outer_value(void) { return inner_value(); }\n')!
	// A native source defines symbols that every cached object would duplicate.
	real_source := os.real_path(source)
	inputs.module_inputs['scratch'] = [real_source]
	inputs.native_paths[real_source] = true
	with_source := v3_native_input_closure(&inputs, vroot, true, '')
	assert with_source.unassignable == real_source
	assert real_inner in with_source.inputs['scratch']
	// crun only needs the closure, not the replication check.
	assert v3_native_input_closure(&inputs, vroot, false, '').unassignable == ''
	inputs.module_inputs['scratch'] = [real_outer]
	inputs.implementation_define = 'STB_IMAGE_IMPLEMENTATION'
	assert v3_native_input_closure(&inputs, vroot, true, '').unassignable == '#define STB_IMAGE_IMPLEMENTATION'
}

fn test_native_input_closure_is_recorded_until_a_header_changes() ! {
	vroot := os.join_path(os.vtmp_dir(), 'v3_native_closure_record_${os.getpid()}')
	os.rmdir_all(vroot) or {}
	// Only the headers that V ships are followed: those below `vlib`, among others.
	scratch := os.join_path(vroot, 'vlib', 'scratch')
	os.mkdir_all(scratch)!
	defer {
		os.rmdir_all(vroot) or {}
	}
	records := os.join_path(vroot, 'records')
	outer := os.join_path(scratch, 'outer.h')
	inner := os.join_path(scratch, 'inner.h')
	os.write_file(outer, '#include "inner.h"\nstatic inline int outer_value(void) { return inner_value(); }\n')!
	os.write_file(inner, '#pragma once\nstatic inline int inner_value(void) { return 101; }\n')!
	real_outer := os.real_path(outer)
	real_inner := os.real_path(inner)
	inputs := cgen.CacheNativeInputs{
		module_inputs: {
			'scratch': [real_outer]
		}
		native_paths:  {
			real_outer: true
		}
	}
	first := v3_native_input_closure(&inputs, vroot, true, records)
	assert first.inputs['scratch'] == [real_inner, real_outer].sorted()
	assert first.unassignable == ''
	if modulecache.file_metadata_signature(real_inner) == '' {
		// This file system cannot tell an edit by a file's metadata yet, so the
		// expansion is not recorded.
		return
	}
	assert (os.ls(records) or { []string{} }).len == 1
	// The record answers for the files as they were.
	second := v3_native_input_closure(&inputs, vroot, true, records)
	assert second.inputs['scratch'] == first.inputs['scratch']
	assert second.unassignable == ''
	// A header that now defines storage is not replicable any more, and the record
	// of its former text must not say otherwise.
	os.write_file(inner, '#pragma once\nint inner_counter = 0;\nstatic inline int inner_value(void) { return inner_counter; }\n')!
	changed := v3_native_input_closure(&inputs, vroot, true, records)
	assert changed.unassignable == real_outer
	// Without a record directory the answer is the same, and nothing is written.
	recorded := (os.ls(records) or { []string{} }).len
	assert v3_native_input_closure(&inputs, vroot, true, '').unassignable == real_outer
	assert (os.ls(records) or { []string{} }).len == recorded
}

// An include directive names the first file that its lookup finds. A file that
// appears earlier in that lookup changes what the header includes, without a
// change to the header or to the file that it included until then.
fn test_recorded_native_input_closure_follows_a_new_include_candidate() ! {
	vroot := os.join_path(os.vtmp_dir(), 'v3_native_closure_candidates_${os.getpid()}')
	os.rmdir_all(vroot) or {}
	scratch := os.join_path(vroot, 'vlib', 'scratch')
	shared := os.join_path(vroot, 'vlib', 'shared')
	os.mkdir_all(scratch)!
	os.mkdir_all(shared)!
	defer {
		os.rmdir_all(vroot) or {}
	}
	records := os.join_path(vroot, 'records')
	outer := os.join_path(scratch, 'outer.h')
	shared_inner := os.join_path(shared, 'inner.h')
	os.write_file(outer, '#include <stddef.h>\n#include "inner.h"\nstatic inline int outer_value(void) { return inner_value(); }\n')!
	os.write_file(shared_inner, '#pragma once\nstatic inline int inner_value(void) { return 101; }\n')!
	real_outer := os.real_path(outer)
	real_shared_inner := os.real_path(shared_inner)
	inputs := cgen.CacheNativeInputs{
		module_inputs: {
			'scratch': [real_outer]
		}
		native_paths:  {
			real_outer: true
		}
		include_dirs:  [os.real_path(shared)]
	}
	first := v3_native_input_closure(&inputs, vroot, true, records)
	assert first.inputs['scratch'] == [real_outer, real_shared_inner].sorted()
	assert first.unassignable == ''
	if modulecache.file_metadata_signature(real_shared_inner) == '' {
		return
	}
	assert (os.ls(records) or { []string{} }).len == 1
	assert v3_native_input_closure(&inputs, vroot, true, records).inputs['scratch'] == first.inputs['scratch']
	// The same name beside the including header wins the lookup of a quoted
	// include, and this one keeps storage, so it cannot be replicated.
	beside_inner := os.join_path(scratch, 'inner.h')
	os.write_file(beside_inner, '#pragma once\nint inner_counter = 0;\nstatic inline int inner_value(void) { return inner_counter; }\n')!
	real_beside_inner := os.real_path(beside_inner)
	changed := v3_native_input_closure(&inputs, vroot, true, records)
	assert changed.inputs['scratch'] == [real_beside_inner, real_outer].sorted()
	assert changed.unassignable == real_outer
	// A system header that the include directories did not hold is one that the
	// C compiler finds; once V ships a file of that name there, it is expanded.
	os.rm(beside_inner)!
	assert v3_native_input_closure(&inputs, vroot, true, records).inputs['scratch'] == first.inputs['scratch']
	shipped_stddef := os.join_path(shared, 'stddef.h')
	os.write_file(shipped_stddef, '#pragma once\ntypedef unsigned long size_t;\n')!
	with_stddef := v3_native_input_closure(&inputs, vroot, true, records)
	assert os.real_path(shipped_stddef) in with_stddef.inputs['scratch']
}

// An include directive can find a symbolic link. Pointing the link at another
// header changes what is included, while the header it pointed at until then is
// still there, unchanged.
fn test_recorded_native_input_closure_follows_a_retargeted_symbolic_link() ! {
	$if windows {
		return
	}
	vroot := os.join_path(os.vtmp_dir(), 'v3_native_closure_links_${os.getpid()}')
	os.rmdir_all(vroot) or {}
	scratch := os.join_path(vroot, 'vlib', 'scratch')
	shared := os.join_path(vroot, 'vlib', 'shared')
	outside := os.join_path(vroot, 'outside')
	os.mkdir_all(scratch)!
	os.mkdir_all(shared)!
	os.mkdir_all(outside)!
	defer {
		os.rmdir_all(vroot) or {}
	}
	records := os.join_path(vroot, 'records')
	outer := os.join_path(scratch, 'outer.h')
	first := os.join_path(shared, 'first.h')
	second := os.join_path(shared, 'second.h')
	link := os.join_path(scratch, 'inner.h')
	os.write_file(outer, '#include "inner.h"\n#include <extra.h>\nstatic inline int outer_value(void) { return inner_value(); }\n')!
	os.write_file(first, '#pragma once\nstatic inline int inner_value(void) { return 101; }\n')!
	os.write_file(second, '#pragma once\nstatic inline int inner_value(void) { return 202; }\n')!
	os.symlink(first, link)!
	// A header outside of what V ships is left to the C compiler.
	extra := os.join_path(outside, 'extra.h')
	os.write_file(extra, '#pragma once\n')!
	real_outer := os.real_path(outer)
	real_first := os.real_path(first)
	real_second := os.real_path(second)
	inputs := cgen.CacheNativeInputs{
		module_inputs: {
			'scratch': [real_outer]
		}
		native_paths:  {
			real_outer: true
		}
		include_dirs:  [os.real_path(outside)]
	}
	before := v3_native_input_closure(&inputs, vroot, true, records)
	assert before.inputs['scratch'] == [real_first, real_outer].sorted()
	if modulecache.file_metadata_signature(real_first) == '' {
		return
	}
	assert (os.ls(records) or { []string{} }).len == 1
	assert v3_native_input_closure(&inputs, vroot, true, records).inputs['scratch'] == before.inputs['scratch']
	os.rm(link)!
	os.symlink(second, link)!
	retargeted := v3_native_input_closure(&inputs, vroot, true, records)
	assert retargeted.inputs['scratch'] == [real_outer, real_second].sorted()
	assert v3_native_input_closure(&inputs, vroot, true, records).inputs['scratch'] == retargeted.inputs['scratch']
	// The header that the C compiler was left with becomes a link to one that V
	// ships: it is expanded from now on.
	os.rm(extra)!
	os.symlink(first, extra)!
	shipped := v3_native_input_closure(&inputs, vroot, true, records)
	assert shipped.inputs['scratch'] == [real_first, real_outer, real_second].sorted()
}

fn test_cached_native_owner_is_limited_to_the_builtin_signal_header() ! {
	header := os.real_path(os.join_path(@VEXEROOT, 'vlib', 'builtin', 'segfault_handler_nix.h'))
	source := os.read_file(header)!
	assert !modulecache.c_source_is_replicable(source)
	assert v3_cache_native_input_has_program_owner(header, source, @VEXEROOT)
	// The shipped header also supports the equivalent owner-first layout.
	owner_first := '#ifndef V_SEGFAULT_HANDLER_NIX_H\n#define V_SEGFAULT_HANDLER_NIX_H\n#define V_PARALLEL_CC_STATIC_STORAGE_HANDLED 1\n#if !defined(V_PARALLEL_CC) || defined(V_PARALLEL_CC_OUT_0)\nstatic int signal_state;\n#else\nvoid v_install_segfault_handler(void* fallback, void* main_argv);\n#endif\n#endif\n'
	assert v3_cache_native_input_has_program_owner(header, owner_first, @VEXEROOT)
	for protocol in [source, owner_first] {
		assert !v3_cache_native_input_has_program_owner(header, protocol.replace('V_PARALLEL_CC_STATIC_STORAGE_HANDLED',
			'UNRECOGNIZED_STORAGE'), @VEXEROOT)
		assert !v3_cache_native_input_has_program_owner(header, protocol.replace('v_install_segfault_handler',
			'another_installer'), @VEXEROOT)
		assert !v3_cache_native_input_has_program_owner(os.join_path(@VEXEROOT, 'vlib', 'scratch',
			'other.h'), protocol, @VEXEROOT)
	}
	inputs := cgen.CacheNativeInputs{
		module_inputs: {
			'builtin': [header]
		}
		native_paths:  {
			header: true
		}
	}
	closure := v3_native_input_closure(&inputs, @VEXEROOT, true, '')
	assert closure.unassignable == ''
	assert header in closure.inputs['builtin']
	mut state := V3ModuleCacheState{}
	assert prepare_v3_cache_external_inputs(mut state, &inputs, &closure)
	assert state.external_input_digests[header] == sha256.hexhash(source)
	// Other shipped headers cannot claim an owner using the same marker/protocol.
	root := os.join_path(os.vtmp_dir(), 'cached_native_owner_${os.getpid()}_${time.now().unix_nano()}')
	other := os.join_path(root, 'vlib', 'scratch', 'other.h')
	os.mkdir_all(os.dir(other))!
	defer {
		os.rmdir_all(root) or {}
	}
	os.write_file(other, owner_first)!
	real_other := os.real_path(other)
	other_inputs := cgen.CacheNativeInputs{
		module_inputs: {
			'scratch': [real_other]
		}
		native_paths:  {
			real_other: true
		}
	}
	assert v3_native_input_closure(&other_inputs, root, true, '').unassignable == real_other
}

fn test_cached_signal_header_has_one_owner_with_shared_program_declarations() ! {
	$if bsd || linux {
		header := os.join_path(@VEXEROOT, 'vlib', 'builtin', 'segfault_handler_nix.h')
		prefix := '/* V3CACHE_NATIVE_DIRECTIVES_BEGIN */\n#include "${header}"\n/* V3CACHE_NATIVE_DIRECTIVES_END */\n'
		owner_source := v3_cached_c_unit_source(prefix, true)
		declarations := v3_cached_c_unit_source(modulecache.declaration_header(prefix), false)
		body := 'void (*cached_installer(void))(void*, void*);\nint main(void) { return cached_installer() != v_install_segfault_handler; }\n'
		tcc_declarations := tcc_cached_main_source(declarations, body)
		root := os.join_path(os.vtmp_dir(), 'cached_signal_header_${os.getpid()}_${time.now().unix_nano()}')
		os.mkdir_all(root)!
		defer {
			os.rmdir_all(root) or {}
		}
		owner_path := os.join_path(root, 'owner.c')
		cached_path := os.join_path(root, 'cached.c')
		declarations_path := os.join_path(root, 'tcc_declarations.h')
		body_path := os.join_path(root, 'body.c')
		incremental_path := os.join_path(root, 'incremental.c')
		os.write_file(owner_path, owner_source)!
		os.write_file(cached_path, v3_cached_c_unit_source(prefix + 'void (*cached_installer(void))(void*, void*) { return v_install_segfault_handler; }\n',
			false))!
		os.write_file(declarations_path, tcc_declarations)!
		os.write_file(body_path, body)!
		os.write_file(incremental_path, v3_incremental_main_source(declarations_path, body_path))!
		// A cached combined source may later be split for parallel compilation.
		// The owner prelude copied into its declaration header cannot promote a body unit.
		parallel_header, safe := v3_parallel_c_declaration_header(owner_source, []string{},
			@VEXEROOT)
		assert safe
		parallel_header_path := os.join_path(root, 'parallel.h')
		parallel_path := os.join_path(root, 'parallel.c')
		os.write_file(parallel_header_path, parallel_header)!
		os.write_file(parallel_path, v3_parallel_c_unit_source(parallel_header_path, body,
			false, false))!
		for path in [cached_path, declarations_path, incremental_path, parallel_path] {
			// An inherited owner flag must not turn cached declarations into definitions.
			flags := if path == parallel_path { []string{} } else { ['-DV_PARALLEL_CC_OUT_0=1'] }
			preprocessed := cmdexec.run('cc', ['-E', '-P', '-x', 'c', ...flags, path])
			assert preprocessed.exit_code == 0, preprocessed.output
			assert preprocessed.output.contains('void v_install_segfault_handler('), preprocessed.output
			for name in ['v_segfault_fallback', 'v_segfault_previous', 'v_segfault_previous_consumed',
				'v_segfault_signal_handler'] {
				assert !preprocessed.output.contains(name), '${path}: ${preprocessed.output}'
			}
			assert !preprocessed.output.contains('void v_install_segfault_handler(void* fallback, void* main_argv) {'), preprocessed.output
		}
		owner := cmdexec.run('cc', ['-E', '-P', '-x', 'c', owner_path])
		assert owner.exit_code == 0, owner.output
		assert owner.output.contains('void v_install_segfault_handler(void* fallback, void* main_argv) {'), owner.output
		assert !owner.output.contains('static void v_install_segfault_handler('), owner.output
		// Cached modules and incremental/TCC-style bodies resolve the same installer
		// from the physical program-prefix owner, without a second definition.
		binary := os.join_path(root, 'linked')
		linked := cmdexec.run('cc', ['-o', binary, owner_path, cached_path, incremental_path,
			...v3_default_linker_flags(pref.host_target().os, false)])
		assert linked.exit_code == 0, linked.output
		assert cmdexec.run(binary, []string{}).exit_code == 0
	}
}

fn test_only_declaration_headers_are_replicated_into_cached_objects() {
	assert modulecache.c_source_is_replicable('#include <stdio.h>\ntypedef struct { int x; } Foo;\nint foo_get(Foo *f);\nextern int foo_count;\nstatic inline int foo_one(void) { return 1; }\n')
	// A macro such as `GC_API` supplies the `extern` of a plain declaration, and
	// the linker merges weak definitions.
	assert modulecache.c_source_is_replicable('GC_API int GC_count;\n__attribute__ ((weak)) GC_API void GC_noop(void *p) { (void)p; }\n')
	assert !modulecache.c_source_is_replicable('int foo_get(void) { return 1; }\n')
	assert !modulecache.c_source_is_replicable('int foo_count = 0;\n')
	assert !modulecache.c_source_is_replicable('static int foo_count;\n')
	assert !modulecache.c_source_is_replicable('static inline int foo_next(void) {\n\tstatic int n;\n\treturn ++n;\n}\n')
	// Each cached object would get its own copy of a file-scope static object,
	// whatever its initializer or declarator looks like.
	assert !modulecache.c_source_is_replicable('static uint64_t foo_count = UINT64_C(0);\n')
	assert !modulecache.c_source_is_replicable('static int foo_count = FOO(1);\n')
	assert !modulecache.c_source_is_replicable('static int foo_size = sizeof(int);\n')
	assert !modulecache.c_source_is_replicable('static void (*foo_callback)(void);\n')
	assert !modulecache.c_source_is_replicable('static void (*foo_callback)(void) = 0;\n')
	assert !modulecache.c_source_is_replicable('static char foo_buf[sizeof(int)];\n')
	assert !modulecache.c_source_is_replicable('static int foo_counters[FOO(1)];\n')
	assert modulecache.c_source_is_replicable('static int foo_helper(int x);\n')
	assert modulecache.c_source_is_replicable('static int foo_helper(int a[sizeof(int)]);\n')
}
