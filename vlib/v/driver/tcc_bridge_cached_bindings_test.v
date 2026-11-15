module driver

import os
import v.markused
import v.modulecache
import v.parser
import v.pref
import v.types

fn test_native_bridge_cached_headers_keep_called_c_bindings() ! {
	$if !linux && !macos {
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'native_bridge_cached_bindings_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	main := os.join_path(root, 'main.v')
	source := os.join_path(root, 'bridge.c.v')
	header := os.join_path(root, 'bridge.vh')
	native := os.join_path(root, 'native.h')
	os.write_file(native, 'static int native_answer(void) { return 42; }\n')!
	os.write_file(main, 'module main\nimport bridgecase\nfn main() { _ = bridgecase.answer() }\n')!
	os.write_file(source, 'module bridgecase\n#include "${native}"\nfn C.native_answer() i32\nfn C.unavailable_unused() i32\npub fn answer() i32 { return C.native_answer() }\n')!
	consumer := os.join_path(root, 'consumer.c')
	os.write_file(consumer, 'extern int (*v3_native_bridge_native_answer)(void);\nint main(void) { return v3_native_bridge_native_answer() != 42; }\n')!
	mut state := V3ModuleCacheState{
		manager:          modulecache.Manager{
			dir:     os.join_path(root, 'cache')
			enabled: true
		}
		bundle_valid:     true
		native_bridge:    true
		bridge_functions: {
			'native_answer':      'native_answer'
			'unavailable_unused': 'unavailable_unused'
		}
	}
	for phase in ['cold', 'warm'] {
		needs_source := phase == 'cold'
			|| v3_tcc_dylib_needs_native_sources(&state, header, [source], @VEXEROOT)
		input := if needs_source { source } else { header }
		mut p := parser.Parser.new(pref.new_preferences())
		a := p.parse_files([main, input])
		assert p.diagnostics.len == 0, p.diagnostics.str()
		mut tc := types.TypeChecker.new(a)
		tc.collect(a)
		tc.check_semantics()
		assert tc.errors.len == 0, tc.errors.str()
		if phase == 'cold' {
			os.write_file(header, modulecache.module_header(a, &tc, 'bridgecase', @VEXEROOT,
				map[string]string{}))!
		}
		used := markused.mark_used(a, &tc)
		cache_used := markused.mark_used_for_cache(a, &tc, []string{}, if needs_source {
			{
				'bridgecase': true
			}
		} else {
			map[string]bool{}
		})
		generated := '#include "${native}"\n/* V3CACHE_BODY_BEGIN */\n/* V3CACHE_MODULE main */\n\n/* V3CACHE_BODY_END */\n'
		prepared := prepare_v3_module_cache(generated, &cache_used, &used, &tc, '-std=gnu11',
			'', '', '', []string{}, false, '', 'fixture', unsafe { nil }, mut state)!
		owner := os.join_path(root, '${phase}_owner.c')
		os.write_file(owner, prepared.program_prefix_source)!
		output := os.join_path(root, phase)
		built := os.exec([cc, owner, consumer, '-o', output])
		assert built.exit_code == 0, '${phase} (${input}): ${built.output}'
		assert !prepared.program_prefix_source.contains('v3_native_bridge_unavailable_unused')
		ran := os.exec([output])
		assert ran.exit_code == 0, '${phase}: ${ran.output}'
	}
}

fn test_native_bridge_source_retention_keeps_standard_headers_cached() ! {
	$if windows {
		return
	}
	root := os.join_path(os.vtmp_dir(), 'native_bridge_source_retention_${os.getpid()}')
	checkout := os.join_path(root, 'checkout')
	checkout_target := os.join_path(root, 'original', 'checkout')
	library := os.join_path(root, 'original', 'vlib')
	external := os.join_path(root, 'external', 'binding.c.v')
	local_source := os.join_path(checkout, 'local.c.v')
	library_source := os.join_path(library, 'os', 'binding.c.v')
	os.mkdir_all(checkout_target)!
	os.mkdir_all(os.join_path(library, 'os'))!
	os.mkdir_all(os.dir(external))!
	defer { os.rmdir_all(root) or {} }
	os.symlink(checkout_target, checkout)!
	os.symlink(library, os.join_path(checkout, 'vlib'))!
	// Source files must exist so real_path resolves their symlinked parents.
	for source in [external, local_source, library_source] {
		os.write_file(source, 'module binding\n')!
	}
	header := os.join_path(root, 'binding.vh')
	// The native header need not exist: only the generated V interface is read.
	os.write_file(header, 'module binding\n#include "missing.h"\nfn C.answer() i32\n')!
	mut state := V3ModuleCacheState{ native_bridge: true }
	assert v3_tcc_dylib_needs_native_sources(&state, header, [external], checkout)
	assert !v3_tcc_dylib_needs_native_sources(&state, header, [local_source], checkout)
	assert !v3_tcc_dylib_needs_native_sources(&state, header, [library_source], checkout)
	assert !v3_tcc_dylib_needs_native_sources(&state, header, [os.join_path(checkout, 'vlib',
		'os', 'binding.c.v')], checkout)
	state.native_bridge = false
	assert !v3_tcc_dylib_needs_native_sources(&state, header, [external], checkout)
	state.native_bridge = true
	for text in ['module binding\nfn C.answer() i32\n',
		'module binding\n#include "missing.h"\npub fn answer() i32\n'] {
		os.write_file(header, text)!
		assert !v3_tcc_dylib_needs_native_sources(&state, header, [external], checkout)
	}
	for extension in ['m', 'mm'] {
		os.write_file(header, 'module binding\n#include "missing.${extension}"\n')!
		assert v3_tcc_dylib_needs_native_sources(&state, header, [external], checkout)
	}
}
