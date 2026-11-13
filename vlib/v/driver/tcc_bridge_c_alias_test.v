module driver

import os
import v.gen.c as cgen
import v.markused
import v.modulecache
import v.parser
import v.pref
import v.types

fn test_native_bridge_uses_c_abi_names_for_aliased_bindings() ! {
	$if !linux && !macos {
		return
	}
	cc := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'native_bridge_c_alias_${os.getpid()}')
	os.mkdir_all(root)!
	defer { os.rmdir_all(root) or {} }
	native := os.join_path(root, 'native.h')
	os.write_file(native, 'static int native_answer(int value) { return value + 1; }\n')!
	main := os.join_path(root, 'main.v')
	source := os.join_path(root, 'bridge.c.v')
	os.write_file(main, 'module main\nimport bridgecase\nfn main() { _ = bridgecase.answer() }\n')!
	os.write_file(source, 'module bridgecase\n#include "${native}"\n@[c: \'native_answer\']\nfn C.answer_alias(value i32) i32\n@[c: "native_answer"]\nfn C.second_alias(value i32) i32\n@[c: \'unavailable_unused\']\nfn C.unused_alias() i32\n@[export: "external_callback"]\nfn exported_callback() i32 { return 42 }\n@[c: \'external_callback\']\nfn C.callback_alias() i32\npub fn answer() i32 { return C.answer_alias(40) + C.second_alias(0) }\n')!
	mut prefs := pref.new_preferences()
	prefs.target = pref.host_target()
	prefs.vroot = @VEXEROOT
	mut p := parser.Parser.new(prefs)
	a := p.parse_files([main, source])
	assert p.diagnostics.len == 0, p.diagnostics.str()
	closure := v3_tcc_dylib_native_closure(a, prefs, [], cc, &cgen.CacheNativeInputs{})
	assert closure.unassignable == '', closure.unassignable
	assert 'answer_alias' in closure.functions
	assert 'second_alias' in closure.functions
	mut tc := types.TypeChecker.new(a)
	tc.collect(a)
	tc.check_semantics()
	assert tc.errors.len == 0, tc.errors.str()
	used := markused.mark_used(a, &tc)
	cache_used := markused.mark_used_for_cache(a, &tc, []string{}, {
		'bridgecase': true
	})
	mut state := V3ModuleCacheState{
		manager:          modulecache.Manager{
			dir:     os.join_path(root, 'cache')
			enabled: true
		}
		bundle_valid:     true
		native_bridge:    true
		bridge_functions: closure.functions
	}
	generated := '#include "${native}"\n/* V3CACHE_BODY_BEGIN */\n/* V3CACHE_MODULE main */\n\n/* V3CACHE_BODY_END */\n'
	prepared := prepare_v3_module_cache(generated, &cache_used, &used, &tc, '-std=gnu11',
		'', '', '', []string{}, false, '', 'fixture', unsafe { nil }, mut state)!
	owner := os.join_path(root, 'owner.c')
	consumer := os.join_path(root, 'consumer.c')
	os.write_file(owner, prepared.program_prefix_source)!
	os.write_file(consumer, 'extern int (*v3_native_bridge_native_answer)(int);\nint main(void) { return v3_native_bridge_native_answer(41) != 42; }\n')!
	output := os.join_path(root, 'answer')
	built := os.exec([cc, owner, consumer, '-o', output])
	assert built.exit_code == 0, built.output
	ran := os.exec([output])
	assert ran.exit_code == 0, ran.output
	assert prepared.program_prefix_source.count('v3_native_bridge_native_answer') == 1
	assert !prepared.program_prefix_source.contains('v3_native_bridge_answer_alias')
	assert !prepared.program_prefix_source.contains('v3_native_bridge_second_alias')
	assert !prepared.program_prefix_source.contains('unavailable_unused')
	assert 'callback_alias' !in closure.functions
}
