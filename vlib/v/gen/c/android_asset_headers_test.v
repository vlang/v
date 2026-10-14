module c

import os
import v.flat
import v.parser
import v.pref
import v.types

fn android_asset_header_test_gen(program string) FlatGen {
	return android_asset_header_test_gen_with_cached_owner(program, '')
}

fn android_asset_header_test_gen_with_cached_owner(program string, cached_owner string) FlatGen {
	root := os.join_path(os.vtmp_dir(), 'android_asset_headers_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer { os.rmdir_all(root) or {} }
	source := os.join_path(root, 'main.v')
	os.write_file(source, program) or { panic(err) }
	mut prefs := pref.new_preferences()
	prefs.target = pref.target_from('android', 'arm64') or { panic(err) }
	mut p := parser.Parser.new(prefs)
	binding := os.join_path(@VEXEROOT, 'vlib', 'os', 'os_android_outside_termux.c.v')
	p.parse_file(binding)
	p.parse_file(source)
	assert p.diagnostics.len == 0, p.diagnostics.str()
	cached_header := os.join_path(root, 'os.vh')
	if cached_owner.len > 0 {
		p.a.cached_header_sources[cached_header] = cached_owner
		for i, node in p.a.nodes {
			if node.kind == .file && node.value == binding {
				p.a.nodes[i].value = cached_header
			}
		}
	}
	mut tc := types.TypeChecker.new(p.a)
	tc.collect(p.a)
	mut g := FlatGen.new()
	g.a = p.a
	g.tc = &tc
	g.compiler_vroot = @VEXEROOT
	g.set_target(prefs.target)
	used_fns := {
		'main':      true
		'main.main': true
	}
	g.used_fns = &used_fns
	for header in ['<android/asset_manager.h>', '<android/asset_manager_jni.h>',
		'<android/native_activity.h>'] {
		g.collect_c_directive('os', flat.Node{ kind: .directive, value: 'include', typ: flat.source_type_id(header) },
			if cached_owner.len > 0 { cached_header } else { binding }, false)
	}
	return g
}

fn test_unused_android_asset_sdk_stays_out_of_core_programs() {
	for program in ['import os\nfn main() { println(os.getpid()) }',
		'import os\nfn main() { println("AssetMode NativeActivity AAsset are just text") }'] {
		for owner in ['', os.join_path(@VEXEROOT, 'vlib', 'os', 'args.c.v')] {
			mut g := android_asset_header_test_gen_with_cached_owner(program, owner)
			assert g.omits_unused_android_asset_sdk()
			assert g.c_directives.len == 0
			for native in ['C.AAsset', 'C.AAssetManager', 'C.ANativeActivity'] {
				assert g.skip_builtin_struct(native), native
			}
			g.enum_decls()
			assert !g.sb.str().contains('AASSET_'), g.sb.str()
		}
	}
}

fn test_android_asset_type_enum_and_function_uses_keep_sdk_headers() {
	for program in [
		'import os\nfn keep(activity &os.NativeActivity) {}\nfn main() {}',
		'import os\nfn keep(manager &os.AssetManager) {}\nfn main() {}',
		'import os\nfn main() { _ = os.AssetMode.streaming }',
		'import os\nfn main() { C.AAsset_close(unsafe { nil }) }',
	] {
		for owner in ['', os.join_path(@VEXEROOT, 'vlib', 'os', 'args.c.v')] {
			mut g := android_asset_header_test_gen_with_cached_owner(program, owner)
			assert !g.omits_unused_android_asset_sdk(), program
			assert g.c_directives.len == 3, program
			// SDK ownership also prevents synthesizing the binding's partial layout.
			assert g.skip_builtin_struct('C.ANativeActivity'), program
			g.enum_decls()
			assert g.sb.str().contains('AASSET_MODE_STREAMING'), g.sb.str()
		}
	}
}

fn test_android_asset_sdk_guards_preserve_explicit_user_headers_and_other_modes() {
	mut g := android_asset_header_test_gen('import os\nfn main() {}')
	assert g.omits_unused_android_asset_sdk()
	g.collect_c_directive('main', flat.Node{ kind: .directive, value: 'include', typ: flat.source_type_id('<android/native_activity.h>') },
		'/project/os_android_outside_termux.c.v', false)
	assert g.c_directives.len == 1
	g.set_output_cross_c(true)
	assert !g.omits_unused_android_asset_sdk()
	g.set_output_cross_c(false)
	g.set_object_file_mode(true)
	assert !g.omits_unused_android_asset_sdk()
	g.set_object_file_mode(false)
	g.set_target_libc_headers(true)
	assert !g.omits_unused_android_asset_sdk()
	g.set_target_libc_headers(false)
	g.set_no_builtin(true)
	assert !g.omits_unused_android_asset_sdk()
	for owner in ['/project/vlib/os/args.c.v', @VEXEROOT + '/vlib/os_other/args.c.v'] {
		foreign := android_asset_header_test_gen_with_cached_owner('import os\nfn main() {}',
			owner)
		assert foreign.c_directives.len == 3
	}
	assert !g.is_android_asset_binding('/cache/unregistered/os.vh')
	mut cached := android_asset_header_test_gen_with_cached_owner('import os\nfn main() {}',
		os.join_path(@VEXEROOT, 'vlib', 'os', 'args.c.v'))
	used_fns := {
		'main':                 true
		'os.AssetManager.open': true
	}
	cached.used_fns = &used_fns
	assert !cached.omits_unused_android_asset_sdk()
}
