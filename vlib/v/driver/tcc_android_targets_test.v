module driver

import os
import v.cmdexec
import v.gen.c as cgen
import v.parser
import v.pref

fn test_tcc_android_targets_link_the_atomic_fence() {
	for target_os in ['android', 'termux'] {
		for arch in ['arm64', 'arm32', 'amd64', 'x86'] {
			prefs := &pref.Preferences{
				vroot:  @VEXEROOT
				target: pref.Target{
					os:   target_os
					arch: arch
				}
			}
			expected := if arch == 'amd64' {
				os.join_path(@VEXEROOT, 'vlib', 'sync', 'stdatomic', 'atomic_fence_amd64.S')
			} else {
				os.join_path(@VEXEROOT, 'thirdparty', 'stdatomic', 'nix', 'atomic.S')
			}
			assert tcc_atomic_s_arg(prefs) == expected, '${target_os}/${arch}'
		}
	}
}

fn test_tcc_android_amd64_fence_does_not_duplicate_the_runtime_symbol() {
	$if !(linux && amd64) {
		return
	}
	compiler := os.find_abs_path_of_executable('cc') or { return }
	root := os.join_path(os.vtmp_dir(), 'v_tcc_android_atomic_${os.getpid()}')
	os.mkdir_all(root)!
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'probe.c')
	// libtcc1.a supplies __atomic_thread_fence. V must link its own distinct
	// _V_atomic_thread_fence without replacing or duplicating that symbol.
	os.write_file(source, 'void __atomic_thread_fence(int order) { (void)order; }
extern void _V_atomic_thread_fence(int order);
int main(void) {
	_V_atomic_thread_fence(5);
	return 0;
}
')!
	for target_os in ['android', 'termux'] {
		prefs := &pref.Preferences{
			vroot:  @VEXEROOT
			target: pref.Target{
				os:   target_os
				arch: 'amd64'
			}
		}
		output := os.join_path(root, target_os)
		build := cmdexec.run(compiler, [source, tcc_atomic_s_arg(prefs), '-o', output])
		assert build.exit_code == 0, '${target_os}: ${build.output}'
		run := cmdexec.run(output, [])
		assert run.exit_code == 0, '${target_os}: ${run.output}'
	}
}

fn test_android_dynamic_loading_links_libdl_in_preflight_and_final_flags() {
	source := os.join_path(@VEXEROOT, 'vlib', 'dl', 'dl_nix.c.v')
	for target_os in ['linux', 'android', 'termux', 'macos', 'freebsd'] {
		mut prefs := pref.new_preferences()
		prefs.vroot = @VEXEROOT
		prefs.target = pref.target_from(target_os, 'arm64') or { panic(err) }
		prefs.ccompiler = 'tinyc'
		mut p := parser.Parser.new(prefs)
		a := p.parse_file(source)
		assert p.diagnostics.len == 0, p.diagnostics.str()
		expected := if target_os in ['linux', 'android', 'termux'] { ['-ldl'] } else { []string{} }
		preflight := cgen.preflight_directive_flags(a, prefs.vroot, prefs.target,
			prefs.compile_values)
		assert preflight == expected, '${target_os}: ${preflight}'
		flags := cgen.cache_directive_flags(a, prefs.vroot, prefs.target, prefs.compile_values)
		assert flags == expected, '${target_os}: ${flags}'
	}
}
