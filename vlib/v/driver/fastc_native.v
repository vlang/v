module driver

import v.pref

// vfmt off
$if !skip_fastc ? {
	import v.gen.fastc
}
// vfmt on

$if !skip_fastc ? {
	fn compile_v3_fastc_tcc_generation(input string, mut generation fastc.GenerationResult, bin_file string, prefs &pref.Preferences, environment_flags []string, user_flags []string, link_flags []string, sdk string, is_debug bool, cache_enabled bool, keep_c bool, retry_compilation bool, mut prestarted fastc.FastcPrestartedCUnits) V3FastCCompileResult {
		mut flags := environment_flags.clone()
		flags << generation.c_flags
		flags << user_flags
		flags << link_flags
		if prefs.normalized_target_os() == 'macos' && prefs.ccompiler == 'tinyc'
			&& (c_flags_need_objective_c(flags) || '-framework' in flags
				|| generation.c_pieces[..generation.units.head_end].any(it.starts_with('#include ') && (it.contains('.m"') || it.contains('.mm"')))) {
			if !retry_compilation {
				return V3FastCCompileResult{
					command: 'FastC native compilation'
					output:  'FastC Objective-C inputs require Clang; use `-cc clang` or enable compilation retries'
				}
			}
			// The C toolchain owns native headers. Regenerate compiler-dependent V
			// branches for Clang instead of projecting a C interface for TinyCC.
			mut native_prefs := *prefs
			native_prefs.ccompiler = 'clang'
			generation = fastc.generate_files_with_source_paths([input], &native_prefs) or {
				return V3FastCCompileResult{ command: 'FastC native compilation', output: err.msg() }
			}
			if keep_c {
				fastc.write_c_pieces(keep_c_output_file(bin_file), generation.c_pieces) or {
					return V3FastCCompileResult{ command: 'FastC native compilation', output: err.msg() }
				}
			}
			return compile_v3_fastc_clang_source(generation.c_pieces, generation.units, bin_file, &native_prefs, environment_flags, generation.c_flags, user_flags, link_flags, sdk, '/usr/bin/clang', is_debug, generation.uses_threads)
		}
		return compile_v3_fastc_source(generation.c_pieces, generation.units, bin_file, prefs, environment_flags, generation.c_flags, user_flags, link_flags, sdk, is_debug, generation.uses_threads, cache_enabled, mut prestarted)
	}
}
