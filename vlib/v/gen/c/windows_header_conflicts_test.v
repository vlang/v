module c

import os
import v.cmdexec
import v.flat
import v.pref

fn windows_header_conflict_prefix(target_os string, cross_c bool) string {
	mut g := FlatGen.new()
	g.a = &flat.FlatAst{}
	g.set_target(pref.target_from(target_os, 'amd64') or { panic(err) })
	g.set_output_cross_c(cross_c)
	g.preinclude_directives = ['#include "winapi_config.h"']
	g.module_imports['raylib'] = ['builtin']
	g.add_c_directive('builtin', '#include <gc.h>', false)
	g.add_c_directive('raylib', '#include "raylib_sound.h"', false)
	g.emit_translation_unit_include_directives()
	g.emit_c_directives(false)
	return g.sb.str()
}

fn windows_header_conflict_cc() ?string {
	name := $if windows { 'gcc' } $else { 'x86_64-w64-mingw32-gcc' }
	return os.find_abs_path_of_executable(name) or { return none }
}

fn test_windows_header_configuration_precedes_gc_and_raylib_headers() {
	for target_os in ['windows', 'linux'] {
		prefix := windows_header_conflict_prefix(target_os, target_os == 'linux')
		config_index := prefix.index('#include "winapi_config.h"')?
		lean_index := prefix.index('#define WIN32_LEAN_AND_MEAN')?
		gc_index := prefix.index('#include <gc.h>')?
		raylib_index := prefix.index('#include "raylib_sound.h"')?
		assert config_index < lean_index
		assert lean_index < gc_index
		assert gc_index < raylib_index
	}
}

fn test_windows_header_defaults_respect_existing_macros_and_full_headers() {
	cc := windows_header_conflict_cc() or { return }
	dir := os.join_path(os.vtmp_dir(), 'windows_header_defaults_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	source := os.join_path(dir, 'defaults.c')
	// Stop before gc.h: this checks macro defaults and preinclude overrides
	// without depending on the API declarations selected by those macros.
	prefix := windows_header_conflict_prefix('linux', true).all_before('#include <gc.h>')
	os.write_file(source, prefix + '
#if defined(EXPECT_LEAN) && !defined(WIN32_LEAN_AND_MEAN)
#error missing default WIN32_LEAN_AND_MEAN
#endif
#if !defined(EXPECT_LEAN) && defined(WIN32_LEAN_AND_MEAN)
#error unexpected WIN32_LEAN_AND_MEAN
#endif
#ifdef EXPECT_LEAN_VALUE
#if WIN32_LEAN_AND_MEAN != EXPECT_LEAN_VALUE
#error existing WIN32_LEAN_AND_MEAN value changed
#endif
#endif
')!
	for fixture, defines in {
		'default':     ['-DEXPECT_LEAN']
		'flag_lean':   ['-DEXPECT_LEAN', '-DWIN32_LEAN_AND_MEAN=17', '-DEXPECT_LEAN_VALUE=17']
		'config_lean': ['-DEXPECT_LEAN', '-DEXPECT_LEAN_VALUE=17']
		'flag_full':   ['-DWIN32_FULL']
		'config_full': []string{}
		'nonwindows':  ['-U_WIN32']
	} {
		configuration := match fixture {
			'config_lean' { '#define WIN32_LEAN_AND_MEAN 17\n' }
			'config_full' { '#define WIN32_FULL\n' }
			else { '' }
		}
		os.write_file(os.join_path(dir, 'winapi_config.h'), configuration)!
		mut args := ['-E', '-P', '-Werror', '-I', dir]
		args << defines
		args << source
		result := cmdexec.run(cc, args)
		assert result.exit_code == 0, '${fixture}\n${cmdexec.display(cc, args)}\n${result.output}'
	}
}

fn test_windows_gc_headers_allow_raylib_sound_with_exclusion_macros() {
	cc := windows_header_conflict_cc() or { return }
	gc_include := os.join_path(@VEXEROOT, 'thirdparty', 'libgc', 'include')
	if !os.is_file(os.join_path(gc_include, 'gc.h')) {
		return
	}
	dir := os.join_path(os.vtmp_dir(), 'windows_header_conflicts_${os.getpid()}')
	os.mkdir_all(dir)!
	defer { os.rmdir_all(dir) or {} }
	// raylib's Sound and PlaySound must remain available after builtin's gc.h
	// loads windows.h. The declarations reproduce the conflict without raylib.
	os.write_file(os.join_path(dir, 'raylib_sound.h'), '
typedef struct Sound { unsigned int frameCount; } Sound;
void PlaySound(Sound sound);
')!
	for macros in [['NOUSER', 'NOGDI'], ['NOMSG', 'NOGDI'], ['NOUSER', 'NOMSG', 'NOGDI']] {
		mut configuration := '#ifdef _WINDOWS_\n#error windows.h loaded before configuration\n#endif\n'
		for name in macros {
			configuration += '#define ${name}\n'
		}
		os.write_file(os.join_path(dir, 'winapi_config.h'), configuration)!
		for target_os in ['windows', 'linux'] {
			source := os.join_path(dir, 'headers_${target_os}.c')
			prefix := windows_header_conflict_prefix(target_os, target_os == 'linux')
			os.write_file(source, prefix + 'void use_sound(Sound sound) { PlaySound(sound); }\n')!
			// Match the threaded Boehm configuration used by Windows gcc builds.
			args := ['-fsyntax-only', '-DGC_THREADS=1', '-DGC_WIN32_THREADS=1', '-I', gc_include,
				'-I', dir, source]
			result := cmdexec.run(cc, args)
			assert result.exit_code == 0, '${macros}\n${cmdexec.display(cc, args)}\n${result.output}'
		}
	}
}
