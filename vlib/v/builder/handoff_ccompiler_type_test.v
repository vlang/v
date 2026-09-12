module builder

import os
import v.pref

// bundled_tcc returns the bundled TinyCC, or '' when this checkout has no working one.
fn bundled_tcc() string {
	tcc := os.join_path(@VEXEROOT, 'thirdparty', 'tcc', 'tcc.exe')
	if !os.is_file(tcc) || !os.is_executable(tcc) {
		return ''
	}
	if os.execute('${os.quoted_path(tcc)} -v').exit_code != 0 {
		return ''
	}
	return tcc
}

// neutral_cc_pointing_at reproduces a machine whose `cc` is some particular compiler,
// under a name that says nothing about it, so only probing the executable can tell.
fn neutral_cc_pointing_at(compiler string) !string {
	if compiler == '' {
		return error('no usable compiler to point at')
	}
	dir := os.join_path(os.vtmp_dir(), 'handoff_cc_${os.getpid()}_${os.file_name(compiler)}')
	os.mkdir_all(dir)!
	cc := os.join_path(dir, 'cc')
	os.rm(cc) or {}
	os.symlink(compiler, cc)!
	return cc
}

fn neutral_cc_pointing_at_tcc() !string {
	return neutral_cc_pointing_at(bundled_tcc())!
}

fn neutral_cc_pointing_at_clang() !string {
	return neutral_cc_pointing_at(os.find_abs_path_of_executable('clang') or { '' })!
}

fn test_c_only_output_does_not_resolve_an_implicit_cc_to_tinyc() {
	cc := neutral_cc_pointing_at_tcc() or { return }
	defer {
		os.rmdir_all(os.dir(cc)) or {}
	}
	for out_name in ['out.c', '/tmp/-'] {
		// V writes the C and names no compiler anywhere, so what `cc` happens to be here
		// must not turn on `$if tinyc` and emit `tcc_backtrace` into portable output.
		mut p := pref.Preferences{
			ccompiler: cc
			out_name:  out_name
		}
		resolve_ccompiler_type_and_pkgconfig_mode(mut p)
		assert p.ccompiler_type == .gcc, out_name
	}
}

fn test_c_only_output_still_resolves_a_cc_that_is_not_tinyc() {
	cc := neutral_cc_pointing_at_clang() or { return }
	defer {
		os.rmdir_all(os.dir(cc)) or {}
	}
	// Only the TinyCC outcome is dropped. `cc_from_string('cc')` falls back to `.gcc`, so
	// leaving this unprobed would flip `$if clang` off and `$if gcc` on for every macOS
	// user of `-o out.c`, which has nothing to do with the libc this branch is about.
	for out_name in ['out.c', '/tmp/-'] {
		mut p := pref.Preferences{
			ccompiler: cc
			out_name:  out_name
		}
		resolve_ccompiler_type_and_pkgconfig_mode(mut p)
		assert p.ccompiler_type == .clang, out_name
	}
}

fn test_late_tinyc_resolution_still_disables_unsupported_backtraces() {
	cc := neutral_cc_pointing_at_tcc() or { return }
	defer {
		os.rmdir_all(os.dir(cc)) or {}
	}
	// `fill_with_defaults()` ran this normalisation while `cc` still looked like gcc, so
	// it has to run again once the name is resolved: TinyCC shared libraries must not
	// depend on its backtrace runtime symbols.
	mut p := pref.Preferences{
		ccompiler:          cc
		is_shared:          true
		generate_c_project: 'out/cproject'
		out_name:           'libx'
	}
	resolve_ccompiler_type_and_pkgconfig_mode(mut p)
	assert p.ccompiler_type == .tinyc
	assert 'no_backtrace' in p.compile_defines_all
}

fn test_generated_c_project_resolves_the_compiler_its_scripts_will_name() {
	cc := neutral_cc_pointing_at_tcc() or { return }
	defer {
		os.rmdir_all(os.dir(cc)) or {}
	}
	// `-generate-c-project` writes this compiler into build.sh/Makefile, so the C beside
	// them has to be generated for it: leaving the type at `.gcc` would drop the TinyCC
	// inserts in `sync.stdatomic` from C the scripts then hand to TinyCC.
	mut p := pref.Preferences{
		ccompiler:          cc
		generate_c_project: 'out/cproject'
		out_name:           'prog'
	}
	resolve_ccompiler_type_and_pkgconfig_mode(mut p)
	assert p.ccompiler_type == .tinyc
}

fn test_a_compiler_v_actually_runs_is_still_resolved() {
	cc := neutral_cc_pointing_at_tcc() or { return }
	defer {
		os.rmdir_all(os.dir(cc)) or {}
	}
	// V links this one itself, so the truth about `cc` matters: `$if tinyc` drives the
	// `#flag` and atomics shims that TinyCC needs to compile the generated C at all.
	mut linked_by_v := pref.Preferences{
		ccompiler: cc
		out_name:  'prog'
	}
	resolve_ccompiler_type_and_pkgconfig_mode(mut linked_by_v)
	assert linked_by_v.ccompiler_type == .tinyc

	// Same for an object: V compiles it, even though somebody else links it.
	mut object := pref.Preferences{
		ccompiler: cc
		is_o:      true
		out_name:  'unit.o'
	}
	resolve_ccompiler_type_and_pkgconfig_mode(mut object)
	assert object.ccompiler_type == .tinyc
}

fn test_explicit_cc_is_resolved_even_for_c_only_output() {
	cc := neutral_cc_pointing_at_tcc() or { return }
	defer {
		os.rmdir_all(os.dir(cc)) or {}
	}
	// `-cc` is the user describing the toolchain that will build the generated C.
	mut p := pref.Preferences{
		ccompiler:             cc
		ccompiler_set_by_flag: true
		out_name:              'out.c'
	}
	resolve_ccompiler_type_and_pkgconfig_mode(mut p)
	assert p.ccompiler_type == .tinyc
}
