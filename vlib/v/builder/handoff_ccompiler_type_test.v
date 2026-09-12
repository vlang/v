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

// neutral_cc_pointing_at_tcc reproduces a machine whose `cc` is TinyCC, under a name that
// says nothing about it, so only probing the executable can tell.
fn neutral_cc_pointing_at_tcc() !string {
	tcc := bundled_tcc()
	if tcc == '' {
		return error('no usable bundled tcc')
	}
	dir := os.join_path(os.vtmp_dir(), 'handoff_cc_${os.getpid()}')
	os.mkdir_all(dir)!
	cc := os.join_path(dir, 'cc')
	os.rm(cc) or {}
	os.symlink(tcc, cc)!
	return cc
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
