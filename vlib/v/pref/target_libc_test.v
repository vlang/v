module pref

fn non_host_os() OS {
	return if get_host_os() == .linux { OS.windows } else { OS.linux }
}

// detected_host_musl reports what `detect_musl` found for this machine. A host-native
// build is the one shape that never discards the probe, so it reads it back unchanged.
fn detected_host_musl() bool {
	host, _ := parse_args([], ['-o', 'prog', 'a.v'])
	return host.is_musl
}

fn test_host_native_build_keeps_the_detected_libc() {
	mut p := Preferences{
		is_glibc: true
		os:       ._auto
		out_name: 'prog'
	}
	p.forget_host_glibc_for_foreign_targets()
	assert p.is_glibc
	assert !p.is_musl
}

fn test_explicitly_targeting_the_host_os_keeps_the_detected_libc() {
	mut p := Preferences{
		is_musl:  true
		os:       get_host_os()
		out_name: 'prog'
	}
	p.forget_host_glibc_for_foreign_targets()
	assert p.is_musl
	assert !p.is_glibc
}

fn test_c_output_forgets_the_detected_libc() {
	// V only emits C here; the toolchain that compiles and links it may well use
	// another libc, so the host probe says nothing about the final program.
	mut p := Preferences{
		is_glibc: true
		os:       ._auto
		out_name: 'build/desktop.c'
	}
	p.forget_host_glibc_for_foreign_targets()
	assert !p.is_glibc
	assert !p.is_musl
}

fn test_object_output_forgets_the_detected_libc() {
	// `-o unit.o` and `-is_o` only ever reach the C compiler with `-c`, so the libc
	// belongs to whoever links the object afterwards.
	mut p := Preferences{
		is_glibc: true
		os:       ._auto
		is_o:     true
		out_name: 'unit.o'
	}
	p.forget_host_glibc_for_foreign_targets()
	assert !p.is_glibc
	assert !p.is_musl
}

fn test_stdout_output_forgets_the_detected_libc() {
	// `-o -` prints the C and returns before `cc` is ever invoked.
	mut p := Preferences{
		is_glibc: true
		os:       ._auto
		out_name: '/tmp/-'
	}
	assert p.should_output_to_stdout()
	p.forget_host_glibc_for_foreign_targets()
	assert !p.is_glibc
	assert !p.is_musl
}

fn test_detected_musl_survives_non_linking_output() {
	// An unset `musl` define is not "libc unknown", it is "the target is not musl", and
	// `$if linux && !musl ?` acts on it: `v_gettid` would switch to glibc's `C.gettid()`
	// and `picoev` would include <sys/cdefs.h>, which musl does not ship. So a musl host
	// keeps its libc even where a glibc one gives it up.
	for out_name in ['build/desktop.c', 'unit.o', '/tmp/-'] {
		mut p := Preferences{
			is_musl:  true
			os:       ._auto
			is_o:     out_name.ends_with('.o')
			out_name: out_name
		}
		p.forget_host_glibc_for_foreign_targets()
		assert p.is_musl, out_name
		assert !p.is_glibc, out_name
	}

	mut cross := Preferences{
		is_musl:        true
		os:             non_host_os()
		output_cross_c: true
		out_name:       'v.c'
	}
	cross.forget_host_glibc_for_foreign_targets()
	assert cross.is_musl
}

fn test_generated_c_project_forgets_the_detected_libc() {
	// `-generate-c-project` writes the C next to build.sh/Makefile and returns, so the
	// libc belongs to whatever those scripts are pointed at, not to this machine.
	mut p := Preferences{
		is_glibc:           true
		os:                 ._auto
		generate_c_project: 'out/cproject'
		out_name:           'prog'
	}
	p.forget_host_glibc_for_foreign_targets()
	assert !p.is_glibc
	assert !p.is_musl
}

fn test_foreign_os_forgets_the_detected_libc() {
	mut p := Preferences{
		is_glibc: true
		os:       non_host_os()
		out_name: 'prog'
	}
	p.forget_host_glibc_for_foreign_targets()
	assert !p.is_glibc
	assert !p.is_musl
}

fn test_portable_c_output_forgets_the_detected_libc() {
	mut p := Preferences{
		is_glibc:       true
		os:             ._auto
		output_cross_c: true
		out_name:       'v.c'
	}
	p.forget_host_glibc_for_foreign_targets()
	assert !p.is_glibc
	assert !p.is_musl
}

fn test_explicit_libc_options_survive_foreign_targets() {
	mut glibc := Preferences{
		is_glibc:         true
		libc_set_by_flag: true
		os:               non_host_os()
		out_name:         'build/desktop.c'
	}
	glibc.forget_host_glibc_for_foreign_targets()
	assert glibc.is_glibc

	mut musl := Preferences{
		is_musl:          true
		libc_set_by_flag: true
		os:               non_host_os()
		out_name:         'build/desktop.c'
	}
	musl.forget_host_glibc_for_foreign_targets()
	assert musl.is_musl
}

fn test_object_output_is_resolved_before_the_libc() {
	// A `.o` output name only sets `is_o` late in parsing; the libc has to be settled
	// after that point, or object output would silently keep the host glibc.
	p, _ := parse_args([], ['-o', 'unit.o', 'a.v'])
	assert p.is_o
	assert !p.is_glibc
	// A musl host keeps its libc here, so this has to be read against the host.
	assert p.is_musl == detected_host_musl()
}

fn test_handed_off_output_does_not_default_to_tinyc() {
	// `$if tinyc && !glibc` calls `tcc_backtrace`, declared only behind `#ifdef
	// __TINYC__`, so a defaulted tcc would make clang and gcc reject the output.
	for out_name in ['out.c', 'unit.o', '/tmp/-'] {
		mut p := Preferences{
			os:       ._auto
			is_o:     out_name.ends_with('.o')
			out_name: out_name
		}
		p.try_to_use_tcc_by_default()
		assert p.ccompiler == '', out_name
	}

	mut c_project := Preferences{
		os:                 ._auto
		generate_c_project: 'out/cproject'
		out_name:           'prog'
	}
	c_project.try_to_use_tcc_by_default()
	assert c_project.ccompiler == ''

	mut linked_by_v := Preferences{
		os:       ._auto
		out_name: 'prog'
	}
	linked_by_v.try_to_use_tcc_by_default()
	// Only meaningful where tcc is the default at all; elsewhere this stays empty.
	assert linked_by_v.ccompiler == '' || linked_by_v.ccompiler.contains('tcc')
}

fn test_explicit_tinyc_survives_handed_off_output() {
	mut p := Preferences{
		ccompiler: 'tcc'
		os:        ._auto
		out_name:  'out.c'
	}
	p.try_to_use_tcc_by_default()
	assert p.ccompiler.contains('tcc')
}

fn test_libc_options_are_recorded_as_explicit_by_the_parser() {
	musl, _ := parse_args([], ['-musl', '-o', 'x.c', 'a.v'])
	assert musl.is_musl
	assert !musl.is_glibc
	assert musl.libc_set_by_flag

	glibc, _ := parse_args([], ['-glibc', '-o', 'x.c', 'a.v'])
	assert glibc.is_glibc
	assert !glibc.is_musl
	assert glibc.libc_set_by_flag

	plain, _ := parse_args([], ['-o', 'x.c', 'a.v'])
	assert !plain.libc_set_by_flag
	// Without an explicit option there is no glibc to carry into the generated C, while
	// a detected musl is kept, so that reads against the host.
	assert !plain.is_glibc
	assert plain.is_musl == detected_host_musl()
}
