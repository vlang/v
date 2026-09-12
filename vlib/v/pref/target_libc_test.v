module pref

fn non_host_os() OS {
	return if get_host_os() == .linux { OS.windows } else { OS.linux }
}

fn test_host_native_build_keeps_the_detected_libc() {
	mut p := Preferences{
		is_glibc: true
		os:       ._auto
		out_name: 'prog'
	}
	p.forget_host_libc_for_foreign_targets()
	assert p.is_glibc
	assert !p.is_musl
}

fn test_explicitly_targeting_the_host_os_keeps_the_detected_libc() {
	mut p := Preferences{
		is_musl:  true
		os:       get_host_os()
		out_name: 'prog'
	}
	p.forget_host_libc_for_foreign_targets()
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
	p.forget_host_libc_for_foreign_targets()
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
	p.forget_host_libc_for_foreign_targets()
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
	p.forget_host_libc_for_foreign_targets()
	assert !p.is_glibc
	assert !p.is_musl
}

fn test_foreign_os_forgets_the_detected_libc() {
	mut p := Preferences{
		is_glibc: true
		os:       non_host_os()
		out_name: 'prog'
	}
	p.forget_host_libc_for_foreign_targets()
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
	p.forget_host_libc_for_foreign_targets()
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
	glibc.forget_host_libc_for_foreign_targets()
	assert glibc.is_glibc

	mut musl := Preferences{
		is_musl:          true
		libc_set_by_flag: true
		os:               non_host_os()
		out_name:         'build/desktop.c'
	}
	musl.forget_host_libc_for_foreign_targets()
	assert musl.is_musl
}

fn test_object_output_is_resolved_before_the_libc() {
	// A `.o` output name only sets `is_o` late in parsing; the libc has to be settled
	// after that point, or object output would silently keep the host libc.
	p, _ := parse_args([], ['-o', 'unit.o', 'a.v'])
	assert p.is_o
	assert !p.is_glibc
	assert !p.is_musl
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
	// Without an explicit option there is nothing to carry into the generated C.
	assert !plain.is_glibc
	assert !plain.is_musl
}
