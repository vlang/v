module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	fn C.v_freestanding_program_names_set(uint, uint, uint)
	fn C.v_freestanding_program_short_name() uint
	fn C.v_freestanding_program_auxiliary() uint

	// Kernel argv/environment/auxiliary-vector storage lives for the process.
	// Borrow these addresses without allocating during preinitialization.
	@[export: 'v_freestanding_program_names'; markused]
	fn bare_program_names(argc i32, argv uint) {
		// The shim has its own private builtins and no C main. Publish the
		// kernel arguments here, before any ELF constructor initializes V.
		g_main_argc = int(argc)
		g_main_argv = voidptr(argv)
		arguments := unsafe { &uint(argv) }
		full := if argc > 0 { unsafe { arguments[0] } } else { uint(c'') }
		mut short_name := full
		name := unsafe { &u8(full) }
		mut offset := 0
		for unsafe { name[offset] } != 0 {
			if unsafe { name[offset] } == `/` { short_name = full + uint(offset + 1) }
			offset++
		}
		mut environment := int(argc) + 1
		for unsafe { arguments[environment] } != 0 { environment++ }
		auxiliary := argv + uint(environment + 1) * sizeof(uint)
		C.v_freestanding_program_names_set(full, short_name, auxiliary)
	}

	fn bare_program_short_name() string {
		pointer := C.v_freestanding_program_short_name()
		if pointer == 0 { return '' }
		// This is a borrowed view of the process's original argv, never freed.
		return unsafe { tos2(&u8(pointer)) }
	}
}
