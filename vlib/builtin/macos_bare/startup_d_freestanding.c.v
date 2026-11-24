@[has_globals]
module builtin

// The standalone x86-64 loader runs before V initialization. These helpers must
// not allocate, touch TLS, or enter instrumentation until GS has been installed.
// ARM64 continues to use the system loader and libSystem.
$if amd64 && !native && !v_bare_no_startup ? {
	#include <mach-o/loader.h>
	#include <mach-o/ldsyms.h>
	#include <crt_externs.h>

	struct C.mach_header_64 {
		magic      u32
		cputype    i32
		cpusubtype i32
		filetype   u32
		ncmds      u32
		sizeofcmds u32
		flags      u32
		reserved   u32
	}

	struct C.load_command {
		cmd     u32
		cmdsize u32
	}

	struct C.segment_command_64 {
		cmd      u32
		cmdsize  u32
		segname  [16]char
		vmaddr   u64
		vmsize   u64
		fileoff  u64
		filesize u64
		maxprot  i32
		initprot i32
		nsects   u32
		flags    u32
	}

	struct C.section_64 {
		sectname  [16]char
		segname   [16]char
		addr      u64
		size      u64
		offset    u32
		@align    u32
		reloff    u32
		nreloc    u32
		flags     u32
		reserved1 u32
		reserved2 u32
		reserved3 u32
	}

	struct BareMacTlsImage {
		source       uint
		size         uint
		alignment    uint
		storage_size uint
	}

	struct BareMacTlvDescriptor {
		thunk uint
		key   uint
	mut:
		offset uint
	}

	// Darwin passes these five CRT references to every image initializer.
	struct BareMacProgramVars {
		mh       voidptr
		argc     voidptr
		argv     voidptr
		environ  voidptr
		progname voidptr
	}

	type BareMacInitializer = fn (i32, &&char, &&char, &&char, &BareMacProgramVars)
	type BareMacFinalizer = fn (voidptr)

	__global C._mh_execute_header u8
	__global g_bare_macos_tls_image BareMacTlsImage
	__global g_bare_macos_program_vars BareMacProgramVars
	__global g_bare_macos_finalizers_ready i32
	__global g_bare_macos_exit_started i32

	@[export: 'NXArgc']
	__global g_bare_macos_argc i32
	@[export: 'NXArgv']
	__global g_bare_macos_argv &&char
	@[export: 'environ']
	__global g_bare_macos_environ &&char
	@[export: '__progname']
	__global g_bare_macos_progname &char

	fn C._NSGetArgc() &i32
	fn C._NSGetArgv() &&&char
	fn C._NSGetEnviron() &&&char
	fn C._NSGetProgname() &&char
	fn C._NSGetMachExecuteHeader() &C.mach_header_64
	fn C._Exit(status i32)
	fn C._exit(status i32)

	@[export: '_NSGetArgc'; no_profile; no_trace]
	fn bare_macos_get_argc() &i32 {
		return &g_bare_macos_argc
	}

	@[export: '_NSGetArgv'; no_profile; no_trace]
	fn bare_macos_get_argv() &&&char {
		return &g_bare_macos_argv
	}

	@[export: '_NSGetEnviron'; no_profile; no_trace]
	fn bare_macos_get_environ() &&&char {
		return &g_bare_macos_environ
	}

	@[export: '_NSGetProgname'; no_profile; no_trace]
	fn bare_macos_get_progname() &&char {
		return &g_bare_macos_progname
	}

	@[export: '_NSGetMachExecuteHeader'; no_profile; no_trace]
	fn bare_macos_get_header() &C.mach_header_64 {
		return unsafe { &C.mach_header_64(&C._mh_execute_header) }
	}

	@[no_profile; no_trace]
	fn bare_macos_command_at(address uint, end uint) (&C.load_command, bool) {
		if address > end || end - address < sizeof(C.load_command) {
			return unsafe { nil }, false
		}
		command := unsafe { &C.load_command(address) }
		if command.cmdsize < sizeof(C.load_command) || command.cmdsize & 7 != 0
			|| uint(command.cmdsize) > end - address {
			return unsafe { nil }, false
		}
		if command.cmd == u32(C.LC_SEGMENT_64) {
			if command.cmdsize < sizeof(C.segment_command_64) {
				return unsafe { nil }, false
			}
			segment := unsafe { &C.segment_command_64(address) }
			if uint(segment.nsects) > (uint(segment.cmdsize) - sizeof(C.segment_command_64)) / sizeof(C.section_64) {
				return unsafe { nil }, false
			}
		}
		return command, true
	}

	@[no_profile; no_trace]
	fn bare_macos_tls_prepare() int {
		header := unsafe { &C.mach_header_64(&C._mh_execute_header) }
		if header.magic != u32(C.MH_MAGIC_64) || header.filetype != u32(C.MH_EXECUTE) || header.flags & u32(C.MH_DYLDLINK) != 0 {
			return 22
		}
		begin := uint(header) + sizeof(C.mach_header_64)
		if begin < uint(header) || uint(header.sizeofcmds) > ~uint(0) - begin {
			return 22
		}
		end := begin + uint(header.sizeofcmds)
		mut address := begin
		mut lowest := ~uint(0)
		mut highest := uint(0)
		mut alignment := uint(16)
		for _ in 0 .. int(header.ncmds) {
			command, valid := bare_macos_command_at(address, end)
			if !valid { return 22 }
			if command.cmd == u32(C.LC_SEGMENT_64) {
				segment := unsafe { &C.segment_command_64(address) }
				sections := unsafe { &C.section_64(address + sizeof(C.segment_command_64)) }
				for j in 0 .. int(segment.nsects) {
					section := unsafe { &sections[j] }
					kind := section.flags & u32(C.SECTION_TYPE)
					if (kind != u32(C.S_THREAD_LOCAL_REGULAR) && kind != u32(C.S_THREAD_LOCAL_ZEROFILL)) || section.size == 0 {
						continue
					}
					if section.@align >= sizeof(uint) * 8 || section.addr > ~u64(0) - section.size {
						return 22
					}
					requested := uint(1) << section.@align
					if requested > alignment { alignment = requested }
					if section.addr < lowest { lowest = uint(section.addr) }
					if section.addr + section.size > highest {
						highest = uint(section.addr + section.size)
					}
				}
			}
			address += uint(command.cmdsize)
		}
		if address != end { return 22 }
		if lowest == ~uint(0) {
			lowest = 0
			highest = 0
		}
		if highest < lowest || alignment > uint(max_int) - 2 * sizeof(uint) { return 22 }
		size := highest - lowest
		prefix := 2 * sizeof(uint) + alignment - 1
		if size > uint(max_int) - prefix { return 22 }
		g_bare_macos_tls_image = BareMacTlsImage{lowest, size, alignment, prefix + size}
		// Static TLV descriptors retain template addresses, rather than the
		// offsets used by the access thunk. Normalize them once before use.
		address = begin
		for _ in 0 .. int(header.ncmds) {
			command := unsafe { &C.load_command(address) }
			if command.cmd == u32(C.LC_SEGMENT_64) {
				segment := unsafe { &C.segment_command_64(address) }
				sections := unsafe { &C.section_64(address + sizeof(C.segment_command_64)) }
				for j in 0 .. int(segment.nsects) {
					section := unsafe { &sections[j] }
					if section.flags & u32(C.SECTION_TYPE) != u32(C.S_THREAD_LOCAL_VARIABLES) {
						continue
					}
					if section.size % sizeof(BareMacTlvDescriptor) != 0 { return 22 }
					mut descriptors := unsafe { &BareMacTlvDescriptor(section.addr) }
					for k in 0 .. int(section.size / sizeof(BareMacTlvDescriptor)) {
						if unsafe { descriptors[k].offset < lowest || descriptors[k].offset >= highest } {
							return 22
						}
						unsafe { descriptors[k].offset -= lowest }
					}
				}
			}
			address += uint(command.cmdsize)
		}
		return 0
	}

	@[no_profile; no_trace]
	fn bare_macos_tls_storage_size() uint {
		return g_bare_macos_tls_image.storage_size
	}

	@[no_profile; no_trace]
	fn bare_macos_tls_init_storage(storage voidptr, capacity uint) int {
		image := g_bare_macos_tls_image
		base := uint(storage)
		if base == 0 || image.storage_size == 0 || capacity < image.storage_size
			|| base % sizeof(uint) != 0 || base > ~uint(0) - image.storage_size {
			return 22
		}
		seed := base + 2 * sizeof(uint)
		data := seed + ((image.source - seed) & (image.alignment - 1))
		// TLS gaps and zero-fill sections start at zero. Copy regular sections
		// separately so the template need not have readable mappings in gaps.
		// Optimized memory exports are also safe before TLS is activated.
		unsafe {
			output := &u8(data)
			for i in 0 .. int(image.size) { output[i] = 0 }
			header := &C.mach_header_64(&C._mh_execute_header)
			mut address := uint(header) + sizeof(C.mach_header_64)
			for _ in 0 .. int(header.ncmds) {
				command := &C.load_command(address)
				if command.cmd == u32(C.LC_SEGMENT_64) {
					segment := &C.segment_command_64(address)
					sections := &C.section_64(address + sizeof(C.segment_command_64))
					for j in 0 .. int(segment.nsects) {
						section := &sections[j]
						if section.flags & u32(C.SECTION_TYPE) != u32(C.S_THREAD_LOCAL_REGULAR) || section.size == 0 {
							continue
						}
						source := &u8(section.addr)
						destination := &u8(data + uint(section.addr) - image.source)
						for k in 0 .. int(section.size) { destination[k] = source[k] }
					}
				}
				address += uint(command.cmdsize)
			}
			*&uint(base) = data
			*&uint(base + sizeof(uint)) = base
		}
		return 0
	}

	@[no_profile; no_trace]
	fn bare_macos_tls_activate(pointer voidptr) int {
		if pointer == unsafe { nil } { return 22 }
		_, error := sys_call2(0x3000003, u64(pointer), 0) // thread_set_tsd_base
		return int(error)
	}

	@[_naked; no_profile; no_trace]
	fn bare_macos_tls_current() voidptr {
		asm amd64 raw {
			"movq %gs:8, %rax\n"
			"retq\n"
		}
	}

	@[no_profile; no_trace; noreturn]
	fn bare_macos_immediate_exit(status i32) {
		sys_call1(0x2000001, u64(u32(status)))
		for {}
	}

	@[no_profile; no_trace; noreturn]
	fn bare_macos_startup_failure() {
		message := 'V: unable to initialize standalone macOS runtime\n'
		sys_call3(0x2000004, 2, u64(message.str), u64(message.len))
		bare_macos_immediate_exit(127)
	}

	@[no_profile; no_trace]
	fn bare_macos_run_initializers(argc i32, argv &&char, envp &&char, apple &&char) {
		header := unsafe { &C.mach_header_64(&C._mh_execute_header) }
		// Darwin enumerates every pointer initializer before any offset
		// initializer, even when the offset section precedes it in the image.
		for pass in 0 .. 2 {
			mut address := uint(header) + sizeof(C.mach_header_64)
			for _ in 0 .. int(header.ncmds) {
				command := unsafe { &C.load_command(address) }
				if command.cmd == u32(C.LC_SEGMENT_64) {
					segment := unsafe { &C.segment_command_64(address) }
					sections := unsafe { &C.section_64(address + sizeof(C.segment_command_64)) }
					for j in 0 .. int(segment.nsects) {
						section := unsafe { &sections[j] }
						kind := section.flags & u32(C.SECTION_TYPE)
						if pass == 0 && kind == u32(C.S_MOD_INIT_FUNC_POINTERS) {
							if section.size % sizeof(BareMacInitializer) != 0 {
								bare_macos_startup_failure()
							}
							functions := unsafe { &BareMacInitializer(section.addr) }
							for k in 0 .. int(section.size / sizeof(BareMacInitializer)) {
								unsafe { functions[k](argc, argv, envp, apple, &g_bare_macos_program_vars) }
							}
						} else if pass == 1 && kind == u32(C.S_INIT_FUNC_OFFSETS) {
							if section.size % sizeof(u32) != 0 { bare_macos_startup_failure() }
							offsets := unsafe { &u32(section.addr) }
							for k in 0 .. int(section.size / sizeof(u32)) {
								function := unsafe { BareMacInitializer(uint(header) + uint(offsets[k])) }
								function(argc, argv, envp, apple, &g_bare_macos_program_vars)
							}
						}
					}
				}
				address += uint(command.cmdsize)
			}
		}
	}

	@[no_profile; no_trace]
	fn bare_macos_run_finalizers() {
		header := unsafe { &C.mach_header_64(&C._mh_execute_header) }
		begin := uint(header) + sizeof(C.mach_header_64)
		// The command table is small and borrowed. Reverse traversal avoids an
		// auxiliary array and preserves reverse image/section/function order.
		for i := int(header.ncmds); i > 0; i-- {
			mut address := begin
			for _ in 1 .. i { address += uint(unsafe { &C.load_command(address) }.cmdsize) }
			command := unsafe { &C.load_command(address) }
			if command.cmd != u32(C.LC_SEGMENT_64) { continue }
			segment := unsafe { &C.segment_command_64(address) }
			sections := unsafe { &C.section_64(address + sizeof(C.segment_command_64)) }
			for j := int(segment.nsects); j > 0; j-- {
				section := unsafe { &sections[j - 1] }
				if section.flags & u32(C.SECTION_TYPE) != u32(C.S_MOD_TERM_FUNC_POINTERS) {
					continue
				}
				if section.size % sizeof(BareMacFinalizer) != 0 { bare_macos_startup_failure() }
				functions := unsafe { &BareMacFinalizer(section.addr) }
				for k := int(section.size / sizeof(BareMacFinalizer)); k > 0; k-- {
					unsafe { functions[k - 1](nil) }
				}
			}
		}
	}

	@[export: 'v_macos_bare_exit'; no_profile; no_trace; noreturn]
	fn bare_macos_normal_exit(status i32) {
		if C.v_prealloc_atomic_cas_i32(&g_bare_macos_exit_started, 0, 1) != 0 {
			// Registered callbacks also run when a constructor exits early.
			bare_macos_drain_atexit(false)
			if C.v_prealloc_atomic_load_i32(&g_bare_macos_finalizers_ready) != 0 {
				bare_macos_run_finalizers()
			}
			// Finalizers may have registered additional callbacks. Closing the
			// empty registry rejects concurrent late registration explicitly.
			bare_macos_drain_atexit(true)
		}
		bare_macos_immediate_exit(status)
	}

	@[export: '_Exit'; no_profile; no_trace; noreturn]
	fn bare_macos_exit_immediate(status i32) { bare_macos_immediate_exit(status) }

	@[export: '_exit'; no_profile; no_trace; noreturn]
	fn bare_macos_posix_exit_immediate(status i32) { bare_macos_immediate_exit(status) }

	// Return the apple vector in RAX so the entry point passes native main all
	// four Darwin arguments. Generated V main may consume only argc and argv.
	@[export: 'v_macos_bare_prepare_start'; no_profile; no_trace]
	fn bare_macos_prepare_start(initial_stack &uint) &&char {
		if unsafe { initial_stack[0] } > uint(0x7fffffff) || bare_macos_tls_prepare() != 0 {
			bare_macos_startup_failure()
		}
		mapping, error := sys_call6(0x20000c5, 0, u64(bare_macos_tls_storage_size()), 3, 0x1002, ~u64(0), 0)
		if error != 0 || bare_macos_tls_init_storage(voidptr(mapping), bare_macos_tls_storage_size()) != 0
			|| bare_macos_tls_activate(voidptr(mapping)) != 0 {
			bare_macos_startup_failure()
		}
		unsafe {
			g_bare_macos_argc = i32(initial_stack[0])
			g_bare_macos_argv = &&char(initial_stack + 1)
			g_bare_macos_environ = g_bare_macos_argv + g_bare_macos_argc + 1
			mut apple := g_bare_macos_environ
			for *apple != nil { apple++ }
			apple++
			g_bare_macos_progname = if g_bare_macos_argc > 0 { g_bare_macos_argv[0] } else { c'' }
			mut p := g_bare_macos_progname
			for *p != 0 {
				if *p == `/` { g_bare_macos_progname = p + 1 }
				p++
			}
			g_bare_macos_program_vars = BareMacProgramVars{&C._mh_execute_header, &g_bare_macos_argc, &g_bare_macos_argv, &g_bare_macos_environ, &g_bare_macos_progname}
			bare_macos_run_initializers(g_bare_macos_argc, g_bare_macos_argv, g_bare_macos_environ, apple)
			C.v_prealloc_atomic_store_i32(&g_bare_macos_finalizers_ready, 1)
			return apple
		}
	}

	// TLV thunks preserve every register except RAX, a stronger ABI than C.
	// Defining its entry directly avoids an ordinary function export wrapper.
	asm amd64 raw {
		".section __TEXT,__text,regular,pure_instructions\n"
		".globl _start\n"
		".p2align 4\n"
		"_start:\n"
		"xorq %rbp, %rbp\n"
		"movq %rsp, %rdi\n"
		"andq $-16, %rsp\n"
		"callq _v_macos_bare_prepare_start\n"
		"movq %rax, %rcx\n"
		"movl _NXArgc(%rip), %edi\n"
		"movq _NXArgv(%rip), %rsi\n"
		"movq _environ(%rip), %rdx\n"
		"callq _main\n"
		"movl %eax, %edi\n"
		"callq _v_macos_bare_exit\n"
		"ud2\n"
		".globl __tlv_bootstrap\n"
		".private_extern __tlv_bootstrap\n"
		".p2align 4\n"
		"__tlv_bootstrap:\n"
		"movq %gs:0, %rax\n"
		"addq 16(%rdi), %rax\n"
		"retq\n"
		".subsections_via_symbols\n"
	}
}
