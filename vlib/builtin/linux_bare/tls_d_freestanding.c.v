@[has_globals]
module builtin

// The C compiler/linker provide the ELF TLS image. Initialize its standard
// local-exec ABI before calling generated V code, without using the V heap or
// a per-thread lookup table. Dynamic-library hosts initialize their own TLS.
$if !native && (amd64 || arm64) {
	#include "@VEXEROOT/vlib/builtin/linux_bare/tls.h"
	__global C.__ehdr_start u8

	struct BareElf64Header {
		ident     [16]u8
		typ       u16
		machine   u16
		version   u32
		entry     u64
		phoff     u64
		shoff     u64
		flags     u32
		ehsize    u16
		phentsize u16
		phnum     u16
		shentsize u16
		shnum     u16
		shstrndx  u16
	}

	struct BareElf64ProgramHeader {
		typ       u32
		flags     u32
		offset    u64
		vaddr     u64
		paddr     u64
		filesz    u64
		memsz     u64
		alignment u64
	}

	struct BareTlsImage {
		data      voidptr
		filesz    uint
		memsz     uint
		alignment uint
		// x86-64 places the image before TP; AArch64 places it after TP.
		offset       uint
		mapping_size uint
	}

	struct BareTlsAllocation {
		mapping      voidptr
		mapping_size uint
		pointer      voidptr
	}

	__global g_bare_tls_image BareTlsImage
	__global g_bare_tls_ready bool

	// bare_tls_layout validates arithmetic before mapping or copying an ELF image.
	fn bare_tls_layout(data voidptr, filesz uint, memsz uint, requested_align uint) (BareTlsImage, bool) {
		alignment := if requested_align == 0 { uint(1) } else { requested_align }
		if filesz > memsz || alignment & (alignment - 1) != 0 {
			return BareTlsImage{}, false
		}
		mask := alignment - 1
		image_mod := uint(data) & mask
		mut offset := uint(0)
		mut size := uint(0)
		$if amd64 {
			// Preserve PT_TLS's first-byte alignment, including a nonzero
			// p_vaddr modulo p_align. TP itself is aligned to p_align.
			padding := (alignment - ((image_mod + (memsz & mask)) & mask)) & mask
			if memsz > ~uint(0) - padding { return BareTlsImage{}, false }
			offset = memsz + padding
			if offset > ~uint(0) - 16 { return BareTlsImage{}, false }
			size = offset + 16
		} $else {
			// AArch64 reserves two pointer words at TP (ELF TLS variant I).
			padding := (image_mod + alignment - (uint(16) & mask)) & mask
			if padding > ~uint(0) - 16 { return BareTlsImage{}, false }
			offset = 16 + padding
			if memsz > ~uint(0) - offset { return BareTlsImage{}, false }
			size = offset + memsz
		}
		fixed_align := if alignment < 16 { uint(16) } else { alignment }
		if size > uint(max_int) - (fixed_align - 1) { return BareTlsImage{}, false }
		return BareTlsImage{
			data:         data
			filesz:       filesz
			memsz:        memsz
			alignment:    fixed_align
			offset:       offset
			mapping_size: size + fixed_align - 1
		}, true
	}

	fn bare_tls_allocate() (BareTlsAllocation, Errno) {
		if !g_bare_tls_ready { return BareTlsAllocation{}, .einval }
		image := g_bare_tls_image
		mapping, err := sys_mmap(unsafe { nil }, u64(image.mapping_size),
			unsafe { MemProt(3) }, unsafe { MapFlags(0x22) }, -1, 0)
		if err != .enoerror { return BareTlsAllocation{}, err }
		base := uint(mapping)
		if base > ~uint(0) - image.mapping_size {
			unsafe { sys_munmap(mapping, u64(image.mapping_size)) }
			return BareTlsAllocation{}, .einval
		}
		mut tp := uint(0)
		$if amd64 {
			tp = (base + image.offset + image.alignment - 1) & ~(image.alignment - 1)
		} $else {
			tp = (base + image.alignment - 1) & ~(image.alignment - 1)
		}
		mut destination := tp + image.offset
		$if amd64 {
			destination = tp - image.offset
		}
		// Anonymous mappings supply the zero-filled .tbss tail. Copy only .tdata.
		unsafe {
			if image.filesz != 0 {
				source := &u8(image.data)
				output := &u8(destination)
				for i in 0 .. int(image.filesz) { output[i] = source[i] }
			}
			$if amd64 {
				// The local-exec x86-64 ABI reads its thread pointer from FS:0.
				*&uint(tp) = tp
			}
		}
		return BareTlsAllocation{
			mapping:      mapping
			mapping_size: image.mapping_size
			pointer:      voidptr(tp)
		}, .enoerror
	}

	@[noreturn]
	fn bare_tls_startup_failure() {
		message := 'V: unable to initialize freestanding TLS\n'
		$if amd64 {
			sys_call3(1, 2, u64(message.str), u64(message.len))
			sys_call1(231, 127)
		} $else {
			sys_call3(64, 2, u64(message.str), u64(message.len))
			sys_call1(94, 127)
		}
		for {}
	}

	// bare_tls_init is called only by the standalone Linux _start. A host's
	// loader owns TLS for a shared library and for every thread it creates.
	@[export: 'v_bare_tls_init'; markused]
	fn bare_tls_init(initial_stack voidptr) {
		mut phdr := uint(0)
		mut phent := uint(0)
		mut phnum := uint(0)
		unsafe {
			stack := &uint(initial_stack)
			argc := stack[0]
			if argc > uint(max_int) / sizeof(uint) - 2 { bare_tls_startup_failure() }
			mut cursor := stack + argc + 2
			for *cursor != 0 { cursor++ }
			cursor++
			for cursor[0] != 0 {
				match cursor[0] {
					3 { phdr = cursor[1] } // AT_PHDR
					4 { phent = cursor[1] } // AT_PHENT
					5 { phnum = cursor[1] } // AT_PHNUM
					else {}
				}
				cursor += 2
			}
		}
		if phdr == 0 || phent != sizeof(BareElf64ProgramHeader) || phnum == 0
			|| phnum > uint(max_int) / phent {
			bare_tls_startup_failure()
		}
		header := unsafe { &BareElf64Header(&C.__ehdr_start) }
		if header.ident[0] != 0x7f || header.ident[1] != `E` || header.ident[2] != `L`
			|| header.ident[3] != `F` || header.ident[4] != 2 || header.ident[5] != 1
			|| header.phnum != phnum || header.phentsize != phent
			|| uint(header) > ~uint(0) - uint(header.phoff)
			|| uint(header) + uint(header.phoff) != phdr {
			bare_tls_startup_failure()
		}
		mut bias := uint(0)
		mut found_bias := false
		mut tls := unsafe { &BareElf64ProgramHeader(nil) }
		for i in 0 .. int(phnum) {
			entry := unsafe { &BareElf64ProgramHeader(phdr + uint(i) * phent) }
			if entry.typ == 1 && entry.offset == 0 { // PT_LOAD containing ELF header
				if uint(header) < entry.vaddr { bare_tls_startup_failure() }
				bias = uint(header) - uint(entry.vaddr)
				found_bias = true
			}
			if entry.typ == 7 { tls = entry } // PT_TLS
		}
		if !found_bias { bare_tls_startup_failure() }
		mut image := BareTlsImage{}
		mut valid := false
		if tls == unsafe { nil } {
			image, valid = bare_tls_layout(unsafe { nil }, 0, 0, 1)
		} else {
			if tls.vaddr > ~uint(0) - bias { bare_tls_startup_failure() }
			image, valid = bare_tls_layout(voidptr(bias + uint(tls.vaddr)),
				uint(tls.filesz), uint(tls.memsz), uint(tls.alignment))
		}
		if !valid { bare_tls_startup_failure() }
		g_bare_tls_image = image
		g_bare_tls_ready = true
		allocation, err := bare_tls_allocate()
		if err != .enoerror { bare_tls_startup_failure() }
		$if amd64 {
			// Linux ARCH_SET_FS installs the local-exec thread pointer.
			if i64(sys_call2(158, 0x1002, u64(allocation.pointer))) < 0 {
				bare_tls_startup_failure()
			}
		} $else {
			pointer := allocation.pointer
			asm volatile arm64 {
				msr tpidr_el0, pointer
				isb
				; ; r (pointer)
				; memory
			}
		}
	}
}
