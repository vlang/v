// Copyright (c) 2026 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

module arm64

import os
import time
import crypto.sha256

// Mach-O executable constants
const mh_execute = 2
const lc_load_dylinker = 0xe
const lc_load_dylib = 0xc
const lc_main = u32(0x80000028)
const lc_dyld_info_only = u32(0x80000022)
const lc_dysymtab = 0xb
const lc_uuid = 0x1b
const lc_build_version = 0x32
const lc_source_version = 0x2a
const lc_code_signature = 0x1d

// Code signing constants (big-endian magic numbers)
const csmagic_embedded_signature = u32(0xfade0cc0)
const csmagic_codedirectory = u32(0xfade0c02)
const csmagic_requirements = u32(0xfade0c01)
const csmagic_blobwrapper = u32(0xfade0b01)
const csslot_codedirectory = u32(0)
const csslot_requirements = u32(2)
const csslot_cms_signature = u32(0x10000)
const cs_adhoc = u32(0x2) // Ad-hoc signing flag
const cs_hashtype_sha256 = u8(2)
const cs_hash_size = 32 // SHA256 = 32 bytes
const cs_page_size_arm64 = 16384 // Code signing page size for ARM64 macOS
const cs_page_shift_arm64 = 14 // log2(16384)

// ARM64 page size on macOS
const page_size = 0x4000 // 16KB

// Base address for executables
const base_addr = u64(0x100000000)

// Bind opcodes for dyld
const bind_opcode_done = 0x00
const bind_opcode_set_dylib_ordinal_imm = 0x10
const bind_opcode_set_symbol_flags_imm = 0x40
const bind_opcode_set_type_imm = 0x50
const bind_opcode_set_segment_and_offset_uleb = 0x70
const bind_opcode_do_bind = 0x90
const bind_type_pointer = 1
const bind_symbol_flags_weak_import = 0x01

// vfmt off

// Libc symbols that should ALWAYS resolve to the external system library,
// never to local V wrappers. This prevents infinite recursion where
// V's malloc() wrapper calls C.malloc() which would otherwise resolve
// back to the V wrapper.
const force_external_syms = ['_malloc', '_free', '_calloc', '_realloc', '_exit', '_abort', '_memcpy',
	'_memmove', '_memset', '_memcmp', '___stdoutp', '___stderrp', '_puts', '_printf', '_write',
	'_read', '_open', '_close', '_fwrite', '_fflush', '_fopen', '_fclose', '_putchar', '_sprintf',
	'_snprintf', '_fprintf', '_sscanf', '_mmap', '_munmap', '_getcwd', '_access', '_readlink',
	'_getenv', '_strlen',
	// Filesystem/directory operations
	'_opendir', '_readdir', '_closedir', '_mkdir', '_rmdir',
	'_unlink', '_rename', '_remove', '_stat', '_lstat', '_fstat', '_chmod', '_chdir', '_realpath',
	'_symlink', '_link',
	// Process/system
	'_getpid', '_getuid', '_geteuid', '_fork', '_execve', '_execvp', '_waitpid',
	'_kill', '_system', '_posix_spawn', '_signal', '_atexit',
	// I/O
	'_fgets', '_fputs', '_fread', '_fseek', '_ftell', '_rewind', '_fileno', '_popen',
	'_pclose', '_dup', '_dup2', '_pipe', '_isatty', '_freopen', '_dprintf', '_getc',
	// String/memory
	'_strdup', '_strcmp', '_strncmp', '_strchr', '_strrchr', '_strerror',
	'_strncasecmp', '_strcasecmp', '_atoi', '_atof', '_qsort',
	// Time
	'_time', '_localtime_r', '_gmtime_r', '_mktime', '_gettimeofday',
	'_clock_gettime_nsec_np', '_mach_absolute_time', '_mach_timebase_info', '_nanosleep', '_sleep',
	'_usleep', '_strftime',
	// Other
	'_rand', '_srand', '_isdigit', '_isspace', '_tolower', '_toupper', '_setenv',
	'_unsetenv', '_sysconf', '_uname', '_gethostname', '_pthread_mutex_init', '_pthread_mutex_lock',
	'_pthread_mutex_unlock', '_pthread_mutex_destroy', '_pthread_self', '_arc4random_buf',
	'_proc_pidpath', '_backtrace', '_backtrace_symbols_fd',
	// macOS specific
	'_dispatch_semaphore_create', '_dispatch_semaphore_signal',
	'_dispatch_semaphore_wait', '_dispatch_time', '_dispatch_release', '_setvbuf', '_setbuf',
	'_memchr', '_getlogin_r', '_getppid', '_getgid', '_getegid', '_ftruncate', '_mkstemp', '_statvfs',
	'_chown', '_sigaction', '_sigemptyset', '_sigaddset', '_sigprocmask', '_select', '_kqueue',
	'_abs',
	// Math
	'_cos', '_sin', '_tan', '_acos', '_asin', '_atan', '_atan2',
	'_cosh', '_sinh', '_tanh', '_acosh', '_asinh', '_atanh',
	'_exp', '_exp2', '_log', '_log2', '_log10', '_pow', '_sqrt', '_cbrt',
	'_ceil', '_floor', '_round', '_trunc', '_fmod', '_remainder',
	'_fabs', '_copysign', '_fmax', '_fmin', '_hypot',
	'_ldexp', '_frexp', '_modf', '_scalbn', '_ilogb', '_logb',
	'_erf', '_erfc', '_lgamma', '_tgamma',
	'_j0', '_j1', '_jn', '_y0', '_y1', '_yn']

// vfmt on

pub struct Linker {
	macho &MachOObject
mut:
	// Output buffer
	buf []u8

	// Segment/section info
	text_vmaddr   u64
	text_fileoff  int
	text_size     int
	data_vmaddr   u64
	data_fileoff  int
	data_size     int
	linkedit_off  int
	linkedit_size int

	// External symbols needing binding
	extern_syms []string

	// GOT entries for external symbols
	got_offset int // Offset within __DATA segment
	got_size   int

	// Stubs for external function calls
	stubs_offset int
	stubs_size   int

	// Symbol to GOT index mapping
	sym_to_got map[string]int

	// Code start offset (after header + load commands)
	code_start int
}

pub fn Linker.new(macho &MachOObject) &Linker {
	return unsafe {
		&Linker{
			macho: macho
		}
	}
}

pub fn (mut l Linker) link(output_path string, entry_name string) {
	// Pre-allocate buffer with estimated size to avoid reallocations
	estimated_size := l.macho.text_data.len + l.macho.str_data.len + l.macho.data_data.len + 0x10000
	l.buf = []u8{cap: estimated_size}
	mut t := time.now()
	mut t_total := time.now()

	// First pass: collect all defined symbols (except external ones)
	mut defined_syms := map[string]bool{}
	for sym in l.macho.symbols {
		// N_SECT (0x0E) means symbol is defined in a section
		if (sym.type_ & 0x0E) == 0x0E {
			// Don't track external symbols as defined - they should come from libc
			if sym.name !in force_external_syms {
				defined_syms[sym.name] = true
			}
		}
	}

	// Second pass: collect truly external symbols.
	// force_external_syms should go through GOT/stubs.
	// All other undefined symbols are internal V functions or V-embedded C functions
	// (like wyhash) that resolve to local stubs.
	for sym in l.macho.symbols {
		if sym.name in force_external_syms && sym.name !in l.extern_syms {
			l.extern_syms << sym.name
			l.sym_to_got[sym.name] = l.extern_syms.len - 1
		}
	}

	l.got_size = l.extern_syms.len * 8
	l.stubs_size = l.extern_syms.len * 12 // Each stub is 12 bytes on ARM64

	// Calculate layout
	// On macOS, __TEXT segment MUST start at fileoff 0
	// The header and load commands are inside the __TEXT segment
	n_load_cmds := 14 // Including LC_CODE_SIGNATURE
	pagezero_cmd_size := 72
	text_cmd_size := 72 + (80 * 2) // __text + __stubs
	data_cmd_size := 72 + (80 * 2) // __data + __got
	linkedit_cmd_size := 72
	dyld_info_cmd_size := 48
	symtab_cmd_size := 24
	dysymtab_cmd_size := 80
	dylinker_cmd_size := 32
	dylib_cmd_size := 56
	main_cmd_size := 24
	uuid_cmd_size := 24
	build_version_cmd_size := 24
	source_version_cmd_size := 16
	code_signature_cmd_size := 16

	load_cmds_size := pagezero_cmd_size + text_cmd_size + data_cmd_size + linkedit_cmd_size +
		dyld_info_cmd_size + symtab_cmd_size + dysymtab_cmd_size + dylinker_cmd_size +
		dylib_cmd_size + main_cmd_size + uuid_cmd_size + build_version_cmd_size +
		source_version_cmd_size + code_signature_cmd_size

	// __TEXT starts at file offset 0 and vmaddr base_addr
	l.text_fileoff = 0
	l.text_vmaddr = base_addr

	// Code starts after header + load commands, aligned to 16 bytes
	// Leave ~600 bytes extra for codesign to add LC_CODE_SIGNATURE
	// Header (32) + load_cmds (~700) + codesign reserve (600) ≈ 1332, align to 2048
	header_size := 32
	code_start_min := header_size + load_cmds_size + 600 // Reserve for codesign
	l.code_start = (code_start_min + 15) & ~15 // Align to 16 bytes

	// Calculate where stubs will be (after code and cstrings)
	l.stubs_offset = l.code_start + l.macho.text_data.len + l.macho.str_data.len
	// Align to 4 bytes
	for l.stubs_offset % 4 != 0 {
		l.stubs_offset++
	}

	// Text segment size includes header, load commands, code, cstrings, stubs
	text_content_end := l.stubs_offset + l.stubs_size
	l.text_size = (text_content_end + page_size - 1) & ~(page_size - 1)

	// Data segment follows text
	l.data_fileoff = l.text_size
	l.data_vmaddr = base_addr + u64(l.text_size)

	// GOT offset within data section
	l.got_offset = l.macho.data_data.len
	// Align GOT to 8 bytes
	for l.got_offset % 8 != 0 {
		l.got_offset++
	}

	data_content_size := l.got_offset + l.got_size
	l.data_size = (data_content_size + page_size - 1) & ~(page_size - 1)
	if l.data_size == 0 {
		l.data_size = page_size
	}

	// Write header
	l.write_header(n_load_cmds, load_cmds_size)

	// Write load commands
	l.write_pagezero_segment()
	l.write_text_segment()
	l.write_data_segment()
	linkedit_start := l.buf.len
	l.write_linkedit_segment() // Will patch later

	// Bind info position (in LINKEDIT)
	bind_off := l.data_fileoff + l.data_size
	bind_info := l.generate_bind_info()
	bind_size := bind_info.len

	// Build symbol table for internal function names (visible in objdump -d)
	mut symtab_data := []u8{}
	mut strtab_data := []u8{}
	strtab_data << 0 // First byte of string table must be null

	sym_code_vmaddr := l.text_vmaddr + u64(l.code_start)

	// Find data section base address (minimum symbol value in sect 3)
	mut sym_data_base := u64(0xFFFFFFFFFFFFFFFF)
	for sym in l.macho.symbols {
		if (sym.type_ & 0x0E) == 0x0E && sym.sect == 3 {
			if sym.value < sym_data_base {
				sym_data_base = sym.value
			}
		}
	}
	if sym_data_base == 0xFFFFFFFFFFFFFFFF {
		sym_data_base = u64(l.macho.text_data.len + l.macho.str_data.len)
	}

	for sym in l.macho.symbols {
		if (sym.type_ & 0x0E) != 0x0E {
			continue // Skip undefined symbols
		}
		if sym.name in force_external_syms {
			continue
		}

		mut vm_addr := u64(0)
		mut out_sect := u8(0)
		if sym.sect == 1 {
			// __text section
			vm_addr = sym_code_vmaddr + sym.value
			out_sect = 1
		} else if sym.sect == 3 {
			// __data section
			vm_addr = l.data_vmaddr + (sym.value - sym_data_base)
			out_sect = 3
		} else {
			continue
		}

		str_idx := strtab_data.len
		strtab_data << sym.name.bytes()
		strtab_data << 0

		write_u32_le(mut symtab_data, u32(str_idx)) // n_strx
		symtab_data << sym.type_ // n_type
		symtab_data << out_sect // n_sect
		write_u16_le(mut symtab_data, sym.desc) // n_desc
		write_u64_le(mut symtab_data, vm_addr) // n_value
	}

	// Symbol table follows bind info and must be aligned in LINKEDIT.
	symtab_unaligned_off := bind_off + bind_size
	symtab_off := (symtab_unaligned_off + 7) & ~7
	symtab_pad := symtab_off - symtab_unaligned_off
	n_syms := symtab_data.len / 16
	strtab_off := symtab_off + symtab_data.len
	strtab_size := strtab_data.len

	// Code signature follows string table and should be aligned in LINKEDIT.
	code_limit_unaligned := strtab_off + strtab_size
	cs_off := (code_limit_unaligned + 15) & ~15
	cs_pad := cs_off - code_limit_unaligned
	// code_limit is where the signature starts (everything before is hashed)
	code_limit := cs_off
	// Signature size: SuperBlob(12) + 2*BlobIndex(8) + CodeDirectory header + identifier + hashes + Requirements blob
	ident := output_path.all_after_last('/') // Use filename as identifier
	cs_size := l.estimate_signature_size(code_limit, ident)

	l.linkedit_off = bind_off
	l.linkedit_size = bind_size + symtab_pad + symtab_data.len + strtab_size + cs_pad + cs_size

	l.write_dyld_info(bind_off, bind_size)
	l.write_symtab(symtab_off, n_syms, strtab_off, strtab_size)
	l.write_dysymtab(n_syms)
	l.write_load_dylinker()
	l.write_load_dylib()

	// Find entry point
	entry_off := l.find_entry_offset(entry_name)
	l.write_main_cmd(entry_off)

	l.write_uuid()
	l.write_build_version()
	l.write_source_version()

	// Write LC_CODE_SIGNATURE (will be at cs_off with size cs_size)
	codesig_cmd_start := l.buf.len
	l.write_code_signature_cmd(cs_off, cs_size)

	// Patch LINKEDIT segment with actual values (including signature)
	l.patch_linkedit(linkedit_start, bind_off, l.linkedit_size)

	println('  headers+cmds: ${time.since(t)}')
	t = time.now()

	// Pad to code start (after header + load commands)
	l.pad_to(l.code_start)

	// Write text section with relocations applied
	l.write_text_with_relocations()

	println('  text+relocs: ${time.since(t)}')
	t = time.now()

	// Write cstring section
	l.buf << l.macho.str_data

	// Pad and write stubs
	l.pad_to(l.stubs_offset)
	l.write_stubs()

	// Pad to data start
	l.pad_to(l.data_fileoff)

	// Write data section
	l.buf << l.macho.data_data

	// Pad to GOT offset and write GOT (initially zeros, dyld will fill)
	l.pad_to(l.data_fileoff + l.got_offset)
	l.write_zeros(l.extern_syms.len * 8)

	// Pad data segment
	l.pad_to(l.data_fileoff + l.data_size)

	// Write LINKEDIT content
	l.buf << bind_info
	l.write_zeros(symtab_pad)

	// Write symbol table nlist entries
	l.buf << symtab_data

	// Write string table
	l.buf << strtab_data

	// Align code signature start in LINKEDIT.
	l.write_zeros(cs_pad)

	println('  padding+data: ${time.since(t)}')
	t = time.now()

	// Generate and write code signature (ad-hoc signing)
	signature := l.generate_code_signature(ident)
	l.buf << signature

	// Patch LC_CODE_SIGNATURE if size differs from estimate
	actual_cs_size := signature.len
	if actual_cs_size != cs_size {
		// Patch datasize in LC_CODE_SIGNATURE command
		write_u32_le_at(mut l.buf, codesig_cmd_start + 12, u32(actual_cs_size))
	}

	println('  codesign: ${time.since(t)}')
	t = time.now()

	os.write_file_array(output_path, l.buf) or { panic(err) }

	println('  file write: ${time.since(t)}')

	// Make executable
	os.chmod(output_path, 0o755) or {}
	l.codesign_output(output_path)

	println('  TOTAL linker: ${time.since(t_total)}')
}

fn (l Linker) codesign_output(output_path string) {
	// Re-sign with system codesign to ensure valid signature for large binaries.
	// Our built-in ad-hoc signature works for small binaries but has issues with
	// large (30MB+) executables that cause dyld to hang.
	os.execute('codesign -s - -f ${output_path}')
}

fn (mut l Linker) write_header(ncmds int, cmdsize int) {
	write_u32_le(mut l.buf, mh_magic_64)
	write_u32_le(mut l.buf, u32(cpu_type_arm64))
	write_u32_le(mut l.buf, u32(cpu_subtype_arm64_all))
	write_u32_le(mut l.buf, mh_execute)
	write_u32_le(mut l.buf, u32(ncmds))
	write_u32_le(mut l.buf, u32(cmdsize))
	write_u32_le(mut l.buf, 0x00200085) // MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE
	write_u32_le(mut l.buf, 0) // reserved
}

fn (mut l Linker) write_pagezero_segment() {
	write_u32_le(mut l.buf, u32(lc_segment_64))
	write_u32_le(mut l.buf, 72)
	write_string_fixed(mut l.buf, '__PAGEZERO', 16)
	write_u64_le(mut l.buf, 0) // vmaddr
	write_u64_le(mut l.buf, base_addr) // vmsize
	write_u64_le(mut l.buf, 0) // fileoff
	write_u64_le(mut l.buf, 0) // filesize
	write_u32_le(mut l.buf, 0) // maxprot
	write_u32_le(mut l.buf, 0) // initprot
	write_u32_le(mut l.buf, 0) // nsects
	write_u32_le(mut l.buf, 0) // flags
}

fn (mut l Linker) write_text_segment() {
	write_u32_le(mut l.buf, u32(lc_segment_64))
	write_u32_le(mut l.buf, 72 + 80 * 2) // cmd size with 2 sections
	write_string_fixed(mut l.buf, '__TEXT', 16)
	write_u64_le(mut l.buf, l.text_vmaddr) // vmaddr = base_addr
	write_u64_le(mut l.buf, u64(l.text_size)) // vmsize
	write_u64_le(mut l.buf, 0) // fileoff MUST be 0
	write_u64_le(mut l.buf, u64(l.text_size)) // filesize
	write_u32_le(mut l.buf, 5) // maxprot (r-x)
	write_u32_le(mut l.buf, 5) // initprot (r-x)
	write_u32_le(mut l.buf, 2) // nsects
	write_u32_le(mut l.buf, 0) // flags

	// __text section (code starts at code_start offset)
	write_string_fixed(mut l.buf, '__text', 16)
	write_string_fixed(mut l.buf, '__TEXT', 16)
	write_u64_le(mut l.buf, l.text_vmaddr + u64(l.code_start)) // addr
	write_u64_le(mut l.buf, u64(l.macho.text_data.len)) // size
	write_u32_le(mut l.buf, u32(l.code_start)) // offset
	write_u32_le(mut l.buf, 4) // align (16 bytes = 2^4)
	write_u32_le(mut l.buf, 0) // reloff
	write_u32_le(mut l.buf, 0) // nreloc
	write_u32_le(mut l.buf, 0x80000400) // flags: S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS
	write_u32_le(mut l.buf, 0) // reserved1
	write_u32_le(mut l.buf, 0) // reserved2
	write_u32_le(mut l.buf, 0) // reserved3

	// __stubs section - using regular code section flags since we use immediate binding
	write_string_fixed(mut l.buf, '__stubs', 16)
	write_string_fixed(mut l.buf, '__TEXT', 16)
	write_u64_le(mut l.buf, l.text_vmaddr + u64(l.stubs_offset)) // addr
	write_u64_le(mut l.buf, u64(l.stubs_size)) // size
	write_u32_le(mut l.buf, u32(l.stubs_offset)) // offset
	write_u32_le(mut l.buf, 2) // align
	write_u32_le(mut l.buf, 0) // reloff
	write_u32_le(mut l.buf, 0) // nreloc
	write_u32_le(mut l.buf, 0x80000400) // S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS (no S_SYMBOL_STUBS)
	write_u32_le(mut l.buf, 0) // reserved1
	write_u32_le(mut l.buf, 0) // reserved2
	write_u32_le(mut l.buf, 0) // reserved3
}

fn (mut l Linker) write_data_segment() {
	write_u32_le(mut l.buf, u32(lc_segment_64))
	write_u32_le(mut l.buf, 72 + 80 * 2) // cmd size with 2 sections
	write_string_fixed(mut l.buf, '__DATA', 16)
	write_u64_le(mut l.buf, l.data_vmaddr) // vmaddr
	write_u64_le(mut l.buf, u64(l.data_size)) // vmsize
	write_u64_le(mut l.buf, u64(l.data_fileoff)) // fileoff
	write_u64_le(mut l.buf, u64(l.data_size)) // filesize
	write_u32_le(mut l.buf, 3) // maxprot (rw-)
	write_u32_le(mut l.buf, 3) // initprot (rw-)
	write_u32_le(mut l.buf, 2) // nsects
	write_u32_le(mut l.buf, 0) // flags

	// __data section
	write_string_fixed(mut l.buf, '__data', 16)
	write_string_fixed(mut l.buf, '__DATA', 16)
	write_u64_le(mut l.buf, l.data_vmaddr) // addr
	write_u64_le(mut l.buf, u64(l.macho.data_data.len)) // size
	write_u32_le(mut l.buf, u32(l.data_fileoff)) // offset
	write_u32_le(mut l.buf, 3) // align (8 bytes = 2^3)
	write_u32_le(mut l.buf, 0) // reloff
	write_u32_le(mut l.buf, 0) // nreloc
	write_u32_le(mut l.buf, 0) // flags
	write_u32_le(mut l.buf, 0) // reserved1
	write_u32_le(mut l.buf, 0) // reserved2
	write_u32_le(mut l.buf, 0) // reserved3

	// __got section - using regular data section since we use immediate binding via bind info
	write_string_fixed(mut l.buf, '__got', 16)
	write_string_fixed(mut l.buf, '__DATA', 16)
	write_u64_le(mut l.buf, l.data_vmaddr + u64(l.got_offset)) // addr
	write_u64_le(mut l.buf, u64(l.got_size)) // size
	write_u32_le(mut l.buf, u32(l.data_fileoff + l.got_offset)) // offset
	write_u32_le(mut l.buf, 3) // align
	write_u32_le(mut l.buf, 0) // reloff
	write_u32_le(mut l.buf, 0) // nreloc
	write_u32_le(mut l.buf, 0x00) // S_REGULAR (no special flags - dyld will fill via bind info)
	write_u32_le(mut l.buf, 0) // reserved1
	write_u32_le(mut l.buf, 0) // reserved2
	write_u32_le(mut l.buf, 0) // reserved3
}

fn (mut l Linker) write_linkedit_segment() {
	write_u32_le(mut l.buf, u32(lc_segment_64))
	write_u32_le(mut l.buf, 72)
	write_string_fixed(mut l.buf, '__LINKEDIT', 16)
	write_u64_le(mut l.buf, 0) // vmaddr - patched later
	write_u64_le(mut l.buf, 0) // vmsize - patched later
	write_u64_le(mut l.buf, 0) // fileoff - patched later
	write_u64_le(mut l.buf, 0) // filesize - patched later
	write_u32_le(mut l.buf, 1) // maxprot (r--)
	write_u32_le(mut l.buf, 1) // initprot (r--)
	write_u32_le(mut l.buf, 0) // nsects
	write_u32_le(mut l.buf, 0) // flags
}

fn (mut l Linker) patch_linkedit(cmd_start int, fileoff int, filesize int) {
	linkedit_vmaddr := l.data_vmaddr + u64(l.data_size)
	mut linkedit_vmsize := u64((filesize + page_size - 1) & ~(page_size - 1))
	if linkedit_vmsize == 0 {
		linkedit_vmsize = u64(page_size)
	}

	// Patch vmaddr, vmsize, fileoff, filesize at known offsets within LINKEDIT cmd
	off := cmd_start + 8 + 16 // after cmd, cmdsize, segname
	write_u64_le_at(mut l.buf, off, linkedit_vmaddr)
	write_u64_le_at(mut l.buf, off + 8, linkedit_vmsize)
	write_u64_le_at(mut l.buf, off + 16, u64(fileoff))
	write_u64_le_at(mut l.buf, off + 24, u64(filesize))
}

fn (mut l Linker) write_dyld_info(bind_off int, bind_size int) {
	write_u32_le(mut l.buf, u32(lc_dyld_info_only))
	write_u32_le(mut l.buf, 48)
	write_u32_le(mut l.buf, 0) // rebase_off
	write_u32_le(mut l.buf, 0) // rebase_size
	write_u32_le(mut l.buf, u32(bind_off)) // bind_off
	write_u32_le(mut l.buf, u32(bind_size)) // bind_size
	write_u32_le(mut l.buf, 0) // weak_bind_off
	write_u32_le(mut l.buf, 0) // weak_bind_size
	write_u32_le(mut l.buf, 0) // lazy_bind_off
	write_u32_le(mut l.buf, 0) // lazy_bind_size
	write_u32_le(mut l.buf, 0) // export_off
	write_u32_le(mut l.buf, 0) // export_size
}

fn (mut l Linker) write_symtab(symoff int, nsyms int, stroff int, strsize int) {
	write_u32_le(mut l.buf, u32(lc_symtab))
	write_u32_le(mut l.buf, 24)
	write_u32_le(mut l.buf, u32(symoff))
	write_u32_le(mut l.buf, u32(nsyms))
	write_u32_le(mut l.buf, u32(stroff))
	write_u32_le(mut l.buf, u32(strsize))
}

fn (mut l Linker) write_dysymtab(_nsyms int) {
	write_u32_le(mut l.buf, u32(lc_dysymtab))
	write_u32_le(mut l.buf, 80)
	write_u32_le(mut l.buf, 0) // ilocalsym
	write_u32_le(mut l.buf, 0) // nlocalsym
	write_u32_le(mut l.buf, 0) // iextdefsym
	write_u32_le(mut l.buf, 0) // nextdefsym
	write_u32_le(mut l.buf, 0) // iundefsym
	write_u32_le(mut l.buf, 0) // nundefsym
	write_u32_le(mut l.buf, 0) // tocoff
	write_u32_le(mut l.buf, 0) // ntoc
	write_u32_le(mut l.buf, 0) // modtaboff
	write_u32_le(mut l.buf, 0) // nmodtab
	write_u32_le(mut l.buf, 0) // extrefsymoff
	write_u32_le(mut l.buf, 0) // nextrefsyms
	write_u32_le(mut l.buf, 0) // indirectsymoff
	write_u32_le(mut l.buf, 0) // nindirectsyms
	write_u32_le(mut l.buf, 0) // extreloff
	write_u32_le(mut l.buf, 0) // nextrel
	write_u32_le(mut l.buf, 0) // locreloff
	write_u32_le(mut l.buf, 0) // nlocrel
}

fn (mut l Linker) write_load_dylinker() {
	write_u32_le(mut l.buf, u32(lc_load_dylinker))
	write_u32_le(mut l.buf, 32)
	write_u32_le(mut l.buf, 12) // offset to string
	write_string_fixed(mut l.buf, '/usr/lib/dyld', 20)
}

fn (mut l Linker) write_load_dylib() {
	write_u32_le(mut l.buf, u32(lc_load_dylib))
	write_u32_le(mut l.buf, 56)
	write_u32_le(mut l.buf, 24) // offset to string
	write_u32_le(mut l.buf, 0) // timestamp
	write_u32_le(mut l.buf, 0x10000) // current version
	write_u32_le(mut l.buf, 0x10000) // compatibility version
	write_string_fixed(mut l.buf, '/usr/lib/libSystem.B.dylib', 32)
}

fn (mut l Linker) write_main_cmd(entry_off int) {
	write_u32_le(mut l.buf, u32(lc_main))
	write_u32_le(mut l.buf, 24)
	write_u64_le(mut l.buf, u64(entry_off)) // entryoff (offset from __TEXT start)
	write_u64_le(mut l.buf, 0) // stacksize
}

fn (mut l Linker) write_uuid() {
	write_u32_le(mut l.buf, u32(lc_uuid))
	write_u32_le(mut l.buf, 24)
	// Random UUID
	for _ in 0 .. 16 {
		l.buf << 0
	}
}

fn (mut l Linker) write_build_version() {
	write_u32_le(mut l.buf, u32(lc_build_version))
	write_u32_le(mut l.buf, 24)
	write_u32_le(mut l.buf, 1) // platform: MACOS
	write_u32_le(mut l.buf, 0x000b0000) // minos: 11.0.0
	write_u32_le(mut l.buf, 0x000b0000) // sdk: 11.0.0
	write_u32_le(mut l.buf, 0) // ntools
}

fn (mut l Linker) write_source_version() {
	write_u32_le(mut l.buf, u32(lc_source_version))
	write_u32_le(mut l.buf, 16)
	write_u64_le(mut l.buf, 0) // version
}

fn (mut l Linker) write_code_signature_cmd(dataoff int, datasize int) {
	write_u32_le(mut l.buf, u32(lc_code_signature))
	write_u32_le(mut l.buf, 16) // cmdsize
	write_u32_le(mut l.buf, u32(dataoff))
	write_u32_le(mut l.buf, u32(datasize))
}

fn (l Linker) estimate_signature_size(code_limit int, ident string) int {
	// Calculate pages using ARM64 16KB page size
	n_pages := (code_limit + cs_page_size_arm64 - 1) / cs_page_size_arm64

	// SuperBlob header (12) + 3 BlobIndex entries (24)
	// + CodeDirectory + Requirements blob + CMS blob
	ident_len := ident.len + 1 // null terminated
	n_special_slots := 2
	special_hashes_size := n_special_slots * cs_hash_size
	// CodeDirectory: header (88 for version 0x20400) + ident + special hashes + code hashes
	cd_size := 88 + ident_len + special_hashes_size + (n_pages * cs_hash_size)
	// Round up to 4-byte alignment
	cd_size_aligned := (cd_size + 3) & ~3
	// Requirements blob: minimal empty requirements (12 bytes)
	req_size := 12
	// CMS blob: empty wrapper (8 bytes)
	cms_size := 8
	// Total: SuperBlob(12) + 3*BlobIndex(8) + CodeDirectory + Requirements + CMS
	return 12 + 24 + cd_size_aligned + req_size + cms_size
}

fn (l Linker) generate_code_signature(ident string) []u8 {
	mut sig := []u8{}

	// Calculate sizes using ARM64 16KB pages
	code_limit := l.buf.len // Current buffer is the code to hash
	n_pages := (code_limit + cs_page_size_arm64 - 1) / cs_page_size_arm64
	ident_bytes := ident.bytes()
	ident_len := ident_bytes.len + 1 // null terminated

	// Special slots: we need at least slot for requirements (-2)
	n_special_slots := 2 // Slots -1 (info.plist) and -2 (requirements)
	special_hashes_size := n_special_slots * cs_hash_size

	// CodeDirectory layout for version 0x20400:
	// - Base header (44 bytes): magic, length, version, flags, hashOffset, identOffset,
	//   nSpecialSlots, nCodeSlots, codeLimit, hashSize, hashType, platform, pageSize, spare2
	// - scatterOffset (4 bytes)
	// - teamOffset (4 bytes)
	// - spare3 (4 bytes)
	// - codeLimit64 (8 bytes)
	// - execSegBase (8 bytes)
	// - execSegLimit (8 bytes)
	// - execSegFlags (8 bytes)
	// Total header: 88 bytes
	cd_header_size := 88
	ident_offset := cd_header_size
	hash_offset := ident_offset + ident_len + special_hashes_size
	cd_size := hash_offset + (n_pages * cs_hash_size)
	cd_size_aligned := (cd_size + 3) & ~3

	// Requirements blob (empty)
	req_size := 12
	// CMS signature blob (empty wrapper for ad-hoc)
	cms_size := 8

	// SuperBlob layout with 3 blobs
	blob_count := 3 // CodeDirectory + Requirements + CMS
	super_blob_header := 12
	blob_index_size := blob_count * 8

	cd_blob_offset := super_blob_header + blob_index_size
	req_blob_offset := cd_blob_offset + cd_size_aligned
	cms_blob_offset := req_blob_offset + req_size
	total_size := cms_blob_offset + cms_size

	// Write SuperBlob header (big-endian)
	write_u32_be(mut sig, csmagic_embedded_signature)
	write_u32_be(mut sig, u32(total_size))
	write_u32_be(mut sig, u32(blob_count))

	// BlobIndex for CodeDirectory (type = 0 = CSSLOT_CODEDIRECTORY)
	write_u32_be(mut sig, csslot_codedirectory)
	write_u32_be(mut sig, u32(cd_blob_offset))

	// BlobIndex for Requirements (type = 2 = CSSLOT_REQUIREMENTS)
	write_u32_be(mut sig, csslot_requirements)
	write_u32_be(mut sig, u32(req_blob_offset))

	// BlobIndex for CMS signature (type = 0x10000)
	write_u32_be(mut sig, csslot_cms_signature)
	write_u32_be(mut sig, u32(cms_blob_offset))

	// Write CodeDirectory (big-endian) - version 0x20400
	write_u32_be(mut sig, csmagic_codedirectory)
	write_u32_be(mut sig, u32(cd_size))
	write_u32_be(mut sig, 0x20400) // version
	write_u32_be(mut sig, cs_adhoc) // flags (ad-hoc)
	write_u32_be(mut sig, u32(hash_offset)) // hashOffset
	write_u32_be(mut sig, u32(ident_offset)) // identOffset
	write_u32_be(mut sig, u32(n_special_slots)) // nSpecialSlots
	write_u32_be(mut sig, u32(n_pages)) // nCodeSlots
	write_u32_be(mut sig, u32(code_limit)) // codeLimit
	sig << cs_hash_size // hashSize
	sig << cs_hashtype_sha256 // hashType
	sig << 0 // platform
	sig << cs_page_shift_arm64 // pageSize (log2 of 16384 = 14)
	write_u32_be(mut sig, 0) // spare2
	// Version 0x20400 additional fields:
	write_u32_be(mut sig, 0) // scatterOffset (0 = none)
	write_u32_be(mut sig, 0) // teamOffset (0 = none)
	write_u32_be(mut sig, 0) // spare3
	write_u64_be(mut sig, 0) // codeLimit64 (0 = use codeLimit)
	write_u64_be(mut sig, 0) // execSegBase (0 = __TEXT starts at 0)
	write_u64_be(mut sig, u64(l.text_size)) // execSegLimit (size of __TEXT segment)
	write_u64_be(mut sig, 1) // execSegFlags (CS_EXECSEG_MAIN_BINARY = 1)

	// Write identifier (null-terminated)
	sig << ident_bytes
	sig << 0

	// Write special slot hashes (slots -2, -1 in that order)
	// Slot -2: Requirements hash (we'll compute it from our empty requirements blob)
	// Slot -1: Info.plist hash (zeros for no Info.plist)

	// First, create the requirements blob to hash it
	mut req_blob := []u8{}
	write_u32_be(mut req_blob, csmagic_requirements)
	write_u32_be(mut req_blob, u32(req_size))
	write_u32_be(mut req_blob, 0) // count = 0

	// Slot -2: Hash of requirements blob
	req_hash := sha256.sum(req_blob)
	sig << req_hash

	// Slot -1: Info.plist (zeros = no Info.plist)
	for _ in 0 .. cs_hash_size {
		sig << 0
	}

	// Compute and write page hashes (16KB pages)
	for page := 0; page < n_pages; page++ {
		start := page * cs_page_size_arm64
		mut end := start + cs_page_size_arm64
		if end > code_limit {
			end = code_limit
		}
		hash := sha256.sum(l.buf[start..end])
		sig << hash
	}

	// Pad CodeDirectory to alignment
	for sig.len < cd_blob_offset + cd_size_aligned {
		sig << 0
	}

	// Write Requirements blob
	sig << req_blob

	// Write empty CMS signature blob (for ad-hoc signing)
	write_u32_be(mut sig, csmagic_blobwrapper)
	write_u32_be(mut sig, u32(cms_size))

	return sig
}

fn (mut l Linker) find_entry_offset(entry_name string) int {
	// Find the _main symbol
	// LC_MAIN entryoff is relative to __TEXT segment vmaddr
	// Code section starts at code_start within __TEXT
	for sym in l.macho.symbols {
		if sym.name == entry_name && sym.sect == 1 {
			return l.code_start + int(sym.value)
		}
	}
	return l.code_start // Default to start of code section
}

fn (mut l Linker) generate_bind_info() []u8 {
	mut info := []u8{}

	// Data segment index (segment 2: __PAGEZERO=0, __TEXT=1, __DATA=2)
	data_seg_idx := u8(2)

	for i, sym_name in l.extern_syms {
		// Internal runtime callback names can appear as unresolved function refs in
		// bootstrap builds. Bind them as weak imports so dyld does not abort load
		// when they are absent from libSystem; unresolved weak symbols become NULL.
		mut bind_flags := u8(0)
		if sym_name.contains('__') && sym_name !in force_external_syms {
			bind_flags = bind_symbol_flags_weak_import
		}

		// Set dylib ordinal (1 = first dylib = libSystem)
		info << (bind_opcode_set_dylib_ordinal_imm | 1)

		// Set symbol name
		info << (bind_opcode_set_symbol_flags_imm | bind_flags)
		info << sym_name.bytes()
		info << 0 // null terminator

		// Set type (pointer)
		info << (bind_opcode_set_type_imm | bind_type_pointer)

		// Set segment and offset
		got_entry_offset := l.got_offset + (i * 8)
		info << (bind_opcode_set_segment_and_offset_uleb | data_seg_idx)
		info << l.encode_uleb128(u64(got_entry_offset))

		// Do bind
		info << bind_opcode_do_bind
	}

	// Done
	info << bind_opcode_done

	return info
}

fn (l Linker) encode_uleb128(val u64) []u8 {
	mut result := []u8{}
	mut v := val
	for {
		mut b := u8(v & 0x7f)
		v >>= 7
		if v != 0 {
			b |= 0x80
		}
		result << b
		if v == 0 {
			break
		}
	}
	return result
}

fn (mut l Linker) write_text_with_relocations() {
	// Copy text data
	mut text := l.macho.text_data.clone()

	// Build symbol address map
	// Note: code section vmaddr = text_vmaddr + code_start
	// Symbol values are offsets from segment start, so we use code_vmaddr for all __TEXT symbols
	code_vmaddr := l.text_vmaddr + u64(l.code_start)
	stubs_vmaddr := l.text_vmaddr + u64(l.stubs_offset)

	// In the object file, __data section starts at text_len + cstring_len + alignment_padding
	// We need to find the actual base address of data symbols (minimum symbol value in sect 3)
	// to correctly compute offsets within the data_data array
	mut data_base_addr := u64(0xFFFFFFFFFFFFFFFF) // Start with max, find minimum
	for sym in l.macho.symbols {
		if (sym.type_ & 0x0E) == 0x0E && sym.sect == 3 {
			if sym.value < data_base_addr {
				data_base_addr = sym.value
			}
		}
	}
	// If no data symbols, use section start
	if data_base_addr == 0xFFFFFFFFFFFFFFFF {
		data_base_addr = u64(l.macho.text_data.len + l.macho.str_data.len)
	}

	mut sym_addrs := map[int]u64{}
	// Map symbol names to their defined addresses (for resolving undefined references)
	mut sym_name_to_addr := map[string]u64{}

	// First pass: collect all defined symbol addresses (except external syms)
	for i, sym in l.macho.symbols {
		// N_SECT (0x0E) means symbol is defined in a section
		if (sym.type_ & 0x0E) == 0x0E {
			// Skip external symbols - they should always resolve to libc
			is_external := sym.name in force_external_syms
			if sym.sect == 1 {
				// Text section symbol (code)
				addr := code_vmaddr + sym.value
				sym_addrs[i] = addr
				if !is_external {
					sym_name_to_addr[sym.name] = addr
				}
			} else if sym.sect == 2 {
				// Cstring section symbol
				addr := code_vmaddr + sym.value
				sym_addrs[i] = addr
				if !is_external {
					sym_name_to_addr[sym.name] = addr
				}
			} else if sym.sect == 3 {
				// Data section symbol
				// Subtract data base address to get offset within data_data array
				addr := l.data_vmaddr + (sym.value - data_base_addr)
				sym_addrs[i] = addr
				if !is_external {
					sym_name_to_addr[sym.name] = addr
				}
			}
		}
	}

	// Second pass: handle external symbols and resolve undefined references to local symbols
	for i, sym in l.macho.symbols {
		if sym.type_ == 0x01 { // N_UNDF | N_EXT
			// Check if this symbol is defined locally
			if addr := sym_name_to_addr[sym.name] {
				// Resolve to local definition
				sym_addrs[i] = addr
			} else if sym.name in l.sym_to_got {
				// External symbol - address is in stub
				got_idx := l.sym_to_got[sym.name]
				sym_addrs[i] = stubs_vmaddr + u64(got_idx * 12)
			}
		}
	}

	// Apply relocations
	for r in l.macho.relocs {
		// Check if this relocation references an external symbol
		// If so, redirect it to use the stub instead of the local definition
		sym_name := l.macho.symbols[r.sym_idx].name
		mut sym_addr := sym_addrs[r.sym_idx]
		if sym_addr == 0 && r.sym_idx !in sym_addrs {
			eprintln('LINKER: unresolved symbol "${sym_name}" (idx=${r.sym_idx}) at text offset ${r.addr}')
			// Redirect to return-zero stub (___unresolved_stub) generated by ARM64 codegen
			if stub_addr := sym_name_to_addr['___unresolved_stub'] {
				sym_addr = stub_addr
			}
		}
		if sym_name in force_external_syms {
			// Use stub address for external symbols
			if sym_name in l.sym_to_got {
				got_idx := l.sym_to_got[sym_name]
				sym_addr = stubs_vmaddr + u64(got_idx * 12)
			}
		}
		pc := code_vmaddr + u64(r.addr)

		match r.type_ {
			arm64_reloc_branch26 {
				// BL instruction: PC-relative branch
				if sym_addr == 0 {
					eprintln('LINKER: unresolved BL to "${sym_name}" (idx=${r.sym_idx}) at text offset ${r.addr}')
					// Redirect to return-zero stub (___unresolved_stub) generated by ARM64 codegen
					if stub_addr := sym_name_to_addr['___unresolved_stub'] {
						sym_addr = stub_addr
					}
				}
				rel := i64(sym_addr) - i64(pc)
				imm26 := (rel >> 2) & 0x3FFFFFF
				instr := read_u32_le(text, r.addr)
				new_instr := (instr & 0xFC000000) | u32(imm26)
				write_u32_le_at_arr(mut text, r.addr, new_instr)
			}
			arm64_reloc_page21 {
				// ADRP instruction: PC-relative page address
				sym_page := i64(sym_addr) & ~0xFFF
				pc_page := i64(pc) & ~0xFFF
				page_off := (sym_page - pc_page) >> 12

				immlo := u32(page_off & 0x3) << 29
				immhi := u32((page_off >> 2) & 0x7FFFF) << 5
				instr := read_u32_le(text, r.addr)
				new_instr := (instr & 0x9F00001F) | immlo | immhi
				write_u32_le_at_arr(mut text, r.addr, new_instr)
			}
			arm64_reloc_pageoff12 {
				// ADD/LDR instruction: page offset
				page_off := sym_addr & 0xFFF
				instr := read_u32_le(text, r.addr)

				// Check if this is ADD or LDR
				if (instr & 0xFF800000) == 0x91000000 {
					// ADD immediate
					new_instr := (instr & 0xFFC003FF) | (u32(page_off) << 10)
					write_u32_le_at_arr(mut text, r.addr, new_instr)
				} else {
					// LDR with scaled offset
					// Determine scale from instruction encoding
					scale := (instr >> 30) & 0x3
					scaled_off := page_off >> scale
					new_instr := (instr & 0xFFC003FF) | (u32(scaled_off) << 10)
					write_u32_le_at_arr(mut text, r.addr, new_instr)
				}
			}
			arm64_reloc_got_load_page21 {
				// ADRP instruction: PC-relative page address to GOT entry
				got_idx1 := l.sym_to_got[sym_name] or { 0 }
				got_entry_addr1 := l.data_vmaddr + u64(l.got_offset) + u64(got_idx1 * 8)

				got_page := i64(got_entry_addr1) & ~0xFFF
				pc_page := i64(pc) & ~0xFFF
				page_off := (got_page - pc_page) >> 12

				immlo := u32(page_off & 0x3) << 29
				immhi := u32((page_off >> 2) & 0x7FFFF) << 5
				instr := read_u32_le(text, r.addr)
				new_instr := (instr & 0x9F00001F) | immlo | immhi
				write_u32_le_at_arr(mut text, r.addr, new_instr)
			}
			arm64_reloc_got_load_pageoff12 {
				// LDR instruction: page offset to GOT entry
				got_idx2 := l.sym_to_got[sym_name] or { 0 }
				got_entry_addr2 := l.data_vmaddr + u64(l.got_offset) + u64(got_idx2 * 8)

				page_off := got_entry_addr2 & 0xFFF
				instr := read_u32_le(text, r.addr)
				// LDR with scaled offset (8-byte scale for 64-bit load)
				scaled_off := page_off >> 3
				new_instr := (instr & 0xFFC003FF) | (u32(scaled_off) << 10)
				write_u32_le_at_arr(mut text, r.addr, new_instr)
			}
			else {}
		}
	}

	l.buf << text
}

fn (mut l Linker) write_stubs() {
	// Generate stub for each external symbol
	// Each stub: ADRP x16, GOT@PAGE; LDR x16, [x16, GOT@PAGEOFF]; BR x16
	for i, _ in l.extern_syms {
		got_entry_addr := l.data_vmaddr + u64(l.got_offset) + u64(i * 8)
		stub_addr := l.text_vmaddr + u64(l.stubs_offset) + u64(i * 12)

		// ADRP x16, got_entry@PAGE
		got_page := i64(got_entry_addr) & ~0xFFF
		stub_page := i64(stub_addr) & ~0xFFF
		page_off := (got_page - stub_page) >> 12
		immlo := u32(page_off & 0x3) << 29
		immhi := u32((page_off >> 2) & 0x7FFFF) << 5
		adrp := u32(0x90000010) | immlo | immhi
		write_u32_le(mut l.buf, adrp)

		// LDR x16, [x16, got_entry@PAGEOFF]
		pageoff := (got_entry_addr & 0xFFF) >> 3 // Scale by 8 for 64-bit load
		ldr := u32(0xF9400210) | (u32(pageoff) << 10)
		write_u32_le(mut l.buf, ldr)

		// BR x16
		write_u32_le(mut l.buf, 0xD61F0200)
	}
}

fn read_u32_le(data []u8, off int) u32 {
	return u32(data[off]) | (u32(data[off + 1]) << 8) | (u32(data[off + 2]) << 16) | (u32(data[
		off + 3]) << 24)
}

fn write_u32_le_at_arr(mut data []u8, off int, v u32) {
	data[off] = u8(v)
	data[off + 1] = u8(v >> 8)
	data[off + 2] = u8(v >> 16)
	data[off + 3] = u8(v >> 24)
}

fn write_u32_le_at(mut data []u8, off int, v u32) {
	data[off] = u8(v)
	data[off + 1] = u8(v >> 8)
	data[off + 2] = u8(v >> 16)
	data[off + 3] = u8(v >> 24)
}

// Big-endian write for code signature (Mach-O signatures use big-endian)
fn write_u32_be(mut b []u8, v u32) {
	b << u8(v >> 24)
	b << u8(v >> 16)
	b << u8(v >> 8)
	b << u8(v)
}

fn write_u64_be(mut b []u8, v u64) {
	b << u8(v >> 56)
	b << u8(v >> 48)
	b << u8(v >> 40)
	b << u8(v >> 32)
	b << u8(v >> 24)
	b << u8(v >> 16)
	b << u8(v >> 8)
	b << u8(v)
}

fn write_u64_le_at(mut b []u8, off int, v u64) {
	b[off] = u8(v)
	b[off + 1] = u8(v >> 8)
	b[off + 2] = u8(v >> 16)
	b[off + 3] = u8(v >> 24)
	b[off + 4] = u8(v >> 32)
	b[off + 5] = u8(v >> 40)
	b[off + 6] = u8(v >> 48)
	b[off + 7] = u8(v >> 56)
}

// Pad buffer to target size with zeros (efficient bulk write)
fn (mut l Linker) pad_to(target int) {
	if l.buf.len >= target {
		return
	}
	count := target - l.buf.len
	unsafe { l.buf.grow_len(count) }
}

// Write n zero bytes (efficient)
fn (mut l Linker) write_zeros(n int) {
	if n <= 0 {
		return
	}
	unsafe { l.buf.grow_len(n) }
}
