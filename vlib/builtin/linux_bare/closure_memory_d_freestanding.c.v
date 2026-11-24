module builtin

$if !native && (amd64 || arm64) {
	// These private C bridges let the closure module reuse the bare mapping layer
	// without making kernel types part of its public interface.
	@[export: 'v_closure_map'; markused]
	fn bare_closure_map(size uint) voidptr {
		p, code := sys_mmap(unsafe { nil }, u64(size), unsafe { MemProt(3) }, unsafe { MapFlags(0x22) }, -1, 0)
		if code != .enoerror { return unsafe { nil } }
		return p
	}

	@[export: 'v_closure_protect'; markused]
	fn bare_closure_protect(p voidptr, size uint, protection i32) i32 {
		$if amd64 {
			return i32(i64(sys_call3(10, u64(p), u64(size), u64(protection))))
		} $else {
			return i32(i64(sys_call3(226, u64(p), u64(size), u64(protection))))
		}
	}

	@[export: 'v_closure_page_size'; markused]
	fn bare_closure_page_size() uint {
		$if v_bare_no_startup ? {
			return uint(C.sysconf(C._SC_PAGESIZE))
		} $else {
			// The kernel places auxv immediately after the original environment.
			unsafe {
				if g_main_argv == nil { return 0 }
				mut cursor := &uint(&&char(g_main_argv) + g_main_argc + 1)
				for *cursor != 0 { cursor++ }
				cursor++
				for cursor[0] != 0 {
					if cursor[0] == 6 { return cursor[1] } // AT_PAGESZ
					cursor += 2
				}
			}
			return 0
		}
	}
}
