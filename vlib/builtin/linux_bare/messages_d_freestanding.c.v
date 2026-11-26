@[has_globals]
module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	@[heap]
	struct BareTextDomain {
	mut:
		name      uint
		directory uint
		codeset   uint
		next      &BareTextDomain = unsafe { nil }
	}
	__global bare_text_domains &BareTextDomain
	__global bare_text_domain uint
	__global bare_text_mutex u32

	fn bare_message_copy(address uint) uint {
		length := bare_string_chr_nul(address, 0) - address
		copy := unsafe { __malloc(length + 1) }
		if copy == unsafe { nil } { return 0 }
		unsafe { vmemcpy(copy, voidptr(address), int(length + 1)) }
		return uint(copy)
	}
	@[export: 'v_message_domain'; markused]
	fn bare_message_domain(name uint) uint {
		bare_compat_lock(&bare_text_mutex)
		defer { bare_compat_unlock(&bare_text_mutex) }
		if name == 0 {
			return if bare_text_domain == 0 { uint(c'messages') } else { bare_text_domain }
		}
		value := if unsafe { *(&u8(name)) } == 0 { uint(c'messages') } else { name }
		if bare_text_domain != 0 && bare_string_collate(value, bare_text_domain) == 0 {
			return bare_text_domain
		}
		owned := bare_message_copy(value)
		if owned == 0 { return 0 }
		unsafe { __free(voidptr(bare_text_domain)) }
		bare_text_domain = owned
		return owned
	}
	@[export: 'v_message_binding'; markused]
	fn bare_message_binding(name uint, value uint, codeset i32) uint {
		if name == 0 || unsafe { *(&u8(name)) } == 0 {
			bf_native_errno_cell = i32(C.EINVAL)
			return 0
		}
		bare_compat_lock(&bare_text_mutex)
		defer { bare_compat_unlock(&bare_text_mutex) }
		mut entry := bare_text_domains
		for entry != unsafe { nil } && bare_string_collate(entry.name, name) != 0 {
			entry = entry.next
		}
		if value == 0 {
			if entry == unsafe { nil } {
				return if codeset != 0 { uint(0) } else { uint(c'/usr/share/locale') }
			}
			return if codeset != 0 {
				entry.codeset
			} else {
				if entry.directory == 0 { uint(c'/usr/share/locale') } else { entry.directory }
			}
		}
		if entry == unsafe { nil } {
			entry = unsafe { &BareTextDomain(__calloc(1, sizeof(BareTextDomain))) }
			if entry == unsafe { nil } { return 0 }
			entry.name = bare_message_copy(name)
			if entry.name == 0 {
				unsafe { __free(entry) }
				return 0
			}
			entry.next = bare_text_domains
			bare_text_domains = entry
		}
		previous := if codeset != 0 { entry.codeset } else { entry.directory }
		if previous != 0 && bare_string_collate(previous, value) == 0 { return previous }
		owned := bare_message_copy(value)
		if owned == 0 { return 0 }
		if codeset != 0 { entry.codeset = owned } else { entry.directory = owned }
		unsafe { __free(voidptr(previous)) }
		return owned
	}
	// C/POSIX and C.UTF-8, the supported message locales, use source messages.
	@[export: 'v_message_translate'; markused]
	fn bare_message_translate(singular uint, plural uint, count uint) uint {
		return if plural == 0 || count == 1 { singular } else { plural }
	}
}
