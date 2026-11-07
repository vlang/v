module builtin

// The argv-block environment walkers shared by the ELF bare runtimes (FreeBSD,
// OpenBSD, NetBSD, macOS, Solaris): the bf_environ/bf_getenv of each OS call them
// by name. Only `-freestanding` and `-syscalls` builds reach them.
$if freestanding || syscalls {
	// env_walk helpers walk the argv-block environment: envp starts at
	// argv + (argc + 1) slots (see linux_bare/bf.v for the layout note). Every
	// ELF target's kernel lays this out the same way, so the walkers are shared
	// by name across the bf files.
	fn env_walk_find(key string) (string, bool) {
		unsafe {
			if g_main_argv == nil {
				return '', false
			}
			argv_base := &&u8(g_main_argv)
			mut i := 0
			for {
				entry := argv_base[g_main_argc + 1 + i]
				if entry == nil {
					return '', false
				}
				mut eq := -1
				for {
					c := entry[eq + 1]
					if c == u8(61) {
						eq++
						break
					}
					if c == u8(0) {
						break
					}
					eq++
				}
				if eq == key.len && eq >= 0 {
					mut ok := true
					for k in 0 .. key.len {
						if entry[k] != u8(key[k]) {
							ok = false
							break
						}
					}
					if ok {
						mut vl := 0
						for entry[key.len + 1 + vl] != 0 {
							vl++
						}
						val := (&u8(voidptr(uint(entry) + uint(key.len + 1)))).vstring_with_len(vl)
						return val.clone(), true
					}
				}
				i++
			}
		}
	}

	fn env_walk_to_map() map[string]string {
		mut res := map[string]string{}
		unsafe {
			if g_main_argv == nil {
				return res
			}
			argv_base := &&u8(g_main_argv)
			mut i := 0
			for {
				entry := argv_base[g_main_argc + 1 + i]
				if entry == nil {
					return res
				}
				mut eq := -1
				for {
					c := entry[eq + 1]
					if c == u8(61) {
						eq++
						break
					}
					if c == u8(0) {
						break
					}
					eq++
				}
				if eq > 0 {
					key := (&u8(entry)).vstring_with_len(eq)
					mut vl := 0
					for entry[eq + 1 + vl] != 0 {
						vl++
					}
					val := (&u8(voidptr(uint(entry) + uint(eq + 1)))).vstring_with_len(vl)
					res[key.clone()] = val.clone()
				}
				i++
			}
		}
		return res
	}
}
