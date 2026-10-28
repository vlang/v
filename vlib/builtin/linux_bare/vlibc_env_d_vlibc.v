module builtin

$if !i386 {
	// Read environment via /proc/self/environ (NUL-separated KEY=VALUE).
	// vlibc_getenv returns an environment value and whether it was found.
	pub fn vlibc_getenv(name string) (string, bool) {
		data, e := vlibc_read_file('/proc/self/environ')
		if e != .enoerror {
			return '', false
		}
		mut i := u64(0)
		for i < u64(data.len) {
			// find end of entry
			mut j := i
			for j < u64(data.len) && data[j] != 0 {
				j++
			}
			entry := data[i..j]
			// check prefix "name="
			if u64(name.len) + 1 <= u64(entry.len) {
				mut ok := true
				for k in 0 .. name.len {
					if entry[k] != u8(name[k]) {
						ok = false
						break
					}
				}
				if ok && entry[name.len] == u8(61) { // '='
					mut val := []u8{cap: entry.len - name.len}
					for k in name.len + 1 .. entry.len {
						val << entry[k]
					}
					val << u8(0)
					mut s := ''
					unsafe {
						s = tos_clone(&u8(val.data))
					}
					// tos_clone expects NUL-terminated; ensure
					_ = val
					return s, true
				}
			}
			i = j + 1
		}
		return '', false
	}

	// vlibc_environ_keys returns the names available in the process environment.
	pub fn vlibc_environ_keys() []string {
		data, e := vlibc_read_file('/proc/self/environ')
		if e != .enoerror {
			return []string{}
		}
		mut keys := []string{}
		mut i := u64(0)
		for i < u64(data.len) {
			mut j := i
			for j < u64(data.len) && data[j] != 0 {
				j++
			}
			entry := data[i..j]
			// find '='
			mut eq := -1
			for k in 0 .. entry.len {
				if entry[k] == u8(61) {
					eq = k
					break
				}
			}
			if eq > 0 {
				mut key := []u8{cap: eq + 1}
				for k in 0 .. eq {
					key << entry[k]
				}
				key << u8(0)
				mut s := ''
				unsafe {
					s = tos_clone(&u8(key.data))
				}
				keys << s
			}
			i = j + 1
		}
		return keys
	}
}
