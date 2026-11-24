@[has_globals]
module os

import sync

$if (linux || (macos && amd64)) && !native && !i386 {
	// One allocation owns each changed entry, including its two borrowed string
	// views. Startup entries remain in the kernel-provided argument block.
	struct FSysEnvironmentEntry {
		name    string
		value   string
		removed bool
	mut:
		next &FSysEnvironmentEntry = unsafe { nil }
	}

	__global fsys_environment_mutex sync.Mutex
	__global fsys_environment_entries &FSysEnvironmentEntry
	__global fsys_environment_cleared bool

	// All callers supply bounds obtained from the enclosing string. This is a
	// borrowed view; ordinary string ranges allocate a new NUL-terminated copy.
	@[inline]
	fn fsys_environment_view(text string, start int, end int) string {
		return unsafe { (&char(text.str + start)).vstring_literal_with_len(end - start) }
	}

	// Match the C-string boundary used by the hosted environment APIs.
	@[manualfree]
	fn fsys_environment_prefix(text string) string {
		end := text.index_u8(0)
		return if end >= 0 { fsys_environment_view(text, 0, end) } else { text }
	}

	fn fsys_environment_valid_name(name string) bool {
		return name.len > 0 && name.index_u8(`=`) < 0
	}

	@[manualfree]
	fn fsys_environment_changed(name string) &FSysEnvironmentEntry {
		mut entry := fsys_environment_entries
		for entry != unsafe { nil } {
			if entry.name == name { return entry }
			entry = entry.next
		}
		return unsafe { nil }
	}

	@[manualfree]
	fn fsys_environment_startup_find(name string) (string, bool) {
		startup := bf_startup_environment()
		if startup == unsafe { nil } { return '', false }
		for index := 0; unsafe { startup[index] } != unsafe { nil }; index++ {
			text := unsafe { startup[index].vstring() }
			separator := text.index_u8(`=`)
			if separator > 0 && fsys_environment_view(text, 0, separator) == name {
				return fsys_environment_view(text, separator + 1, text.len), true
			}
		}
		return '', false
	}

	// Return a borrowed value while the environment mutex is held.
	@[manualfree]
	fn fsys_environment_find(name string) (string, bool) {
		entry := fsys_environment_changed(name)
		if entry != unsafe { nil } {
			// Returning a field directly inserts a clone under autofree. This
			// view remains borrowed until the caller releases the mutex.
			return fsys_environment_view(entry.value, 0, entry.value.len), !entry.removed
		}
		if fsys_environment_cleared { return '', false }
		return fsys_environment_startup_find(name)
	}

	@[manualfree]
	fn fsys_environment_get(key string) (string, bool) {
		name := fsys_environment_prefix(key)
		if !fsys_environment_valid_name(name) { return '', false }
		fsys_environment_mutex.lock()
		defer { fsys_environment_mutex.unlock() }
		value, found := fsys_environment_find(name)
		return if found { value.clone() } else { '' }, found
	}

	@[manualfree]
	fn fsys_environment_entry(name string, value string, removed bool) &FSysEnvironmentEntry {
		header := int(sizeof(FSysEnvironmentEntry)) + 2
		if name.len > max_int - header || value.len > max_int - header - name.len {
			return unsafe { nil }
		}
		storage := vcalloc_heap(header + name.len + value.len)
		if storage == unsafe { nil } { return unsafe { nil } }
		unsafe {
			key := storage + sizeof(FSysEnvironmentEntry)
			data := key + name.len + 1
			vmemcpy(key, name.str, name.len)
			vmemcpy(data, value.str, value.len)
			mut entry := &FSysEnvironmentEntry(storage)
			*entry = FSysEnvironmentEntry{
				name:    key.vstring_literal_with_len(name.len)
				value:   data.vstring_literal_with_len(value.len)
				removed: removed
			}
			return entry
		}
	}

	// Publish under the mutex; release the retired block outside it so allocator
	// hooks cannot reenter the environment while its mutex is held.
	@[manualfree]
	fn fsys_environment_publish(mut created FSysEnvironmentEntry) &FSysEnvironmentEntry {
		mut previous := unsafe { &FSysEnvironmentEntry(nil) }
		mut entry := fsys_environment_entries
		for entry != unsafe { nil } {
			if entry.name == created.name {
				created.next = entry.next
				if previous == unsafe { nil } {
					fsys_environment_entries = &created
				} else {
					previous.next = &created
				}
				return entry
			}
			previous = entry
			entry = entry.next
		}
		created.next = fsys_environment_entries
		fsys_environment_entries = &created
		return unsafe { nil }
	}

	@[manualfree]
	fn fsys_environment_remove(name string) &FSysEnvironmentEntry {
		mut previous := unsafe { &FSysEnvironmentEntry(nil) }
		mut entry := fsys_environment_entries
		for entry != unsafe { nil } {
			if entry.name == name {
				if previous == unsafe { nil } {
					fsys_environment_entries = entry.next
				} else {
					previous.next = entry.next
				}
				entry.next = unsafe { nil }
				return entry
			}
			previous = entry
			entry = entry.next
		}
		return unsafe { nil }
	}

	@[manualfree]
	fn fsys_environment_set(key string, text string, overwrite bool) int {
		name := fsys_environment_prefix(key)
		value := fsys_environment_prefix(text)
		if !fsys_environment_valid_name(name) { return -1 }
		fsys_environment_mutex.lock()
		old, found := fsys_environment_find(name)
		ignored := found && (!overwrite || old == value)
		fsys_environment_mutex.unlock()
		if ignored { return 0 }
		mut created := fsys_environment_entry(name, value, false)
		if created == unsafe { nil } { return -1 }
		fsys_environment_mutex.lock()
		if !overwrite {
			_, now_present := fsys_environment_find(name)
			if now_present {
				fsys_environment_mutex.unlock()
				unsafe { vfree_heap(created) }
				return 0
			}
		}
		retired := fsys_environment_publish(mut created)
		fsys_environment_mutex.unlock()
		unsafe { vfree_heap(retired) }
		return 0
	}

	@[manualfree]
	fn fsys_environment_unset(key string) int {
		name := fsys_environment_prefix(key)
		if !fsys_environment_valid_name(name) { return -1 }
		fsys_environment_mutex.lock()
		_, found := fsys_environment_find(name)
		if !found {
			fsys_environment_mutex.unlock()
			return 0
		}
		_, in_startup := if fsys_environment_cleared {
			'', false
		} else {
			fsys_environment_startup_find(name)
		}
		if !in_startup {
			retired := fsys_environment_remove(name)
			fsys_environment_mutex.unlock()
			unsafe { vfree_heap(retired) }
			return 0
		}
		fsys_environment_mutex.unlock()
		mut created := fsys_environment_entry(name, '', true)
		if created == unsafe { nil } { return -1 }
		fsys_environment_mutex.lock()
		retired := fsys_environment_publish(mut created)
		fsys_environment_mutex.unlock()
		unsafe { vfree_heap(retired) }
		return 0
	}

	@[manualfree]
	fn fsys_environment_clear() {
		fsys_environment_mutex.lock()
		mut retired := fsys_environment_entries
		fsys_environment_entries = unsafe { nil }
		fsys_environment_cleared = true
		fsys_environment_mutex.unlock()
		for retired != unsafe { nil } {
			next := retired.next
			unsafe { vfree_heap(retired) }
			retired = next
		}
	}

	@[manualfree]
	fn fsys_environment_all() map[string]string {
		mut result := map[string]string{}
		fsys_environment_mutex.lock()
		defer { fsys_environment_mutex.unlock() }
		if !fsys_environment_cleared {
			startup := bf_startup_environment()
			if startup != unsafe { nil } {
				for index := 0; unsafe { startup[index] } != unsafe { nil }; index++ {
					text := unsafe { startup[index].vstring() }
					separator := text.index_u8(`=`)
					if separator > 0 {
						name := fsys_environment_view(text, 0, separator)
						if fsys_environment_changed(name) != unsafe { nil } { continue }
						// A map clones its string keys; only the value needs an
						// explicit copy out of the borrowed startup block.
						result[name] = fsys_environment_view(text, separator + 1, text.len).clone()
					}
				}
			}
		}
		mut entry := fsys_environment_entries
		for entry != unsafe { nil } {
			if !entry.removed {
				result[entry.name] = entry.value.clone()
			}
			entry = entry.next
		}
		return result
	}
}
