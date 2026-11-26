module builtin

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	#include "@DIR/libc_glob_abi.h"
	fn C.v_glob_view(uint, uint, i32, i32)
	fn C.v_glob_assign(uint, uint, i32)
	fn C.v_glob_open(uint, uint) uint
	fn C.v_glob_read(uint, uint, i32) uint
	fn C.v_glob_close(uint, uint)
	fn C.v_glob_stat(uint, uint, i32, i32, uint) i32
	fn C.v_glob_error(uint, uint, i32) i32
	fn C.v_glob_home(uint) uint

	struct BareGlobView {
	mut:
		count    uint
		paths    uint
		offset   uint
		flags    i32
		close_fn uint
		read_fn  uint
		open_fn  uint
		lstat_fn uint
		stat_fn  uint
	}
	struct BareGlobList {
	mut:
		data     uint
		count    uint
		capacity uint
	}
	struct BareGlobWalk {
	mut:
		view    BareGlobView
		matches BareGlobList
		flags   i32
		large   i32
		error   uint
		magic   bool
	}

	fn bare_glob_join(a string, b string, c string) uint {
		length := u64(a.len) + u64(b.len) + u64(c.len)
		if length >= u64(max_int) {
			bf_native_errno_cell = i32(C.ENOMEM)
			return 0
		}
		output := unsafe { __malloc(uint(length + 1)) }
		if output == unsafe { nil } { return 0 }
		// Each input is borrowed; the caller owns this single NUL-terminated copy.
		unsafe {
			vmemcpy(output, a.str, a.len)
			vmemcpy(&u8(output) + a.len, b.str, b.len)
			vmemcpy(&u8(output) + a.len + b.len, c.str, c.len)
			(&u8(output))[int(length)] = 0
		}
		return uint(output)
	}
	fn bare_glob_push(mut list BareGlobList, value uint) bool {
		if list.count == list.capacity {
			if list.capacity > (~uint(0) / sizeof(uint) - 1) / 2 { return false }
			next := if list.capacity == 0 { uint(16) } else { list.capacity * 2 }
			grown := unsafe { __malloc(next * sizeof(uint)) }
			if grown == unsafe { nil } { return false }
			for index := uint(0); index < list.count; index++ {
				unsafe { (&uint(grown))[index] = (&uint(list.data))[index] }
			}
			unsafe { __free(voidptr(list.data)) }
			list.data = uint(grown)
			list.capacity = next
		}
		unsafe { (&uint(list.data))[list.count] = value }
		list.count++
		return true
	}
	fn bare_glob_release(mut list BareGlobList) {
		for index := uint(0); index < list.count; index++ {
			unsafe { __free(voidptr((&uint(list.data))[index])) }
		}
		unsafe { __free(voidptr(list.data)) }
		list = BareGlobList{}
	}
	fn bare_glob_magic(pattern string, flags i32) bool {
		mut index := 0
		for index < pattern.len {
			c := pattern[index]
			if c == `\\` && flags & i32(C.GLOB_NOESCAPE) == 0 && index + 1 < pattern.len {
				index += 2
				continue
			}
			if c == `*` || c == `?` { return true }
			if c == `[` {
				mut end := index + 1
				if end < pattern.len && (pattern[end] == `!` || pattern[end] == `^`) { end++ }
				if end < pattern.len && pattern[end] == `]` { end++ }
				for end < pattern.len {
					if pattern[end] == `]` { return true }
					if pattern[end] == `\\` && flags & i32(C.GLOB_NOESCAPE) == 0 { end++ }
					end++
				}
			}
			index++
		}
		return false
	}
	fn bare_glob_unquote(pattern string, flags i32) uint {
		output := bare_glob_join(pattern, '', '')
		if output == 0 { return 0 }
		if flags & i32(C.GLOB_NOESCAPE) != 0 { return output }
		mut used := 0
		mut index := 0
		for index < pattern.len {
			if pattern[index] == `\\` && index + 1 < pattern.len { index++ }
			unsafe { (&u8(output))[used] = pattern[index] }
			used++
			index++
		}
		unsafe { (&u8(output))[used] = 0 }
		return output
	}
	fn bare_glob_is_dir(mut walk BareGlobWalk, path uint, follow bool) (bool, bool) {
		mut mode := u32(0)
		callback := if follow { walk.view.stat_fn } else { walk.view.lstat_fn }
		code := C.v_glob_stat(callback, path, i32(follow), walk.large, uint(&mode))
		return code == 0, code == 0 && mode & 0xf000 == 0x4000
	}
	fn bare_glob_failure(mut walk BareGlobWalk, path uint) i32 {
		error := bf_native_errno_cell
		if walk.error != 0 && C.v_glob_error(walk.error, path, error) != 0 {
			return i32(C.GLOB_ABORTED)
		}
		return if walk.flags & i32(C.GLOB_ERR) != 0 { i32(C.GLOB_ABORTED) } else { i32(0) }
	}
	fn bare_glob_leaf(mut walk BareGlobWalk, path string, require_directory bool) i32 {
		owned := bare_glob_join(path, '', '')
		if owned == 0 { return i32(C.GLOB_NOSPACE) }
		mut keep := false
		defer { if !keep { unsafe { __free(voidptr(owned)) } } }
		exists, mut directory := bare_glob_is_dir(mut walk, owned, false)
		if !exists { return 0 }
		if require_directory || walk.flags & (i32(C.GLOB_MARK) | i32(C.GLOB_ONLYDIR)) != 0 {
			_, directory = bare_glob_is_dir(mut walk, owned, true)
		}
		if (require_directory || walk.flags & i32(C.GLOB_ONLYDIR) != 0) && !directory { return 0 }
		if directory && walk.flags & i32(C.GLOB_MARK) != 0 && !path.ends_with('/') {
			marked := bare_glob_join(path, '/', '')
			if marked == 0 { return i32(C.GLOB_NOSPACE) }
			if !bare_glob_push(mut walk.matches, marked) {
				unsafe { __free(voidptr(marked)) }
				return i32(C.GLOB_NOSPACE)
			}
			return 0
		}
		if !bare_glob_push(mut walk.matches, owned) { return i32(C.GLOB_NOSPACE) }
		keep = true
		return 0
	}
	fn bare_glob_walk(mut walk BareGlobWalk, prefix string, pattern string) i32 {
		if pattern.len == 0 {
			if prefix.len == 0 { return 0 }
			return bare_glob_leaf(mut walk, prefix, prefix.ends_with('/'))
		}
		mut end := 0
		for end < pattern.len && pattern[end] != `/` { end++ }
		mut component_end := end
		if end < pattern.len && walk.flags & i32(C.GLOB_NOESCAPE) == 0 {
			mut backslashes := 0
			for index := end - 1; index >= 0 && pattern[index] == `\\`; index-- { backslashes++ }
			if backslashes % 2 != 0 { component_end-- }
		}
		component := pattern[..component_end]
		mut next := end
		for next < pattern.len && pattern[next] == `/` { next++ }
		separators := pattern[end..next]
		if !bare_glob_magic(component, walk.flags) {
			part := bare_glob_unquote(component, walk.flags)
			if part == 0 { return i32(C.GLOB_NOSPACE) }
			defer { unsafe { __free(voidptr(part)) } }
			path := bare_glob_join(prefix, unsafe { tos2(&u8(part)) }, separators)
			if path == 0 { return i32(C.GLOB_NOSPACE) }
			defer { unsafe { __free(voidptr(path)) } }
			if end == pattern.len {
				return bare_glob_leaf(mut walk, unsafe { tos2(&u8(path)) }, false)
			}
			return bare_glob_walk(mut walk, unsafe { tos2(&u8(path)) }, pattern[next..])
		}
		walk.magic = true
		directory_path := if prefix.len == 0 { uint(c'.') } else { uint(prefix.str) }
		directory := C.v_glob_open(walk.view.open_fn, directory_path)
		if directory == 0 {
			if bf_native_errno_cell == i32(C.ENOTDIR) {
				return 0
			}
			return bare_glob_failure(mut walk, directory_path)
		}
		defer { C.v_glob_close(walk.view.close_fn, directory) }
		part := bare_glob_join(component, '', '')
		if part == 0 { return i32(C.GLOB_NOSPACE) }
		defer { unsafe { __free(voidptr(part)) } }
		mut match_flags := if walk.flags & i32(C.GLOB_PERIOD) != 0 {
			i32(0)
		} else {
			i32(C.FNM_PERIOD)
		}
		if walk.flags & i32(C.GLOB_NOESCAPE) != 0 { match_flags |= i32(C.FNM_NOESCAPE) }
		for {
			bf_native_errno_cell = 0
			entry := C.v_glob_read(walk.view.read_fn, directory, walk.large)
			if entry == 0 {
				if bf_native_errno_cell != 0 { return bare_glob_failure(mut walk, directory_path) }
				return 0
			}
			matched := bare_pattern_fnmatch(part, entry, match_flags)
			if matched < 0 { return i32(C.GLOB_NOSPACE) }
			if matched != 0 { continue }
			name := unsafe { tos2(&u8(entry)) }
			path := bare_glob_join(prefix, name, separators)
			if path == 0 { return i32(C.GLOB_NOSPACE) }
			code := if end == pattern.len {
				bare_glob_leaf(mut walk, unsafe { tos2(&u8(path)) }, false)
			} else {
				bare_glob_walk(mut walk, unsafe { tos2(&u8(path)) }, pattern[next..])
			}
			unsafe { __free(voidptr(path)) }
			if code != 0 { return code }
		}
		return 0
	}

	// Expand one balanced brace. The explicit work list
	// bounds C stack use for deeply nested brace expressions.
	fn bare_glob_brace(pattern string, flags i32, mut pending BareGlobList) i32 {
		mut start := -1
		mut level := 0
		mut index := 0
		for index < pattern.len {
			c := pattern[index]
			if c == `\\` && flags & i32(C.GLOB_NOESCAPE) == 0 {
				index += 2
				continue
			}
			if c == `{` {
				if level == 0 {
					start = index
				}
				level++
			}
			if c == `}` && level > 0 {
				level--
				if level == 0 {
					before := pending.count
					mut part := start + 1
					mut nested := 0
					mut cursor := part
					for cursor <= index {
						ch := pattern[cursor]
						if ch == `\\` && flags & i32(C.GLOB_NOESCAPE) == 0 {
							cursor += 2
							continue
						}
						if ch == `{` { nested++ }
						if cursor == index || (ch == `,` && nested == 0) {
							expanded := bare_glob_join(pattern[..start], pattern[part..cursor], pattern[index + 1..])
							if expanded == 0 { return -1 }
							if !bare_glob_push(mut pending, expanded) {
								unsafe { __free(voidptr(expanded)) }
								return -1
							}
							part = cursor + 1
						}
						if ch == `}` { nested-- }
						cursor++
					}
					// Pop alternatives in their original order; each group is sorted
					// independently, as if glob were called once per alternative.
					mut left := before
					mut right := pending.count - 1
					for left < right {
						unsafe {
							items := &uint(pending.data)
							value := items[left]
							items[left] = items[right]
							items[right] = value
						}
						left++
						right--
					}
					return 1
				}
			}
			index++
		}
		return 0
	}
	fn bare_glob_tilde(pattern string, flags i32) (uint, bool) {
		if pattern.len == 0 || pattern[0] != `~` || flags & (i32(C.GLOB_TILDE) | i32(C.GLOB_TILDE_CHECK)) == 0 {
			return bare_glob_join(pattern, '', ''), false
		}
		mut end := 1
		for end < pattern.len && pattern[end] != `/` { end++ }
		mut home := if end == 1 { uint(C.getenv(c'HOME')) } else { uint(0) }
		mut entry := uint(0)
		mut buffer := uint(0)
		defer {
			unsafe {
				__free(voidptr(entry))
				__free(voidptr(buffer))
			}
		}
		if home == 0 {
			name := bare_glob_join(pattern[1..end], '', '')
			if name == 0 { return 0, false }
			defer { unsafe { __free(voidptr(name)) } }
			entry = uint(unsafe { __malloc(C.v_account_size(0)) })
			if entry == 0 { return 0, false }
			mut capacity := uint(1024)
			for {
				buffer = uint(unsafe { __malloc(capacity) })
				if buffer == 0 { return 0, false }
				mut found := uint(0)
				code := bare_account_lookup(if end == 1 { uint(0) } else { name }, u32(sys_getuid()), entry, buffer, capacity, uint(&found), 0)
				if code != i32(C.ERANGE) {
					if code == 0 && found != 0 { home = C.v_glob_home(entry) }
					break
				}
				unsafe { __free(voidptr(buffer)) }
				buffer = 0
				if capacity > ~uint(0) / 2 { return 0, false }
				capacity *= 2
			}
		}
		if home == 0 {
			return bare_glob_join(pattern, '', ''), flags & i32(C.GLOB_TILDE_CHECK) != 0
		}
		return bare_glob_join(unsafe { tos2(&u8(home)) }, pattern[end..], ''), false
	}

	fn bare_glob_sift(list uint, start uint, count uint) {
		mut root := start
		for root < count / 2 {
			mut child := root * 2 + 1
			unsafe {
				items := &uint(list)
				if child + 1 < count && bare_string_collate(items[child], items[child + 1]) < 0 {
					child++
				}
				if bare_string_collate(items[root], items[child]) >= 0 { break }
				value := items[root]
				items[root] = items[child]
				items[child] = value
			}
			root = child
		}
	}
	fn bare_glob_sort(list uint, count uint) {
		if count < 2 { return }
		mut index := count / 2
		for index > 0 {
			index--
			bare_glob_sift(list, index, count)
		}
		index = count - 1
		for index > 0 {
			unsafe {
				items := &uint(list)
				value := items[0]
				items[0] = items[index]
				items[index] = value
			}
			bare_glob_sift(list, 0, index)
			index--
		}
	}
	@[export: 'v_pattern_glob'; markused]
	fn bare_glob_execute(pattern uint, flags i32, error uint, output uint, large i32) i32 {
		if flags & ~((1 << 15) - 1) != 0 || pattern == 0 || output == 0 {
			bf_native_errno_cell = i32(C.EINVAL)
			return -1
		}
		mut walk := BareGlobWalk{ flags: flags, error: error, large: large }
		C.v_glob_view(output, uint(&walk.view), large, flags)
		if flags & i32(C.GLOB_ALTDIRFUNC) == 0 {
			walk.view.close_fn = 0
			walk.view.read_fn = 0
			walk.view.open_fn = 0
			walk.view.lstat_fn = 0
			walk.view.stat_fn = 0
		}
		if flags & i32(C.GLOB_APPEND) == 0 {
			walk.view.count = 0
			walk.view.paths = 0
		}
		if flags & i32(C.GLOB_DOOFFS) == 0 { walk.view.offset = 0 }
		walk.view.flags = flags
		C.v_glob_assign(output, uint(&walk.view), large)
		mut pending := BareGlobList{}
		defer {
			bare_glob_release(mut pending)
			bare_glob_release(mut walk.matches)
		}
		first := bare_glob_join(unsafe { tos2(&u8(pattern)) }, '', '')
		if first == 0 { return i32(C.GLOB_NOSPACE) }
		if !bare_glob_push(mut pending, first) {
			unsafe { __free(voidptr(first)) }
			return i32(C.GLOB_NOSPACE)
		}
		mut code := i32(0)
		mut braced := false
		for pending.count > 0 {
			pending.count--
			word := unsafe { (&uint(pending.data))[pending.count] }
			text := unsafe { tos2(&u8(word)) }
			brace := if flags & i32(C.GLOB_BRACE) != 0 {
				bare_glob_brace(text, flags, mut pending)
			} else {
				i32(0)
			}
			if brace != 0 {
				braced = true
				unsafe { __free(voidptr(word)) }
				if brace < 0 {
					code = i32(C.GLOB_NOSPACE)
					break
				}
				continue
			}
			expanded, missing_home := bare_glob_tilde(text, flags)
			unsafe { __free(voidptr(word)) }
			if expanded == 0 {
				code = i32(C.GLOB_NOSPACE)
				break
			}
			before := walk.matches.count
			path := unsafe { tos2(&u8(expanded)) }
			if !missing_home {
				code = bare_glob_walk(mut walk, '', path)
				if !braced && code == 0 && before == walk.matches.count
					&& (flags & i32(C.GLOB_NOCHECK) != 0 || (flags & i32(C.GLOB_NOMAGIC) != 0 && !bare_glob_magic(path, flags))) {
					fallback := bare_glob_join(path, '', '')
					if fallback == 0 {
						code = i32(C.GLOB_NOSPACE)
					} else if !bare_glob_push(mut walk.matches, fallback) {
						unsafe { __free(voidptr(fallback)) }
						code = i32(C.GLOB_NOSPACE)
					}
				}
			}
			unsafe { __free(voidptr(expanded)) }
			if flags & i32(C.GLOB_NOSORT) == 0 && walk.matches.count > before {
				bare_glob_sort(walk.matches.data + before * sizeof(uint), walk.matches.count - before)
			}
			if code != 0 { break }
		}
		if code == 0 && braced && walk.matches.count == 0 && (flags & i32(C.GLOB_NOCHECK) != 0
			|| (flags & i32(C.GLOB_NOMAGIC) != 0 && !bare_glob_magic(unsafe { tos2(&u8(pattern)) }, flags))) {
			fallback := bare_glob_join(unsafe { tos2(&u8(pattern)) }, '', '')
			if fallback == 0 { return i32(C.GLOB_NOSPACE) }
			if !bare_glob_push(mut walk.matches, fallback) {
				unsafe { __free(voidptr(fallback)) }
				return i32(C.GLOB_NOSPACE)
			}
		}
		if walk.matches.count == 0 { return if code != 0 { code } else { i32(C.GLOB_NOMATCH) } }
		limit := ~uint(0) / sizeof(uint) - 1
		if walk.view.offset > limit || walk.view.count > limit - walk.view.offset || walk.matches.count > limit - walk.view.offset - walk.view.count {
			return i32(C.GLOB_NOSPACE)
		}
		total := walk.view.count + walk.matches.count
		paths := uint(unsafe { __calloc(walk.view.offset + total + 1, sizeof(uint)) })
		if paths == 0 { return i32(C.GLOB_NOSPACE) }
		if walk.view.paths != 0 {
			for index := uint(0); index < walk.view.offset + walk.view.count; index++ {
				unsafe { (&uint(paths))[index] = (&uint(walk.view.paths))[index] }
			}
		}
		for index := uint(0); index < walk.matches.count; index++ {
			unsafe { (&uint(paths))[walk.view.offset + walk.view.count + index] = (&uint(walk.matches.data))[index] }
		}
		unsafe { __free(voidptr(walk.view.paths)) }
		walk.matches.count = 0
		walk.view.paths = paths
		walk.view.count = total
		if walk.magic { walk.view.flags |= i32(C.GLOB_MAGCHAR) }
		C.v_glob_assign(output, uint(&walk.view), large)
		return code
	}
	@[export: 'v_pattern_globfree'; markused]
	fn bare_pattern_globfree(output uint, large i32) {
		mut view := BareGlobView{}
		C.v_glob_view(output, uint(&view), large, i32(C.GLOB_APPEND) | i32(C.GLOB_DOOFFS))
		if view.paths != 0 {
			for index := uint(0); index < view.count; index++ {
				unsafe { __free(voidptr((&uint(view.paths))[view.offset + index])) }
			}
			unsafe { __free(voidptr(view.paths)) }
		}
		view.paths = 0
		view.count = 0
		C.v_glob_assign(output, uint(&view), large)
	}
}
