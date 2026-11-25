@[has_globals]
module time

$if linux && (amd64 || arm64) && !v_bare_no_startup ? {
	import sync

	#include "@DIR/../builtin/linux_bare/libc_time_abi.h"
	#include "@DIR/../builtin/linux_bare/libc_environment.h"
	#include <errno.h>
	#include <stdlib.h>
	#include <sys/syscall.h>

	fn C.v_time_tm_set(uint, uint, i64, uint)
	fn C.v_time_tm_get(uint, uint)
	fn C.v_time_tm_static(i32) uint
	fn C.v_time_text_static(i32) uint
	fn C.v_time_names_set(i64, i32, uint, uint)
	fn C.v_time_errno_get() i32
	fn C.v_time_errno_set(i32)
	fn C.v_time_clocks_per_second() uint
	fn C.v_freestanding_environment_lock()
	fn C.v_freestanding_environment_unlock()
	fn C.v_freestanding_environment_get_locked(&char) &char
	fn C.malloc(uint) voidptr
	fn C.free(voidptr)
	fn C.getauxval(uint) uint

	@[heap]
	struct CTimeName {
	mut:
		value string
		next  &CTimeName = unsafe { nil }
	}
	@[heap]
	struct CTimeLocation {
	mut:
		tz        string
		directory string
		has_tz    bool
		location  &Location = unsafe { nil }
		arena     voidptr
		next      &CTimeLocation = unsafe { nil }
	}

	__global c_time_mutex sync.Mutex
	__global c_time_names &CTimeName
	__global c_time_location &CTimeLocation
	__global c_time_retained &CTimeLocation

	fn c_time_copy(text string) (string, bool) {
		pointer := C.malloc(uint(text.len + 1))
		if pointer == unsafe { nil } { return '', false }
		unsafe {
			vmemcpy(pointer, text.str, text.len)
			(&u8(pointer))[text.len] = 0
		}
		return unsafe { tos(&u8(pointer), text.len) }, true
	}
	fn c_time_intern(text string) uint {
		mut node := c_time_names
		for node != unsafe { nil } {
			if node.value == text { return uint(node.value.str) }
			node = node.next
		}
		copy, copied := c_time_copy(text)
		if !copied { return 0 }
		mut entry := unsafe { &CTimeName(C.malloc(sizeof(CTimeName))) }
		if entry == unsafe { nil } {
			C.free(copy.str)
			return 0
		}
		entry.value = copy
		entry.next = c_time_names
		c_time_names = entry
		return uint(copy.str)
	}
	fn c_time_environment(name &char) (string, bool, bool) {
		C.v_freestanding_environment_lock()
		defer { C.v_freestanding_environment_unlock() }
		value := C.v_freestanding_environment_get_locked(name)
		if value == unsafe { nil } { return '', false, true }
		text := unsafe { tos2(&u8(value)) }
		copy, copied := c_time_copy(text)
		return copy, true, copied
	}
	fn c_time_read_location(tz string, present bool, directory string) !&Location {
		if !present {
			data := zoneinfo_read_file('/etc/localtime')!
			return parse_tzif_location('Local', data)
		}
		if tz == '' { return load_location('UTC') }
		name := if tz.starts_with(':') { tz[1..] } else { tz }
		if name.starts_with('/') || name.starts_with('.') {
			if C.getauxval(23) != 0 && name != '/etc/localtime' { return load_location('UTC') }
			data := zoneinfo_read_file(name)!
			return parse_tzif_location('Local', data)
		}
		if !name.contains('..') {
			if directory != '' {
				if data := zoneinfo_read_file(zoneinfo_join(directory, name)) {
					return parse_tzif_location(name, data)
				}
			}
			for base in zoneinfo_unix_sources {
				if data := zoneinfo_read_file(zoneinfo_join(base, name)) {
					return parse_tzif_location(name, data)
				}
			}
		}
		if rule := parse_posix_zone_rule(name) { return location_from_posix_rule(name, rule) }
		return load_location('UTC')
	}
	// Called with c_time_mutex held. A dedicated arena owns parsed TZif strings,
	// transitions and all parser temporaries, independently of the caller's arena.
	fn c_time_get_location() ?&Location {
		tz, present, copied := c_time_environment(c'TZ')
		if !copied { return none }
		dir, has_dir, dir_copied := c_time_environment(c'TZDIR')
		if !dir_copied {
			if present { C.free(tz.str) }
			return none
		}
		defer {
			if present { C.free(tz.str) }
			if has_dir { C.free(dir.str) }
		}
		if c_time_location != unsafe { nil } && c_time_location.tz == tz
			&& c_time_location.has_tz == present && c_time_location.directory == dir {
			return c_time_location.location
		}
		mut arena := unsafe { nil }
		mut loc := unsafe { &Location(nil) }
		$if !prealloc && !no_builtin_arena ? {
			arena = v_arena_try_new(16384)
			if arena == unsafe { nil } { return none }
			selection := v_arena_scope_enter(arena)
			loc = c_time_read_location(tz, present, dir) or { load_location('UTC') or { panic(err) } }
			v_arena_scope_leave(&selection)
		} $else {
			// Without allocator scopes, retain parsed locations by configuration.
			// This also keeps abbreviation pointers valid for native C callers.
			mut old := c_time_retained
			for old != unsafe { nil } {
				if old.tz == tz && old.has_tz == present && old.directory == dir {
					c_time_location = old
					return old.location
				}
				old = old.next
			}
			loc = c_time_read_location(tz, present, dir) or { load_location('UTC') or { panic(err) } }
		}
		owned_tz, tz_copied := c_time_copy(tz)
		owned_dir, directory_copied := c_time_copy(dir)
		mut entry := unsafe { &CTimeLocation(C.malloc(sizeof(CTimeLocation))) }
		if !tz_copied || !directory_copied || entry == unsafe { nil } {
			if tz_copied { C.free(owned_tz.str) }
			if directory_copied { C.free(owned_dir.str) }
			C.free(entry)
			$if !prealloc && !no_builtin_arena ? {
				v_arena_free(arena)
			}
			return none
		}
		entry.tz = owned_tz
		entry.directory = owned_dir
		entry.has_tz = present
		entry.location = loc
		entry.arena = arena
		entry.next = unsafe { nil }
		$if !prealloc && !no_builtin_arena ? {
			old := c_time_location
			if old != unsafe { nil } {
				v_arena_free(old.arena)
				C.free(old.tz.str)
				C.free(old.directory.str)
				C.free(old)
			}
		} $else {
			entry.next = c_time_retained
			c_time_retained = entry
		}
		c_time_location = entry
		return loc
	}
	fn c_time_fill(epoch i64, output uint, loc &Location) bool {
		mut offset := 0
		mut dst := false
		mut zone := uint(c'UTC')
		if loc != unsafe { nil } {
			active := loc.zone_at(epoch) or {
				C.v_time_errno_set(i32(C.EOVERFLOW))
				return false
			}
			offset = active.offset
			dst = active.is_dst
			zone = c_time_intern(active.name)
			if zone == 0 {
				C.v_time_errno_set(i32(C.ENOMEM))
				return false
			}
		}
		if (offset > 0 && epoch > bare_max_epoch - i64(offset)) || (offset < 0 && epoch < bare_min_epoch - i64(offset)) {
			C.v_time_errno_set(i32(C.EOVERFLOW))
			return false
		}
		value := unix(epoch + i64(offset))
		if value.year - 1900 < -2147483648 || value.year - 1900 > 2147483647 {
			C.v_time_errno_set(i32(C.EOVERFLOW))
			return false
		}
		fields := [i32(value.year - 1900), i32(value.month - 1), i32(value.day), i32(value.hour),
			i32(value.minute), i32(value.second), i32(value.day_of_week() % 7),
			i32(value.year_day() - 1), i32(dst)]!
		C.v_time_tm_set(output, uint(&fields[0]), i64(offset), zone)
		return true
	}
	@[export: 'v_time_now'; markused]
	fn c_time_now(output uint) i64 {
		seconds, _, code := bf_realtime_now()
		if code != 0 {
			C.v_time_errno_set(i32(code))
			return -1
		}
		if output != 0 { unsafe { *(&i64(output)) = seconds } }
		return seconds
	}
	@[export: 'v_time_gmtime'; markused]
	fn c_time_gmtime(input uint, output uint) uint {
		return if c_time_fill(unsafe { *(&i64(input)) }, output, unsafe { nil }) {
			output
		} else {
			uint(0)
		}
	}
	@[export: 'v_time_localtime'; markused]
	fn c_time_localtime(input uint, output uint) uint {
		return c_time_localtime_with_names(input, output, false)
	}
	fn c_time_localtime_with_names(input uint, output uint, publish bool) uint {
		c_time_mutex.lock()
		defer { c_time_mutex.unlock() }
		loc := c_time_get_location() or {
			C.v_time_errno_set(i32(C.ENOMEM))
			return 0
		}
		if publish { c_time_publish_zone(loc) }
		return if c_time_fill(unsafe { *(&i64(input)) }, output, loc) { output } else { uint(0) }
	}
	@[export: 'v_time_localtime_default'; markused]
	fn c_time_localtime_default(input uint, output uint) uint {
		return c_time_localtime_with_names(input, output, true)
	}
	@[export: 'v_time_ctime_default'; markused]
	fn c_time_ctime_default(input uint, output uint) uint {
		mut calendar := C.tm{}
		if c_time_localtime_default(input, uint(&calendar)) == 0 { return 0 }
		return c_time_asctime(uint(&calendar), output)
	}

	@[export: 'v_time_local_offset'; markused]
	fn c_time_local_offset(epoch i64) i64 {
		c_time_mutex.lock()
		defer { c_time_mutex.unlock() }
		loc := c_time_get_location() or { return 0 }
		return i64(loc.offset_at(epoch) or { 0 })
	}
	fn c_time_wall(fields [9]i32) i64 {
		years, month := floor_div_mod(i64(fields[1]), 12)
		base := Time{ year: int(i64(fields[0]) + 1900 + years), month: int(month + 1), day: 1 }
		return time_fields_to_unix(base) + (i64(fields[2]) - 1) * 86400 + i64(fields[3]) * 3600 + i64(fields[4]) * 60 + i64(fields[5])
	}
	@[export: 'v_time_mktime'; markused]
	fn c_time_mktime(address uint, local i32) i64 {
		mut fields := [9]i32{}
		C.v_time_tm_get(address, uint(&fields[0]))
		wall := c_time_wall(fields)
		mut epoch := wall
		if local != 0 {
			c_time_mutex.lock()
			defer { c_time_mutex.unlock() }
			loc := c_time_get_location() or {
				C.v_time_errno_set(i32(C.ENOMEM))
				return -1
			}
			c_time_publish_zone(loc)
			epoch = bare_local_wall_to_unix(unix(wall), loc)
			if fields[8] >= 0 {
				active := loc.zone_at(epoch) or {
					C.v_time_errno_set(i32(C.EOVERFLOW))
					return -1
				}
				if active.is_dst != (fields[8] != 0) {
					mut found := false
					for shift in [-183 * i64(86400), 183 * i64(86400), -366 * i64(86400),
						366 * i64(86400)]! {
						candidate := loc.zone_at(epoch + shift) or { continue }
						if candidate.is_dst == (fields[8] != 0) {
							epoch = wall - candidate.offset
							found = true
							break
						}
					}
					if !found {
						// With no seasonal counterpart, an explicit DST hint uses
						// the conventional one-hour adjustment, then normalizes tm.
						adjustment := if fields[8] != 0 { i64(3600) } else { i64(-3600) }
						epoch = wall - i64(active.offset) - adjustment
					}
				}
			}
			if !c_time_fill(epoch, address, loc) { return -1 }
		} else if !c_time_fill(epoch, address, unsafe { nil }) {
			return -1
		}
		return epoch
	}
	@[export: 'v_time_tzset'; markused]
	fn c_time_tzset() {
		saved := C.v_time_errno_get()
		c_time_mutex.lock()
		defer {
			c_time_mutex.unlock()
			C.v_time_errno_set(saved)
		}
		loc := c_time_get_location() or { return }
		c_time_publish_zone(loc)
	}
	fn c_time_publish_zone(loc &Location) {
		mut standard := Zone{ name: 'UTC' }
		mut summer := standard
		mut has_dst := false
		for zone in loc.zones {
			if zone.is_dst {
				summer = zone
				has_dst = true
			} else {
				standard = zone
			}
		}
		if loc.has_posix {
			standard = Zone{ name: loc.posix.std_name, offset: loc.posix.std_offset }
			if loc.posix.has_dst {
				summer = Zone{ name: loc.posix.dst_name, offset: loc.posix.dst_offset, is_dst: true }
				has_dst = true
			}
		}
		if !has_dst { summer = standard }
		standard_name := c_time_intern(standard.name)
		summer_name := c_time_intern(summer.name)
		if standard_name != 0 && summer_name != 0 {
			C.v_time_names_set(-i64(standard.offset), i32(has_dst), standard_name, summer_name)
		}
	}
	@[export: 'v_time_difftime'; markused]
	fn c_time_difftime(left i64, right i64) f64 {
		if left >= right { return f64(u64(left) - u64(right)) }
		return -f64(u64(right) - u64(left))
	}
	@[export: 'v_time_clock'; markused]
	fn c_time_clock() i64 {
		mut stamp := C.timespec{}
		if C.clock_gettime(2, &stamp) != 0 { return -1 }
		scale := i64(C.v_time_clocks_per_second())
		fraction := stamp.tv_nsec * scale / 1_000_000_000
		if stamp.tv_sec > (bare_max_epoch - fraction) / scale {
			C.v_time_errno_set(i32(C.EOVERFLOW))
			return -1
		}
		return stamp.tv_sec * scale + fraction
	}
	fn c_time_digits(mut buffer [26]u8, position int, number int, width int, space bool) {
		mut value := number
		for i := position + width - 1; i >= position; i-- {
			buffer[i] = if space && value == 0 && i != position + width - 1 {
				u8(` `)
			} else {
				u8(`0` + value % 10)
			}
			value /= 10
		}
	}
	@[export: 'v_time_asctime'; markused]
	fn c_time_asctime(address uint, output uint) uint {
		mut fields := [9]i32{}
		C.v_time_tm_get(address, uint(&fields[0]))
		year := int(fields[0]) + 1900
		if year < 0 || year > 9999 || fields[1] < 0 || fields[1] > 11 || fields[6] < 0 || fields[6] > 6 || fields[2] < 0 || fields[2] > 99 || fields[3] < 0 || fields[3] > 99 || fields[4] < 0 || fields[4] > 99 || fields[5] < 0 || fields[5] > 99 {
			C.v_time_errno_set(i32(C.EOVERFLOW))
			return 0
		}
		// A borrowed view writes the caller's required 26-byte C buffer.
		mut buffer := [26]u8{}
		weekdays := 'SunMonTueWedThuFriSat'
		months := 'JanFebMarAprMayJunJulAugSepOctNovDec'
		for i in 0 .. 3 {
			buffer[i] = weekdays[int(fields[6]) * 3 + i]
			buffer[i + 4] = months[int(fields[1]) * 3 + i]
		}
		for i in [3, 7, 10, 19]! { buffer[i] = ` ` }
		buffer[13] = `:`
		buffer[16] = `:`
		buffer[24] = `\n`
		buffer[25] = 0
		c_time_digits(mut buffer, 8, int(fields[2]), 2, true)
		c_time_digits(mut buffer, 11, int(fields[3]), 2, false)
		c_time_digits(mut buffer, 14, int(fields[4]), 2, false)
		c_time_digits(mut buffer, 17, int(fields[5]), 2, false)
		c_time_digits(mut buffer, 20, year, 4, false)
		unsafe { vmemcpy(voidptr(output), &buffer[0], 26) }
		return output
	}
	@[export: 'v_time_ctime'; markused]
	fn c_time_ctime(input uint, output uint) uint {
		mut calendar := C.tm{}
		if c_time_localtime(input, uint(&calendar)) == 0 { return 0 }
		return c_time_asctime(uint(&calendar), output)
	}
}
