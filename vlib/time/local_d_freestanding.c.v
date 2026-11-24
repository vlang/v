@[has_globals]
module time

$if linux && !native && !i386 {
	#include <errno.h>

	const bare_min_epoch = i64(-9223372036854775807) - 1
	const bare_max_epoch = i64(9223372036854775807)

	__global bare_local_cache &Location = unsafe { nil }
	__global bare_local_cache_loaders int
	__global bare_local_loading u32
	__global bare_local_loading_tid u64

	fn bare_realtime_parts() (i64, int) {
		mut stamp := C.timespec{}
		if C.clock_gettime(C.CLOCK_REALTIME, &stamp) != 0 {
			panic('time: realtime clock failed (${C.errno})')
		}
		return stamp.tv_sec, int(stamp.tv_nsec)
	}

	fn bare_local_cache_can_persist() bool {
		$if prealloc {
			return false
		} $else {
			$if !no_builtin_arena ? {
				return v_arena_current() == unsafe { nil }
			}
			return true
		}
	}

	// BF exposes the immutable startup environment. Cache its local-clock
	// configuration on stable heap storage; explicit load_location remains an
	// uncached lookup. Loader registration invalidates this snapshot by length.
	fn bare_local_clock_location() &Location {
		for {
			zoneinfo_loaders_lock()
			if bare_local_cache != unsafe { nil }
				&& bare_local_cache_loaders == zoneinfo_loaders_count {
				loc := bare_local_cache
				zoneinfo_loaders_unlock()
				return loc
			}
			tid := bf_gettid()
			if C.atomic_load_u32(&bare_local_loading) != 0 {
				if bare_local_loading_tid == tid {
					zoneinfo_loaders_unlock()
					panic('time: recursive local timezone loader; use utc() inside a loader')
				}
				zoneinfo_loaders_unlock()
				code := bf_futex_wait(&bare_local_loading, 1, -1)
				if code !in [0, 4, 11] {
					panic('time: timezone wait failed (${code})')
				}
				continue
			}
			loaders := zoneinfo_loaders_count
			bare_local_loading_tid = tid
			C.atomic_store_u32(&bare_local_loading, 1)
			zoneinfo_loaders_unlock()
			persistent := bare_local_cache_can_persist()
			loc := local_location() or { fixed_local_location() }
			zoneinfo_loaders_lock()
			if persistent && bare_local_cache_can_persist() && loaders == zoneinfo_loaders_count {
				bare_local_cache = loc
				bare_local_cache_loaders = loaders
			}
			bare_local_loading_tid = 0
			C.atomic_store_u32(&bare_local_loading, 0)
			zoneinfo_loaders_unlock()
			_, code := bf_futex_wake(&bare_local_loading, 0x7fffffff)
			if code != 0 {
				panic('time: timezone wake failed (${code})')
			}
			return loc
		}
	}

	fn bare_local_from_unix(epoch i64, nanosecond int) Time {
		return bare_local_from_unix_at_location(epoch, nanosecond, bare_local_clock_location())
	}

	fn bare_local_from_unix_at_location(epoch i64, nanosecond int, loc &Location) Time {
		if nanosecond < 0 || nanosecond >= 1_000_000_000 {
			normalized := unix_nanosecond(epoch, nanosecond)
			return bare_local_from_unix_at_location(normalized.unix, normalized.nanosecond, loc)
		}
		zone := loc.zone_at(epoch) or { panic(err) }
		offset := i64(zone.offset)
		if (offset > 0 && epoch > bare_max_epoch - offset)
			|| (offset < 0 && epoch < bare_min_epoch - offset) {
			panic('time: local calendar is outside the Unix timestamp range')
		}
		local := unix_nanosecond(epoch + offset, nanosecond)
		return Time{
			...local
			is_local:             true
			local_offset_known:   true
			local_offset_seconds: i32(zone.offset)
		}
	}

	fn bare_subtract_offset(wall i64, offset int) ?i64 {
		if (offset > 0 && wall < bare_min_epoch + i64(offset))
			|| (offset < 0 && wall > bare_max_epoch + i64(offset)) {
			return none
		}
		return wall - i64(offset)
	}

	fn bare_local_candidate(loc &Location, wall i64, offset int) ?i64 {
		candidate := bare_subtract_offset(wall, offset) or { return none }
		actual := loc.offset_at(candidate) or { return none }
		if actual != offset {
			return none
		}
		return candidate
	}

	fn bare_gap_candidate(loc &Location, wall i64, transition i64) ?i64 {
		// TZif UTC offsets are signed 32-bit values. A more distant transition
		// cannot contain this wall time; unsigned distance avoids signed overflow.
		distance := if transition >= wall {
			u64(transition) - u64(wall)
		} else {
			u64(wall) - u64(transition)
		}
		if distance > u64(2147483648) || transition == bare_min_epoch {
			return none
		}
		before := loc.offset_at(transition - 1) or { return none }
		after := loc.offset_at(transition) or { return none }
		if after <= before {
			return none
		}
		using_before := bare_subtract_offset(wall, before) or { return none }
		using_after := bare_subtract_offset(wall, after) or { return none }
		if using_before >= transition && using_after < transition {
			return using_before
		}
		return none
	}

	fn bare_local_wall_to_unix(t Time, loc &Location) i64 {
		wall := time_fields_to_unix(normalize_zero_date_parts(t))
		mut found := false
		mut earliest := i64(0)
		for zone in loc.zones {
			if candidate := bare_local_candidate(loc, wall, zone.offset) {
				if !found || candidate < earliest {
					earliest = candidate
					found = true
				}
			}
		}
		if loc.has_posix {
			for offset in [loc.posix.std_offset, loc.posix.dst_offset] {
				if candidate := bare_local_candidate(loc, wall, offset) {
					if !found || candidate < earliest {
						earliest = candidate
						found = true
					}
				}
			}
		}
		if found {
			return earliest
		}
		// No inverse exists inside a forward clock jump. Use the preceding
		// offset to normalize the nonexistent wall time forward by that jump.
		for tx in loc.transitions {
			if candidate := bare_gap_candidate(loc, wall, tx.when) {
				return candidate
			}
		}
		if loc.has_posix && loc.posix.has_dst {
			year := unix(wall).year
			for y in year - 2 .. year + 3 {
				for transition in [
					loc.posix.transition_utc(y, loc.posix.start, loc.posix.std_offset),
					loc.posix.transition_utc(y, loc.posix.end, loc.posix.dst_offset),
				] {
					if candidate := bare_gap_candidate(loc, wall, transition) {
						return candidate
					}
				}
			}
		}
		panic('time: local time is outside the location timestamp range')
	}
}
