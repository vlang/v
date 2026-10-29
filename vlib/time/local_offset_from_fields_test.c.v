module time

fn C.tzset()

struct OffsetZone {
	tz    string
	label string
}

fn set_test_tz(tz string) {
	unsafe { C.setenv(c'TZ', &char(tz.str), 1) }
	C.tzset()
}

fn localtime_at(sec i64) C.tm {
	mut loc_tm := C.tm{}
	C.localtime_r(voidptr(&sec), &loc_tm)
	return loc_tm
}

// Solaris's struct tm has no tm_gmtoff, so c_local_epoch_offset derives the offset there
// from the local calendar fields with local_offset_from_fields. Where tm_gmtoff exists,
// the derivation must agree with it at every instant, including the hours next to DST
// changes, where reading the UTC fields back through mktime picks the wrong offset.
fn test_local_offset_from_fields_matches_tm_gmtoff() {
	$if windows || solaris {
		return
	} $else {
		old_tz_ptr := C.getenv(c'TZ')
		old_tz := if isnil(old_tz_ptr) { '' } else { unsafe { cstring_to_vstring(old_tz_ptr) } }
		defer {
			if isnil(old_tz_ptr) {
				C.unsetenv(c'TZ')
				C.tzset()
			} else {
				set_test_tz(old_tz)
			}
		}
		set_test_tz('EST5EDT,M3.2.0/2,M11.1.0/2')
		// 2026-03-08 06:30 UTC is 01:30 EST, half an hour before the spring change.
		spring := localtime_at(1772951400)
		assert spring.tm_hour == 1 && spring.tm_min == 30
		assert local_offset_from_fields(1772951400, &spring) == -18000
		// 2026-11-01 05:30 UTC is the first 01:30, still EDT, of the autumn change.
		autumn := localtime_at(1793511000)
		assert autumn.tm_hour == 1 && autumn.tm_min == 30
		assert local_offset_from_fields(1793511000, &autumn) == -14400
		zones := [
			OffsetZone{ tz: 'EST5EDT,M3.2.0/2,M11.1.0/2', label: 'New York' },
			OffsetZone{ tz: 'CET-1CEST,M3.5.0,M10.5.0/3', label: 'Berlin' },
			OffsetZone{ tz: 'AEST-10AEDT,M10.1.0,M4.1.0/3', label: 'Sydney' },
			OffsetZone{ tz: 'NST3:30NDT,M3.2.0,M11.1.0', label: 'St. Johns' },
			OffsetZone{ tz: 'UTC0', label: 'UTC' },
		]
		// 2026-01-01 00:00 UTC to 2027-01-01 00:00 UTC, every 10 minutes.
		start := i64(1767225600)
		end := i64(1798761600)
		for zone in zones {
			set_test_tz(zone.tz)
			for sec := start; sec < end; sec += 600 {
				loc_tm := localtime_at(sec)
				got := local_offset_from_fields(sec, &loc_tm)
				want := i64(loc_tm.tm_gmtoff)
				if got != want {
					eprintln('${zone.label}: sec=${sec} got=${got} want=${want}')
				}
				assert got == want
			}
		}
	}
}
