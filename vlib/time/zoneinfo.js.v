module time

// Zone describes one time zone rule in an IANA location.
pub struct Zone {
pub:
	name   string
	offset int
	is_dst bool
}

// Location contains parsed IANA time zone data.
pub struct Location {
pub:
	name string
}

// load_location loads an IANA time zone location.
pub fn load_location(name string) !&Location {
	return error('IANA time zone locations are not supported by the JS backend')
}

// zone_at returns the active zone rule for `unix_time`.
pub fn (loc &Location) zone_at(unix_time i64) !Zone {
	return error('IANA time zone locations are not supported by the JS backend')
}

// offset_at returns the UTC offset in seconds for `unix_time` in the location.
pub fn (loc &Location) offset_at(unix_time i64) !int {
	return error('IANA time zone locations are not supported by the JS backend')
}

// unix_to_local resolves a Unix timestamp to a Time in the location.
pub fn (loc &Location) unix_to_local(unix_time i64) !Time {
	return error('IANA time zone locations are not supported by the JS backend')
}

// unix_nanosecond_to_local resolves a Unix timestamp and nanosecond to a Time
// in the location.
pub fn (loc &Location) unix_nanosecond_to_local(unix_time i64, nanosecond int) !Time {
	return error('IANA time zone locations are not supported by the JS backend')
}

// in resolves `t` as a Time in `loc`.
pub fn (t Time) in(loc &Location) !Time {
	return error('IANA time zone locations are not supported by the JS backend')
}

// location returns the IANA location associated with `t`, if any.
pub fn (t Time) location() ?Location {
	return t.loc
}

// zone returns the active zone rule for `t`, if it has an IANA location.
pub fn (t Time) zone() !Zone {
	return error('IANA time zone locations are not supported by the JS backend')
}
