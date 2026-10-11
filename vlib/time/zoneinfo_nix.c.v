module time

fn local_location() !&Location {
	if tz := zoneinfo_getenv('TZ') {
		if tz == '' {
			return load_location('UTC')
		}
		if tz.starts_with(':') || zoneinfo_is_abs_path(tz) {
			path := if tz.starts_with(':') { tz[1..] } else { tz }
			if path != '' {
				if data := zoneinfo_read_file(path) {
					return parse_tzif_location('Local', data) or { fixed_local_location() }
				}
				if !zoneinfo_is_abs_path(path) {
					return load_location(path) or {
						if rule := parse_posix_zone_rule(path) {
							return location_from_posix_rule('Local', rule)
						}
						fixed_local_location()
					}
				}
			}
		} else if tz != 'Local' {
			return load_location(tz) or {
				if rule := parse_posix_zone_rule(tz) {
					return location_from_posix_rule('Local', rule)
				}
				fixed_local_location()
			}
		}
	}
	localtime := '/etc/localtime'
	if target := zoneinfo_readlink(localtime) {
		if name := zoneinfo_name_from_path(target) {
			return load_location(name) or { fixed_local_location() }
		}
	}
	// Regular file or symlink whose target is not under .../zoneinfo/...
	if zoneinfo_exists(localtime) {
		if data := zoneinfo_read_file(localtime) {
			return parse_tzif_location('Local', data) or { fixed_local_location() }
		}
	}
	return fixed_local_location()
}

fn fixed_local_location() &Location {
	return &Location{
		name:  'Local'
		zones: [Zone{
			name:   'Local'
			offset: offset()
		}]
	}
}

fn zoneinfo_name_from_path(path string) ?string {
	marker := '/zoneinfo/'
	index := path.index(marker) or { return none }
	return path[index + marker.len..]
}
