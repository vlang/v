module time

import os

fn local_location() !&Location {
	tz := os.getenv('TZ')
	if tz != '' {
		if tz.starts_with(':') {
			path := tz[1..]
			if path != '' {
				if data := os.read_bytes(path) {
					return parse_tzif_location('Local', data) or { fixed_local_location() }
				}
				if !os.is_abs_path(path) {
					return load_location(path) or { fixed_local_location() }
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
	if os.is_link(localtime) {
		target := os.readlink(localtime) or { '' }
		if target != '' {
			if name := zoneinfo_name_from_path(target) {
				return load_location(name) or { fixed_local_location() }
			}
		}
	}
	// Regular file or symlink whose target is not under .../zoneinfo/...
	if os.exists(localtime) {
		if data := os.read_bytes(localtime) {
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

fn location_from_posix_rule(name string, rule PosixZoneRule) &Location {
	mut zones := [
		Zone{
			name:   rule.std_name
			offset: rule.std_offset
		},
	]
	if rule.has_dst {
		zones << Zone{
			name:   rule.dst_name
			offset: rule.dst_offset
			is_dst: true
		}
	}
	return &Location{
		name:      name
		zones:     zones
		posix:     rule
		has_posix: true
	}
}

fn zoneinfo_name_from_path(path string) ?string {
	marker := '/zoneinfo/'
	index := path.index(marker) or { return none }
	return path[index + marker.len..]
}
