import os
import time

fn testsuite_begin() {
	os.setenv('ZONEINFO', os.join_path(@VEXEROOT, 'vlib', 'time', 'tzdata', 'zoneinfo.zip'), true)
}

fn test_load_location_utc() {
	loc := time.load_location('UTC')!
	assert loc.offset_at(1_704_067_200)! == 0
	t := loc.unix_to_local(1_704_067_200)!
	assert t.unix() == 1_704_067_200
	t_loc := t.location() or { panic('missing location') }
	assert t_loc.name == 'UTC'
	assert (t.zone()!).name == 'UTC'
	assert t.year == 2024
	assert t.month == 1
	assert t.day == 1
	assert t.hour == 0
}

fn test_load_location_from_embedded_zoneinfo_zip() {
	loc := time.load_location('Asia/Shanghai')!
	assert loc.offset_at(1_704_067_200)! == 28_800
	t := loc.unix_to_local(1_704_067_200)!
	assert t.unix() == 1_704_067_200
	t_loc := t.location() or { panic('missing location') }
	assert t_loc.name == 'Asia/Shanghai'
	assert (t.zone()!).offset == 28_800
	assert t.year == 2024
	assert t.month == 1
	assert t.day == 1
	assert t.hour == 8
	assert t.minute == 0
	assert t.second == 0
}

fn test_load_location_from_zoneinfo_env_zip() {
	zoneinfo_zip := os.join_path(@VEXEROOT, 'vlib', 'time', 'tzdata', 'zoneinfo.zip')
	os.setenv('ZONEINFO', zoneinfo_zip, true)
	loc := time.load_location('Pacific/Niue')!
	t := loc.unix_to_local(1_704_067_200)!
	assert t.year == 2023
	assert t.month == 12
	assert t.day == 31
	assert t.hour == 13
	assert t.unix() == 1_704_067_200
}

fn test_load_location_with_dst_transition() {
	loc := time.load_location('America/New_York')!
	before := loc.unix_to_local(1_710_053_940)!
	after := loc.unix_to_local(1_710_054_000)!
	assert loc.offset_at(1_705_320_000)! == -18_000
	assert loc.offset_at(1_721_044_800)! == -14_400
	before_zone := before.zone()!
	assert before_zone.name == 'EST'
	assert before_zone.is_dst == false
	assert before.year == 2024
	assert before.month == 3
	assert before.day == 10
	assert before.hour == 1
	assert before.minute == 59
	assert after.year == 2024
	assert after.month == 3
	assert after.day == 10
	assert after.hour == 3
	assert after.minute == 0
	after_zone := after.zone()!
	assert after_zone.name == 'EDT'
	assert after_zone.is_dst == true
}

fn test_load_location_local() {
	loc := time.load_location('Local')!
	t := time.utc().in(loc)!
	t_loc := t.location() or { panic('missing location') }
	assert t_loc.name == 'Local' || t_loc.name.len > 0
	assert t.zone()!.name.len > 0
}

fn test_time_in_keeps_nanosecond() {
	loc := time.load_location('Asia/Shanghai')!
	utc_time := time.unix_nanosecond(1_704_067_200, 123_456_789)
	local := utc_time.in(loc)!
	assert local.unix() == 1_704_067_200
	local_loc := local.location() or { panic('missing location') }
	assert local_loc.name == 'Asia/Shanghai'
	assert local.year == 2024
	assert local.month == 1
	assert local.day == 1
	assert local.hour == 8
	assert local.nanosecond == 123_456_789
}

fn test_location_time_utc_formats_use_instant() {
	loc := time.load_location('Asia/Shanghai')!
	local := time.unix_nanosecond(1_704_067_200, 123_456_789).in(loc)!
	assert local.year == 2024
	assert local.hour == 8
	assert local.format_rfc3339() == '2024-01-01T00:00:00.123Z'
	assert local.format_rfc3339_micro() == '2024-01-01T00:00:00.123456Z'
	assert local.format_rfc3339_nano() == '2024-01-01T00:00:00.123456789Z'
	assert local.http_header_string() == 'Mon, 01 Jan 2024 00:00:00 GMT'
}

fn test_location_time_strftime_uses_wall_clock() {
	loc := time.load_location('Asia/Shanghai')!
	local := time.unix(1_704_067_200).in(loc)!
	assert local.strftime('%Y-%m-%d %H:%M:%S') == '2024-01-01 08:00:00'
}

fn test_location_time_add_keeps_instant_and_location() {
	loc := time.load_location('Asia/Shanghai')!
	local := time.unix_nanosecond(1_704_067_200, 123_456_789).in(loc)!
	added := local.add(1 * time.second)
	added_loc := added.location() or { panic('missing location') }
	assert added_loc.name == 'Asia/Shanghai'
	assert added.unix() == 1_704_067_201
	assert added.year == 2024
	assert added.month == 1
	assert added.day == 1
	assert added.hour == 8
	assert added.minute == 0
	assert added.second == 1
	assert added.nanosecond == 123_456_789
}

fn test_location_time_add_seconds_keeps_unix_epoch() {
	loc := time.load_location('Asia/Shanghai')!
	local := time.unix(0).in(loc)!
	added := local.add_seconds(1)
	added_loc := added.location() or { panic('missing location') }
	assert added_loc.name == 'Asia/Shanghai'
	assert added.unix() == 1
	assert added.year == 1970
	assert added.month == 1
	assert added.day == 1
	assert added.hour == 8
	assert added.minute == 0
	assert added.second == 1
}

fn test_location_time_equals_same_instant() {
	loc := time.load_location('Asia/Shanghai')!
	utc_time := time.unix_nanosecond(1_704_067_200, 123_456_789)
	local := utc_time.in(loc)!
	assert utc_time == local
}

fn test_invalid_location_name() {
	if _ := time.load_location('../UTC') {
		assert false
	} else {
		assert err.msg().contains('invalid time zone location')
	}
	if _ := time.load_location('/UTC') {
		assert false
	} else {
		assert err.msg().contains('invalid time zone location')
	}
	if _ := time.load_location('') {
		assert false
	} else {
		assert err.msg().contains('invalid time zone location')
	}
}

fn test_load_location_southern_hemisphere_dst() {
	loc := time.load_location('America/Santiago')!
	// Southern-hemisphere DST: offset is larger (less negative) in summer.
	summer := loc.zone_at(1_709_251_200)! // 2024-02-29 21:00 UTC
	winter := loc.zone_at(1_719_792_000)! // 2024-06-30 20:00 UTC
	assert summer.is_dst == true
	assert summer.offset == -10_800
	assert winter.is_dst == false
	assert winter.offset == -14_400
	summer_local := loc.unix_to_local(1_709_251_200)!
	assert summer_local.year == 2024
	assert summer_local.month == 2
	assert summer_local.day == 29
	assert summer_local.hour == 21
}

fn test_load_location_posix_future_dst() {
	// Beyond the last packed transition, rules come from the POSIX TZ tail.
	loc := time.load_location('Europe/London')!
	winter := loc.zone_at(2_524_608_000)! // 2050-01-01 00:00 UTC
	summer := loc.zone_at(2_540_246_400)! // 2050-07-01 00:00 UTC
	assert winter.offset == 0
	assert winter.is_dst == false
	assert winter.name == 'GMT'
	assert summer.offset == 3_600
	assert summer.is_dst == true
	assert summer.name == 'BST'
	summer_local := loc.unix_to_local(2_540_246_400)!
	assert summer_local.year == 2050
	assert summer_local.month == 7
	assert summer_local.day == 1
	assert summer_local.hour == 1
}

fn test_load_location_posix_fixed_future_rule() {
	// The bundled zoneinfo.zip currently uses a fixed POSIX tail for Morocco.
	loc := time.load_location('Africa/Casablanca')!
	start_of_year := loc.zone_at(2_524_608_000)! // 2050-01-01 00:00 UTC
	end_of_year := loc.zone_at(2_556_057_600)! // 2050-12-31 00:00 UTC
	assert start_of_year.offset == 0
	assert end_of_year.offset == 0
}

fn test_fixed_offset_etc_gmt() {
	// Note: Etc/GMT+N uses POSIX sign (opposite of civil intuition).
	loc := time.load_location('Etc/GMT+5')!
	assert loc.offset_at(1_704_067_200)! == -18_000
	t := loc.unix_to_local(1_704_067_200)!
	assert t.year == 2023
	assert t.month == 12
	assert t.day == 31
	assert t.hour == 19
}

fn test_location_time_is_not_is_local() {
	loc := time.load_location('Asia/Shanghai')!
	local := time.unix(1_704_067_200).in(loc)!
	assert local.is_local == false
	assert local.location() != none
	// Same absolute instant as UTC; equality compares the unix epoch.
	assert local == time.unix(1_704_067_200)
}

fn test_unknown_location_name() {
	if _ := time.load_location('Not/ARealZone') {
		assert false
	} else {
		assert err.msg().contains('unknown time zone location')
	}
}

fn test_load_location_local_ignores_tz_local() {
	old_tz := os.getenv_opt('TZ')
	os.setenv('TZ', 'Local', true)
	defer {
		if old := old_tz {
			os.setenv('TZ', old, true)
		} else {
			os.unsetenv('TZ')
		}
	}
	loc := time.load_location('Local')!
	assert loc.name.len > 0
}

fn test_load_location_local_posix_tz() {
	old_tz := os.getenv_opt('TZ')
	os.setenv('TZ', 'EST5EDT,M3.2.0,M11.1.0', true)
	defer {
		if old := old_tz {
			os.setenv('TZ', old, true)
		} else {
			os.unsetenv('TZ')
		}
	}
	loc := time.load_location('Local')!
	winter := loc.zone_at(1_704_067_200)! // 2024-01-01 00:00 UTC
	summer := loc.zone_at(1_719_792_000)! // 2024-06-30 20:00 UTC
	assert winter.name == 'EST'
	assert winter.offset == -18_000
	assert winter.is_dst == false
	assert summer.name == 'EDT'
	assert summer.offset == -14_400
	assert summer.is_dst == true
}
