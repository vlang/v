module time

import os

fn test_parse_posix_tail_without_dst() {
	rule := parse_posix_zone_rule('CST-8')!
	zone := rule.zone_at(2_524_608_000)
	assert zone.name == 'CST'
	assert zone.offset == 28_800
	assert zone.is_dst == false
}

fn test_parse_posix_tail_julian_and_day_of_year_rules() {
	rule := parse_posix_zone_rule('<+00>0<+01>-1,0/0,J365/23')!
	start_of_year := rule.zone_at(2_524_608_000) // 2050-01-01 00:00 UTC
	end_of_year := rule.zone_at(2_556_057_600) // 2050-12-31 00:00 UTC
	assert start_of_year.name == '+01'
	assert start_of_year.offset == 3_600
	assert start_of_year.is_dst == true
	assert end_of_year.name == '+01'
	assert end_of_year.offset == 3_600
	assert end_of_year.is_dst == true
}

fn test_posix_last_weekday_stays_in_month() {
	rule := parse_posix_zone_rule('GMT0BST,M3.5.0/1,M10.5.0')!
	after_transition := time_fields_to_unix(Time{
		year:  2038
		month: 3
		day:   28
		hour:  2
	})
	zone := rule.zone_at(after_transition)
	assert zone.name == 'BST'
	assert zone.offset == 3_600
	assert zone.is_dst == true
}

fn test_parse_posix_rule_rejects_invalid_numbers() {
	if _ := parse_posix_zone_rule('EST5EDT,Mx.2.0,M11.1.0') {
		assert false
	} else {
		assert err.msg().contains('POSIX')
	}
	if _ := parse_posix_zone_rule('EST5EDT,M13.2.0,M11.1.0') {
		assert false
	} else {
		assert err.msg().contains('POSIX')
	}
	if _ := parse_posix_zone_rule('EST5EDT,M3.2.0/2:99,M11.1.0') {
		assert false
	} else {
		assert err.msg().contains('POSIX')
	}
	if _ := parse_posix_zone_rule('EST5:99') {
		assert false
	} else {
		assert err.msg().contains('POSIX')
	}
}

fn test_parse_posix_rule_uses_default_dst_transitions() {
	rule := parse_posix_zone_rule('EST5EDT')!
	winter := rule.zone_at(1_704_067_200) // 2024-01-01 00:00 UTC
	summer := rule.zone_at(1_719_792_000) // 2024-06-30 20:00 UTC
	assert winter.name == 'EST'
	assert winter.offset == -18_000
	assert winter.is_dst == false
	assert summer.name == 'EDT'
	assert summer.offset == -14_400
	assert summer.is_dst == true
}

fn test_parse_posix_negative_transition_time_with_minutes() {
	rule := parse_posix_zone_rule('<-02>2<-01>,M3.5.0/-1:30,M10.5.0/0')!
	transition := rule.transition_utc(2050, rule.start, rule.std_offset)
	expected := time_fields_to_unix(Time{
		year:   2050
		month:  3
		day:    27
		hour:   0
		minute: 30
	})
	assert transition == expected
}

fn test_tzif_rejects_negative_counts() {
	mut data := []u8{len: 44}
	copy(mut data[0..4], 'TZif'.bytes())
	data[40] = 0xff
	if _ := parse_tzif_location('Bad/Zone', data) {
		assert false
	} else {
		assert err.msg().contains('invalid TZif header counts')
	}
}

fn test_tzif_v4_uses_64bit_data_and_posix_tail() {
	mut data := load_zoneinfo_from_source(zoneinfo_vroot_zip, 'Europe/London')!
	data[4] = `4`
	loc := parse_tzif_location('Europe/London', data)!
	winter := loc.zone_at(2_524_608_000)! // 2050-01-01 00:00 UTC
	summer := loc.zone_at(2_540_246_400)! // 2050-07-01 00:00 UTC
	assert winter.name == 'GMT'
	assert winter.offset == 0
	assert summer.name == 'BST'
	assert summer.offset == 3_600
}

fn test_zoneinfo_zip_rejects_overflowing_central_directory_offset() {
	mut data := []u8{len: 22}
	data[0] = 0x50
	data[1] = 0x4b
	data[2] = 0x05
	data[3] = 0x06
	data[8] = 1
	data[10] = 1
	data[16] = 0xff
	data[17] = 0xff
	data[18] = 0xff
	data[19] = 0xff
	if _ := read_uncompressed_zoneinfo_zip_entry(data, 'UTC') {
		assert false
	} else {
		assert err.msg().contains('invalid zoneinfo.zip')
	}
}

fn test_zoneinfo_search_continues_after_invalid_tzif_data() {
	temp_dir := os.join_path(os.vtmp_dir(), 'zoneinfo_source_fallback')
	bad_source := os.join_path(temp_dir, 'bad')
	good_source := os.join_path(temp_dir, 'good')
	zone_name := 'Europe/London'
	os.rmdir_all(temp_dir) or {}
	defer {
		os.rmdir_all(temp_dir) or {}
	}
	os.mkdir_all(os.join_path(bad_source, 'Europe'))!
	os.mkdir_all(os.join_path(good_source, 'Europe'))!
	os.write_file_array(os.join_path(bad_source, zone_name), [u8(1), 2, 3])!
	valid_data := load_zoneinfo_from_source(zoneinfo_vroot_zip, zone_name)!
	os.write_file_array(os.join_path(good_source, zone_name), valid_data)!
	loc := load_zoneinfo_location_from_sources(zone_name, [bad_source, good_source], [])!
	assert loc.name == zone_name
	assert loc.offset_at(1_704_067_200)! == 0
}

fn unavailable_test_zoneinfo_loader(name string) ![]u8 {
	return error('no test zoneinfo for "${name}"')
}

fn register_test_zoneinfo_loaders(count int) {
	for _ in 0 .. count {
		register_zoneinfo_loader(unavailable_test_zoneinfo_loader)
	}
}

fn read_test_zoneinfo_loader_snapshots(count int) {
	for _ in 0 .. count {
		_ = zoneinfo_loaders_snapshot()
	}
}

fn test_zoneinfo_loader_registry_supports_concurrent_access() {
	initial_count := zoneinfo_loaders_snapshot().len
	mut threads := []thread{}
	for _ in 0 .. 4 {
		threads << spawn register_test_zoneinfo_loaders(25)
		threads << spawn read_test_zoneinfo_loader_snapshots(100)
	}
	threads.wait()
	assert zoneinfo_loaders_snapshot().len == initial_count + 100
}
