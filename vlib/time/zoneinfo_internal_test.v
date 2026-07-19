module time

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
