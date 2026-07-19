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
