module time

import os

const windows_min_system_year = 1601

struct TimeZoneInformation {
pub mut:
	bias          i32
	standard_name [32]u16
	standard_date SystemTime
	standard_bias i32
	daylight_name [32]u16
	daylight_date SystemTime
	daylight_bias i32
}

struct DynamicTimeZoneInformation {
pub mut:
	bias                              i32
	standard_name                     [32]u16
	standard_date                     SystemTime
	standard_bias                     i32
	daylight_name                     [32]u16
	daylight_date                     SystemTime
	daylight_bias                     i32
	time_zone_key_name                [128]u16
	dynamic_daylight_time_is_disabled u8
}

fn C.GetTimeZoneInformation(&TimeZoneInformation) u32

fn C.GetDynamicTimeZoneInformation(&DynamicTimeZoneInformation) u32

fn C.GetTimeZoneInformationForYear(u16, &DynamicTimeZoneInformation, &TimeZoneInformation) C.BOOL

fn local_location() !&Location {
	tz := os.getenv('TZ')
	if tz != '' && tz != 'Local' && !tz.starts_with(':') {
		if rule := parse_posix_zone_rule(tz) {
			return location_from_posix_rule('Local', rule)
		}
	}
	mut info := TimeZoneInformation{}
	C.GetTimeZoneInformation(&info)
	std_zone, dst_zone := windows_zones(info)
	mut loc := &Location{
		name: 'Local'
	}
	if windows_has_daylight(info) {
		loc.posix = windows_posix_rule(std_zone, dst_zone, info)
		loc.has_posix = true
	}
	mut dynamic_info := DynamicTimeZoneInformation{}
	dynamic_status := C.GetDynamicTimeZoneInformation(&dynamic_info)
	has_dynamic_info := dynamic_status != u32(0xffff_ffff)
		&& dynamic_info.dynamic_daylight_time_is_disabled == 0
	current_year := now().year
	if has_dynamic_info {
		// Windows SYSTEMTIME values start at 1601. Query the complete representable
		// history so old dynamic rules do not silently fall back to today's offset.
		for year in windows_min_system_year .. current_year + 1 {
			mut candidate := TimeZoneInformation{}
			if C.GetTimeZoneInformationForYear(u16(year), &dynamic_info, &candidate) == 0 {
				continue
			}
			loc.add_windows_year_transitions(year, candidate)
		}
	}
	if loc.zones.len == 0 {
		loc.zones << std_zone
		if windows_has_daylight(info) {
			loc.zones << dst_zone
		}
		for year in current_year - 100 .. current_year + 101 {
			loc.add_windows_year_transitions(year, info)
		}
	}
	return loc
}

fn windows_has_daylight(info TimeZoneInformation) bool {
	dst_name := unsafe { string_from_wide(&u16(&info.daylight_name[0])) }
	return info.daylight_date.month != 0 && info.standard_date.month != 0 && dst_name != ''
}

fn windows_zones(info TimeZoneInformation) (Zone, Zone) {
	std_name := unsafe { string_from_wide(&u16(&info.standard_name[0])) }
	dst_name := unsafe { string_from_wide(&u16(&info.daylight_name[0])) }
	abbr := windows_abbr(std_name)
	std_abbr := if abbr.std != '' { abbr.std } else { windows_abbr_from_name(std_name) }
	dst_abbr := if abbr.dst != '' { abbr.dst } else { windows_abbr_from_name(dst_name) }
	return Zone{
		name:   if std_abbr == '' { 'Local' } else { std_abbr }
		offset: -int(info.bias + info.standard_bias) * seconds_per_minute
	}, Zone{
		name:   if dst_abbr == '' { 'Daylight' } else { dst_abbr }
		offset: -int(info.bias + info.daylight_bias) * seconds_per_minute
		is_dst: true
	}
}

fn (mut loc Location) add_windows_year_transitions(year int, info TimeZoneInformation) {
	std_zone, dst_zone := windows_zones(info)
	std_index := loc.windows_zone_index(std_zone)
	has_daylight := windows_has_daylight(info)
	mut dst_index := -1
	mut dst_start := i64(0)
	mut std_start := i64(0)
	mut year_start_index := std_index
	mut year_start_offset := std_zone.offset
	if has_daylight {
		dst_index = loc.windows_zone_index(dst_zone)
		dst_start = windows_transition_utc(year, info.daylight_date, std_zone.offset)
		std_start = windows_transition_utc(year, info.standard_date, dst_zone.offset)
		if std_start < dst_start {
			year_start_index = dst_index
			year_start_offset = dst_zone.offset
		}
	}
	if loc.transitions.len == 0 || loc.transitions.last().index != year_start_index {
		loc.transitions << ZoneTransition{
			when:  time_fields_to_unix(Time{
				year:  year
				month: 1
				day:   1
			}) - i64(year_start_offset)
			index: year_start_index
		}
	}
	if !has_daylight {
		return
	}
	if dst_start < std_start {
		loc.transitions << ZoneTransition{
			when:  dst_start
			index: dst_index
		}
		loc.transitions << ZoneTransition{
			when:  std_start
			index: std_index
		}
	} else {
		loc.transitions << ZoneTransition{
			when:  std_start
			index: std_index
		}
		loc.transitions << ZoneTransition{
			when:  dst_start
			index: dst_index
		}
	}
}

fn (mut loc Location) windows_zone_index(zone Zone) int {
	for i, existing in loc.zones {
		if existing.name == zone.name && existing.offset == zone.offset
			&& existing.is_dst == zone.is_dst {
			return i
		}
	}
	loc.zones << zone
	return loc.zones.len - 1
}

fn windows_posix_rule(std_zone Zone, dst_zone Zone, info TimeZoneInformation) PosixZoneRule {
	return PosixZoneRule{
		std_name:   std_zone.name
		std_offset: std_zone.offset
		dst_name:   dst_zone.name
		dst_offset: dst_zone.offset
		start:      windows_system_time_rule(info.daylight_date)
		end:        windows_system_time_rule(info.standard_date)
		has_dst:    true
	}
}

fn windows_system_time_rule(st SystemTime) PosixRule {
	return PosixRule{
		kind:    .month_week_day
		month:   int(st.month)
		week:    int(st.day)
		weekday: int(st.day_of_week)
		seconds: int(st.hour) * seconds_per_hour + int(st.minute) * seconds_per_minute +
			int(st.second)
	}
}

fn windows_transition_utc(year int, st SystemTime, offset_before int) i64 {
	day := windows_month_week_day(year, int(st.month), int(st.day), int(st.day_of_week))
	local := time_fields_to_unix(Time{
		year:   year
		month:  int(st.month)
		day:    day
		hour:   int(st.hour)
		minute: int(st.minute)
		second: int(st.second)
	})
	return local - i64(offset_before)
}

fn windows_month_week_day(year int, month int, week int, weekday int) int {
	first_weekday := day_of_week(year, month, 1) % 7
	mut day := 1 + ((weekday - first_weekday + 7) % 7) + (week - 1) * 7
	days := days_in_month(month, year) or { 31 }
	if week == 5 {
		for day + 7 <= days {
			day += 7
		}
		if day > days {
			day -= 7
		}
	}
	return day
}

struct WindowsAbbr {
	std string
	dst string
}

fn windows_abbr(name string) WindowsAbbr {
	return windows_abbrs[name] or { WindowsAbbr{} }
}

fn windows_abbr_from_name(name string) string {
	mut out := []u8{}
	for c in name {
		if c >= `A` && c <= `Z` {
			out << u8(c)
		}
	}
	if out.len == 0 {
		return name
	}
	return out.bytestr()
}

const windows_abbrs = {
	'Egypt Standard Time':             WindowsAbbr{'EET', 'EEST'}
	'Morocco Standard Time':           WindowsAbbr{'+00', '+01'}
	'South Africa Standard Time':      WindowsAbbr{'SAST', 'SAST'}
	'South Sudan Standard Time':       WindowsAbbr{'CAT', 'CAT'}
	'Sudan Standard Time':             WindowsAbbr{'CAT', 'CAT'}
	'W. Central Africa Standard Time': WindowsAbbr{'WAT', 'WAT'}
	'E. Africa Standard Time':         WindowsAbbr{'EAT', 'EAT'}
	'Sao Tome Standard Time':          WindowsAbbr{'GMT', 'GMT'}
	'Libya Standard Time':             WindowsAbbr{'EET', 'EET'}
	'Namibia Standard Time':           WindowsAbbr{'CAT', 'CAT'}
	'Aleutian Standard Time':          WindowsAbbr{'HST', 'HDT'}
	'Alaskan Standard Time':           WindowsAbbr{'AKST', 'AKDT'}
	'Tocantins Standard Time':         WindowsAbbr{'-03', '-03'}
	'Paraguay Standard Time':          WindowsAbbr{'-04', '-03'}
	'Bahia Standard Time':             WindowsAbbr{'-03', '-03'}
	'SA Pacific Standard Time':        WindowsAbbr{'-05', '-05'}
	'Argentina Standard Time':         WindowsAbbr{'-03', '-03'}
	'Eastern Standard Time (Mexico)':  WindowsAbbr{'EST', 'EST'}
	'Venezuela Standard Time':         WindowsAbbr{'-04', '-04'}
	'SA Eastern Standard Time':        WindowsAbbr{'-03', '-03'}
	'Central Standard Time':           WindowsAbbr{'CST', 'CDT'}
	'Central Brazilian Standard Time': WindowsAbbr{'-04', '-04'}
	'Mountain Standard Time':          WindowsAbbr{'MST', 'MDT'}
	'Greenland Standard Time':         WindowsAbbr{'-02', '-01'}
	'Turks And Caicos Standard Time':  WindowsAbbr{'EST', 'EDT'}
	'Central America Standard Time':   WindowsAbbr{'CST', 'CST'}
	'Atlantic Standard Time':          WindowsAbbr{'AST', 'ADT'}
	'Cuba Standard Time':              WindowsAbbr{'CST', 'CDT'}
	'US Eastern Standard Time':        WindowsAbbr{'EST', 'EDT'}
	'SA Western Standard Time':        WindowsAbbr{'-04', '-04'}
	'Pacific Standard Time':           WindowsAbbr{'PST', 'PDT'}
	'Mountain Standard Time (Mexico)': WindowsAbbr{'MST', 'MST'}
	'Central Standard Time (Mexico)':  WindowsAbbr{'CST', 'CST'}
	'Saint Pierre Standard Time':      WindowsAbbr{'-03', '-02'}
	'Montevideo Standard Time':        WindowsAbbr{'-03', '-03'}
	'Eastern Standard Time':           WindowsAbbr{'EST', 'EDT'}
	'US Mountain Standard Time':       WindowsAbbr{'MST', 'MST'}
	'Haiti Standard Time':             WindowsAbbr{'EST', 'EDT'}
	'Magallanes Standard Time':        WindowsAbbr{'-03', '-03'}
	'Canada Central Standard Time':    WindowsAbbr{'CST', 'CST'}
	'Pacific SA Standard Time':        WindowsAbbr{'-04', '-03'}
	'E. South America Standard Time':  WindowsAbbr{'-03', '-03'}
	'Newfoundland Standard Time':      WindowsAbbr{'NST', 'NDT'}
	'Pacific Standard Time (Mexico)':  WindowsAbbr{'PST', 'PDT'}
	'Yukon Standard Time':             WindowsAbbr{'MST', 'MST'}
	'Jordan Standard Time':            WindowsAbbr{'+03', '+03'}
	'Arabic Standard Time':            WindowsAbbr{'+03', '+03'}
	'Azerbaijan Standard Time':        WindowsAbbr{'+04', '+04'}
	'SE Asia Standard Time':           WindowsAbbr{'+07', '+07'}
	'Altai Standard Time':             WindowsAbbr{'+07', '+07'}
	'Middle East Standard Time':       WindowsAbbr{'EET', 'EEST'}
	'Central Asia Standard Time':      WindowsAbbr{'+06', '+06'}
	'India Standard Time':             WindowsAbbr{'IST', 'IST'}
	'Transbaikal Standard Time':       WindowsAbbr{'+09', '+09'}
	'Sri Lanka Standard Time':         WindowsAbbr{'+0530', '+0530'}
	'Syria Standard Time':             WindowsAbbr{'+03', '+03'}
	'Bangladesh Standard Time':        WindowsAbbr{'+06', '+06'}
	'Arabian Standard Time':           WindowsAbbr{'+04', '+04'}
	'West Bank Standard Time':         WindowsAbbr{'EET', 'EEST'}
	'W. Mongolia Standard Time':       WindowsAbbr{'+07', '+07'}
	'North Asia East Standard Time':   WindowsAbbr{'+08', '+08'}
	'Israel Standard Time':            WindowsAbbr{'IST', 'IDT'}
	'Afghanistan Standard Time':       WindowsAbbr{'+0430', '+0430'}
	'Russia Time Zone 11':             WindowsAbbr{'+12', '+12'}
	'Pakistan Standard Time':          WindowsAbbr{'PKT', 'PKT'}
	'Nepal Standard Time':             WindowsAbbr{'+0545', '+0545'}
	'North Asia Standard Time':        WindowsAbbr{'+07', '+07'}
	'Magadan Standard Time':           WindowsAbbr{'+11', '+11'}
	'N. Central Asia Standard Time':   WindowsAbbr{'+07', '+07'}
	'Omsk Standard Time':              WindowsAbbr{'+06', '+06'}
	'North Korea Standard Time':       WindowsAbbr{'KST', 'KST'}
	'Qyzylorda Standard Time':         WindowsAbbr{'+05', '+05'}
	'Myanmar Standard Time':           WindowsAbbr{'+0630', '+0630'}
	'Arab Standard Time':              WindowsAbbr{'+03', '+03'}
	'Sakhalin Standard Time':          WindowsAbbr{'+11', '+11'}
	'Korea Standard Time':             WindowsAbbr{'KST', 'KST'}
	'China Standard Time':             WindowsAbbr{'CST', 'CST'}
	'Singapore Standard Time':         WindowsAbbr{'+08', '+08'}
	'Russia Time Zone 10':             WindowsAbbr{'+11', '+11'}
	'Taipei Standard Time':            WindowsAbbr{'CST', 'CST'}
	'West Asia Standard Time':         WindowsAbbr{'+05', '+05'}
	'Georgian Standard Time':          WindowsAbbr{'+04', '+04'}
	'Iran Standard Time':              WindowsAbbr{'+0330', '+0330'}
	'Tokyo Standard Time':             WindowsAbbr{'JST', 'JST'}
	'Tomsk Standard Time':             WindowsAbbr{'+07', '+07'}
	'Ulaanbaatar Standard Time':       WindowsAbbr{'+08', '+08'}
	'Vladivostok Standard Time':       WindowsAbbr{'+10', '+10'}
	'Yakutsk Standard Time':           WindowsAbbr{'+09', '+09'}
	'Ekaterinburg Standard Time':      WindowsAbbr{'+05', '+05'}
	'Caucasus Standard Time':          WindowsAbbr{'+04', '+04'}
	'Azores Standard Time':            WindowsAbbr{'-01', '+00'}
	'Cape Verde Standard Time':        WindowsAbbr{'-01', '-01'}
	'Greenwich Standard Time':         WindowsAbbr{'GMT', 'GMT'}
	'Cen. Australia Standard Time':    WindowsAbbr{'ACST', 'ACDT'}
	'E. Australia Standard Time':      WindowsAbbr{'AEST', 'AEST'}
	'AUS Central Standard Time':       WindowsAbbr{'ACST', 'ACST'}
	'Aus Central W. Standard Time':    WindowsAbbr{'+0845', '+0845'}
	'Tasmania Standard Time':          WindowsAbbr{'AEST', 'AEDT'}
	'Lord Howe Standard Time':         WindowsAbbr{'+1030', '+11'}
	'W. Australia Standard Time':      WindowsAbbr{'AWST', 'AWST'}
	'AUS Eastern Standard Time':       WindowsAbbr{'AEST', 'AEDT'}
	'UTC-11':                          WindowsAbbr{'-11', '-11'}
	'Dateline Standard Time':          WindowsAbbr{'-12', '-12'}
	'UTC-02':                          WindowsAbbr{'-02', '-02'}
	'UTC-08':                          WindowsAbbr{'-08', '-08'}
	'UTC-09':                          WindowsAbbr{'-09', '-09'}
	'UTC+12':                          WindowsAbbr{'+12', '+12'}
	'UTC+13':                          WindowsAbbr{'+13', '+13'}
	'UTC':                             WindowsAbbr{'UTC', 'UTC'}
	'Astrakhan Standard Time':         WindowsAbbr{'+04', '+04'}
	'W. Europe Standard Time':         WindowsAbbr{'CET', 'CEST'}
	'GTB Standard Time':               WindowsAbbr{'EET', 'EEST'}
	'Central Europe Standard Time':    WindowsAbbr{'CET', 'CEST'}
	'E. Europe Standard Time':         WindowsAbbr{'EET', 'EEST'}
	'Turkey Standard Time':            WindowsAbbr{'+03', '+03'}
	'Kaliningrad Standard Time':       WindowsAbbr{'EET', 'EET'}
	'FLE Standard Time':               WindowsAbbr{'EET', 'EEST'}
	'GMT Standard Time':               WindowsAbbr{'GMT', 'BST'}
	'Belarus Standard Time':           WindowsAbbr{'+03', '+03'}
	'Russian Standard Time':           WindowsAbbr{'MSK', 'MSK'}
	'Romance Standard Time':           WindowsAbbr{'CET', 'CEST'}
	'Russia Time Zone 3':              WindowsAbbr{'+04', '+04'}
	'Saratov Standard Time':           WindowsAbbr{'+04', '+04'}
	'Volgograd Standard Time':         WindowsAbbr{'MSK', 'MSK'}
	'Central European Standard Time':  WindowsAbbr{'CET', 'CEST'}
	'Mauritius Standard Time':         WindowsAbbr{'+04', '+04'}
	'Samoa Standard Time':             WindowsAbbr{'+13', '+13'}
	'New Zealand Standard Time':       WindowsAbbr{'NZST', 'NZDT'}
	'Bougainville Standard Time':      WindowsAbbr{'+11', '+11'}
	'Chatham Islands Standard Time':   WindowsAbbr{'+1245', '+1345'}
	'Easter Island Standard Time':     WindowsAbbr{'-06', '-05'}
	'Fiji Standard Time':              WindowsAbbr{'+12', '+12'}
	'Central Pacific Standard Time':   WindowsAbbr{'+11', '+11'}
	'Hawaiian Standard Time':          WindowsAbbr{'HST', 'HST'}
	'Line Islands Standard Time':      WindowsAbbr{'+14', '+14'}
	'Marquesas Standard Time':         WindowsAbbr{'-0930', '-0930'}
	'Norfolk Standard Time':           WindowsAbbr{'+11', '+12'}
	'West Pacific Standard Time':      WindowsAbbr{'+10', '+10'}
	'Tonga Standard Time':             WindowsAbbr{'+13', '+13'}
}
