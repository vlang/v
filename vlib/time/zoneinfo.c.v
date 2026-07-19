@[has_globals]
module time

import compress.szip
import os

const zoneinfo_unix_sources = [
	'/usr/share/zoneinfo',
	'/usr/share/lib/zoneinfo',
	'/usr/lib/locale/TZ',
	'/etc/zoneinfo',
]!

const zoneinfo_vroot_zip = os.join_path(@VEXEROOT, 'vlib', 'time', 'tzdata', 'zoneinfo.zip')

pub type ZoneinfoLoaderFn = fn (name string) ![]u8

__global zoneinfo_loaders = []ZoneinfoLoaderFn{}

// register_zoneinfo_loader registers a fallback loader for IANA time zone data.
// Registered loaders are used after ZONEINFO, system zoneinfo paths, and V's
// installed zoneinfo.zip have been tried.
pub fn register_zoneinfo_loader(loader ZoneinfoLoaderFn) {
	zoneinfo_loaders << loader
}

// Zone describes one time zone rule in an IANA location.
pub struct Zone {
pub:
	name   string
	offset int
	is_dst bool
}

struct ZoneTransition {
	when  i64
	index int
}

// Location contains parsed IANA time zone data.
//
// A `Time` with a non-none `location()` carries IANA zone rules. Prefer that
// over the older `is_local` flag, which only means "system local wall time
// with a fixed process offset" and does not model DST transitions by name.
pub struct Location {
pub:
	name string
mut:
	zones       []Zone
	transitions []ZoneTransition
	posix       PosixZoneRule
	has_posix   bool
}

struct PosixZoneRule {
	std_name   string
	std_offset int
	dst_name   string
	dst_offset int
	start      PosixRule
	end        PosixRule
	has_dst    bool
}

enum PosixRuleKind {
	month_week_day
	julian_no_leap
	day_of_year
}

struct PosixRule {
	kind    PosixRuleKind
	month   int
	week    int
	weekday int
	day     int
	seconds int
}

// load_location loads an IANA time zone location from ZONEINFO, system zoneinfo
// paths, V's installed zoneinfo.zip, or registered fallback loaders.
pub fn load_location(name string) !&Location {
	if name == '' || name.contains('..') || name.starts_with('/') || name.starts_with('\\') {
		return error('invalid time zone location "${name}"')
	}
	if name == 'Local' {
		return local_location()
	}
	if name in ['UTC', 'Etc/UTC', 'Etc/GMT', 'GMT'] {
		return &Location{
			name:  name
			zones: [Zone{
				name:   'UTC'
				offset: 0
			}]
		}
	}
	data := load_zoneinfo_data(name)!
	return parse_tzif_location(name, data)!
}

// zone_at returns the active zone rule for `unix_time`.
pub fn (loc &Location) zone_at(unix_time i64) !Zone {
	if loc.zones.len == 0 {
		return error('time zone location "${loc.name}" has no zone rules')
	}
	if loc.has_posix
		&& (loc.transitions.len == 0 || unix_time >= loc.transitions[loc.transitions.len - 1].when) {
		return loc.posix.zone_at(unix_time)
	}
	if loc.transitions.len == 0 {
		return loc.zones[0]
	}
	if unix_time < loc.transitions[0].when {
		return loc.zones[loc.first_standard_zone_index()]
	}
	mut lo := 0
	mut hi := loc.transitions.len
	for lo < hi {
		mid := lo + (hi - lo) / 2
		if loc.transitions[mid].when <= unix_time {
			lo = mid + 1
		} else {
			hi = mid
		}
	}
	tx := loc.transitions[lo - 1]
	if tx.index < 0 || tx.index >= loc.zones.len {
		return error('time zone location "${loc.name}" has an invalid transition')
	}
	return loc.zones[tx.index]
}

// offset_at returns the UTC offset in seconds for `unix_time` in the location.
pub fn (loc &Location) offset_at(unix_time i64) !int {
	return (loc.zone_at(unix_time)!).offset
}

// unix_to_local resolves a Unix timestamp to a Time in the location.
pub fn (loc &Location) unix_to_local(unix_time i64) !Time {
	return loc.unix_nanosecond_to_local(unix_time, 0)
}

// unix_nanosecond_to_local resolves a Unix timestamp and nanosecond to a Time
// in the location.
// The returned `Time` keeps `unix` as the absolute UTC epoch instant; the
// calendar fields are the wall time in `loc`. `is_local` stays false.
pub fn (loc &Location) unix_nanosecond_to_local(unix_time i64, nanosecond int) !Time {
	zone := loc.zone_at(unix_time)!
	local := unix_nanosecond(unix_time + i64(zone.offset), nanosecond)
	return Time{
		loc:        *loc
		unix:       unix_time
		year:       local.year
		month:      local.month
		day:        local.day
		hour:       local.hour
		minute:     local.minute
		second:     local.second
		nanosecond: nanosecond
	}
}

// in resolves `t` as a Time in `loc`.
pub fn (t Time) in(loc &Location) !Time {
	return loc.unix_nanosecond_to_local(t.unix(), t.nanosecond)
}

// location returns the IANA location associated with `t`, if any.
pub fn (t Time) location() ?Location {
	return t.loc
}

// zone returns the active zone rule for `t`, if it has an IANA location.
pub fn (t Time) zone() !Zone {
	loc := t.loc or { return error('time has no IANA location') }
	return loc.zone_at(t.unix())
}

fn (loc &Location) first_standard_zone_index() int {
	for i, zone in loc.zones {
		if !zone.is_dst {
			return i
		}
	}
	return 0
}

fn load_zoneinfo_data(name string) ![]u8 {
	zoneinfo := os.getenv('ZONEINFO')
	if zoneinfo != '' {
		if data := load_zoneinfo_from_source(zoneinfo, name) {
			return data
		}
	}
	for source in platform_zoneinfo_sources() {
		if data := load_zoneinfo_from_source(source, name) {
			return data
		}
	}
	for loader in zoneinfo_loaders {
		if data := loader(name) {
			return data
		}
	}
	return error('unknown time zone location "${name}"')
}

fn platform_zoneinfo_sources() []string {
	mut sources := []string{}
	$if !windows {
		for source in zoneinfo_unix_sources {
			sources << source
		}
	}
	sources << zoneinfo_vroot_zip
	return sources
}

fn load_zoneinfo_from_source(source string, name string) ![]u8 {
	if os.is_dir(source) {
		return os.read_bytes(os.join_path(source, name))
	}
	if os.is_file(source) {
		return read_zoneinfo_zip_entry(source, name)
	}
	return error('time zone source "${source}" does not exist')
}

fn read_zoneinfo_zip_entry(zip_path string, name string) ![]u8 {
	mut zip := szip.open(zip_path, .no_compression, .read_only)!
	defer {
		zip.close()
	}
	// miniz open_entry can succeed with size 0 for missing names; treat that
	// as unknown so callers keep searching other sources.
	zip.open_entry(name) or { return error('unknown time zone location "${name}"') }
	defer {
		zip.close_entry()
	}
	size := int(zip.size())
	if size <= 0 {
		return error('unknown time zone location "${name}"')
	}
	mut data := []u8{len: size}
	zip.read_entry_buf(data.data, size)!
	return data
}

fn parse_tzif_location(name string, data []u8) !&Location {
	mut pos := 0
	header := read_tzif_header(data, pos)!
	pos += 44
	if header.version == `2` || header.version == `3` || header.version == `4` {
		size := tzif_data_size(header, 4)!
		if pos + size > data.len {
			return error('truncated TZif data for "${name}"')
		}
		pos += size
		header2 := read_tzif_header(data, pos)!
		pos += 44
		return parse_tzif_data(name, data, pos, header2, 8)!
	}
	return parse_tzif_data(name, data, pos, header, 4)!
}

struct TzifHeader {
	version   u8
	ttisgmt   int
	ttisstd   int
	leap      int
	time      int
	typ       int
	character int
}

fn read_tzif_header(data []u8, offset int) !TzifHeader {
	if offset + 44 > data.len || data[offset..offset + 4].bytestr() != 'TZif' {
		return error('invalid TZif data')
	}
	header := TzifHeader{
		version:   data[offset + 4]
		ttisgmt:   read_be_i32(data, offset + 20)
		ttisstd:   read_be_i32(data, offset + 24)
		leap:      read_be_i32(data, offset + 28)
		time:      read_be_i32(data, offset + 32)
		typ:       read_be_i32(data, offset + 36)
		character: read_be_i32(data, offset + 40)
	}
	if header.ttisgmt < 0 || header.ttisstd < 0 || header.leap < 0 || header.time < 0
		|| header.typ < 0 || header.character < 0 {
		return error('invalid TZif header counts')
	}
	return header
}

fn tzif_data_size(header TzifHeader, time_size int) !int {
	size := i64(header.time) * i64(time_size) + i64(header.time) + i64(header.typ) * 6 +
		i64(header.character) + i64(header.leap) * i64(time_size + 4) + i64(header.ttisstd) +
		i64(header.ttisgmt)
	if size > i64(max_int) {
		return error('TZif data is too large')
	}
	return int(size)
}

fn parse_tzif_data(name string, data []u8, start int, header TzifHeader, time_size int) !&Location {
	if header.typ <= 0 {
		return error('time zone location "${name}" has no zone rules')
	}
	size := tzif_data_size(header, time_size)!
	if start + size > data.len {
		return error('truncated TZif data for "${name}"')
	}
	mut pos := start
	mut times := []i64{len: header.time}
	for i in 0 .. header.time {
		times[i] = if time_size == 8 {
			read_be_i64(data, pos)
		} else {
			i64(read_be_i32(data, pos))
		}
		pos += time_size
	}
	mut indices := []int{len: header.time}
	for i in 0 .. header.time {
		indices[i] = int(data[pos])
		pos++
	}
	mut zone_offsets := []int{len: header.typ}
	mut zone_is_dst := []bool{len: header.typ}
	mut zone_abbr_indices := []int{len: header.typ}
	for i in 0 .. header.typ {
		zone_offsets[i] = read_be_i32(data, pos)
		zone_is_dst[i] = data[pos + 4] != 0
		zone_abbr_indices[i] = int(data[pos + 5])
		pos += 6
	}
	abbreviations := data[pos..pos + header.character]
	pos += header.character
	mut zones := []Zone{cap: header.typ}
	for i in 0 .. header.typ {
		zones << Zone{
			name:   tzif_abbreviation(abbreviations, zone_abbr_indices[i])
			offset: zone_offsets[i]
			is_dst: zone_is_dst[i]
		}
	}
	mut transitions := []ZoneTransition{cap: header.time}
	for i in 0 .. header.time {
		if indices[i] >= zones.len {
			return error('invalid TZif transition in "${name}"')
		}
		transitions << ZoneTransition{
			when:  times[i]
			index: indices[i]
		}
	}
	pos += header.leap * (time_size + 4) + header.ttisstd + header.ttisgmt
	posix, has_posix := parse_posix_tail(data, pos)
	return &Location{
		name:        name
		zones:       zones
		transitions: transitions
		posix:       posix
		has_posix:   has_posix
	}
}

fn parse_posix_tail(data []u8, start int) (PosixZoneRule, bool) {
	if start >= data.len || data[start] != `\n` {
		return PosixZoneRule{}, false
	}
	mut end := start + 1
	for end < data.len && data[end] != `\n` {
		end++
	}
	if end <= start + 1 {
		return PosixZoneRule{}, false
	}
	rule := parse_posix_zone_rule(data[start + 1..end].bytestr()) or {
		return PosixZoneRule{}, false
	}
	return rule, true
}

fn parse_posix_zone_rule(text string) !PosixZoneRule {
	std_name, mut pos := parse_posix_name(text, 0)!
	std_offset, next_pos := parse_posix_offset(text, pos)!
	pos = next_pos
	if pos == text.len {
		return PosixZoneRule{
			std_name:   std_name
			std_offset: std_offset
		}
	}
	dst_name, dst_name_end := parse_posix_name(text, pos)!
	pos = dst_name_end
	mut dst_offset := std_offset + seconds_per_hour
	if pos < text.len && text[pos] != `,` {
		parsed_dst_offset, offset_end := parse_posix_offset(text, pos)!
		dst_offset = parsed_dst_offset
		pos = offset_end
	}
	if pos >= text.len || text[pos] != `,` {
		return error('unsupported POSIX time zone rule "${text}"')
	}
	parts := text[pos + 1..].split(',')
	if parts.len != 2 {
		return error('unsupported POSIX time zone rule "${text}"')
	}
	return PosixZoneRule{
		std_name:   std_name
		std_offset: std_offset
		dst_name:   dst_name
		dst_offset: dst_offset
		start:      parse_posix_rule(parts[0])!
		end:        parse_posix_rule(parts[1])!
		has_dst:    true
	}
}

fn parse_posix_name(text string, start int) !(string, int) {
	if start >= text.len {
		return error('missing POSIX time zone name')
	}
	if text[start] == `<` {
		mut pos := start + 1
		for pos < text.len && text[pos] != `>` {
			pos++
		}
		if pos >= text.len {
			return error('unterminated POSIX time zone name')
		}
		return text[start + 1..pos], pos + 1
	}
	mut pos := start
	for pos < text.len && ((text[pos] >= `A` && text[pos] <= `Z`)
		|| (text[pos] >= `a` && text[pos] <= `z`)) {
		pos++
	}
	if pos == start {
		return error('missing POSIX time zone name')
	}
	return text[start..pos], pos
}

fn parse_posix_offset(text string, start int) !(int, int) {
	mut pos := start
	mut sign := -1
	if pos < text.len && text[pos] == `-` {
		sign = 1
		pos++
	} else if pos < text.len && text[pos] == `+` {
		pos++
	}
	hour_value, hour_end := parse_posix_number(text, pos)!
	pos = hour_end
	mut minute_value := 0
	mut second_value := 0
	if pos < text.len && text[pos] == `:` {
		minute_value, pos = parse_posix_number(text, pos + 1)!
	}
	if pos < text.len && text[pos] == `:` {
		second_value, pos = parse_posix_number(text, pos + 1)!
	}
	return sign * (hour_value * seconds_per_hour + minute_value * seconds_per_minute + second_value), pos
}

fn parse_posix_rule(text string) !PosixRule {
	rule_parts := text.split('/')
	if rule_parts.len == 0 || rule_parts.len > 2 {
		return error('unsupported POSIX time zone date rule "${text}"')
	}
	date := rule_parts[0]
	seconds := if rule_parts.len > 1 {
		parse_posix_time(rule_parts[1])!
	} else {
		2 * seconds_per_hour
	}
	if date.starts_with('M') {
		date_parts := date[1..].split('.')
		if date_parts.len != 3 {
			return error('unsupported POSIX time zone date rule "${text}"')
		}
		month := parse_posix_number_text(date_parts[0])!
		week := parse_posix_number_text(date_parts[1])!
		weekday := parse_posix_number_text(date_parts[2])!
		if month < 1 || month > 12 || week < 1 || week > 5 || weekday < 0 || weekday > 6 {
			return error('unsupported POSIX time zone date rule "${text}"')
		}
		return PosixRule{
			kind:    .month_week_day
			month:   month
			week:    week
			weekday: weekday
			seconds: seconds
		}
	}
	if date.starts_with('J') {
		day := parse_posix_number_text(date[1..])!
		if day < 1 || day > 365 {
			return error('unsupported POSIX time zone date rule "${text}"')
		}
		return PosixRule{
			kind:    .julian_no_leap
			day:     day
			seconds: seconds
		}
	}
	day := parse_posix_number_text(date)!
	if day < 0 || day > 365 {
		return error('unsupported POSIX time zone date rule "${text}"')
	}
	return PosixRule{
		kind:    .day_of_year
		day:     day
		seconds: seconds
	}
}

fn posix_rule_month_day(year int, rule PosixRule) (int, int) {
	match rule.kind {
		.month_week_day {
			return rule.month, posix_month_week_day(year, rule)
		}
		.julian_no_leap {
			mut ordinal := rule.day
			if is_leap_year(year) && ordinal >= 60 {
				ordinal++
			}
			return month_day_from_year_day(year, ordinal)
		}
		.day_of_year {
			return month_day_from_year_day(year, rule.day + 1)
		}
	}
}

fn month_day_from_year_day(year int, ordinal int) (int, int) {
	mut day := ordinal
	for month in 1 .. 13 {
		days := month_days[month - 1] + if month == 2 && is_leap_year(year) { 1 } else { 0 }
		if day <= days {
			return month, day
		}
		day -= days
	}
	return 12, 31
}

fn parse_posix_time(text string) !int {
	if text == '' {
		return error('unsupported POSIX time "${text}"')
	}
	mut pos := 0
	mut sign := 1
	if text[pos] == `-` {
		sign = -1
		pos++
	} else if text[pos] == `+` {
		pos++
	}
	hour_value, hour_end := parse_posix_number(text, pos)!
	pos = hour_end
	mut minute_value := 0
	mut second_value := 0
	if pos < text.len && text[pos] == `:` {
		minute_value, pos = parse_posix_number(text, pos + 1)!
	}
	if pos < text.len && text[pos] == `:` {
		second_value, pos = parse_posix_number(text, pos + 1)!
	}
	if pos != text.len || minute_value > 59 || second_value > 59 {
		return error('unsupported POSIX time "${text}"')
	}
	seconds := hour_value * seconds_per_hour + minute_value * seconds_per_minute + second_value
	if seconds > 167 * seconds_per_hour {
		return error('unsupported POSIX time "${text}"')
	}
	return sign * seconds
}

fn parse_posix_number(text string, start int) !(int, int) {
	mut pos := start
	for pos < text.len && text[pos] >= `0` && text[pos] <= `9` {
		pos++
	}
	if pos == start {
		return error('missing POSIX number')
	}
	return text[start..pos].int(), pos
}

fn parse_posix_number_text(text string) !int {
	value, pos := parse_posix_number(text, 0)!
	if pos != text.len {
		return error('missing POSIX number')
	}
	return value
}

fn (rule PosixZoneRule) zone_at(unix_time i64) Zone {
	if !rule.has_dst {
		return Zone{
			name:   rule.std_name
			offset: rule.std_offset
		}
	}
	year := unix(unix_time).year
	start := rule.transition_utc(year, rule.start, rule.std_offset)
	end := rule.transition_utc(year, rule.end, rule.dst_offset)
	is_dst := if start <= end {
		unix_time >= start && unix_time < end
	} else {
		unix_time >= start || unix_time < end
	}
	if is_dst {
		return Zone{
			name:   rule.dst_name
			offset: rule.dst_offset
			is_dst: true
		}
	}
	return Zone{
		name:   rule.std_name
		offset: rule.std_offset
	}
}

fn (rule PosixZoneRule) transition_utc(year int, date_rule PosixRule, offset_before int) i64 {
	month, day := posix_rule_month_day(year, date_rule)
	local := time_fields_to_unix(Time{
		year:  year
		month: month
		day:   day
	})
	return local + i64(date_rule.seconds) - i64(offset_before)
}

fn posix_month_week_day(year int, rule PosixRule) int {
	first_weekday := day_of_week(year, rule.month, 1) % 7
	mut day := 1 + ((rule.weekday - first_weekday + 7) % 7) + (rule.week - 1) * 7
	days := days_in_month(rule.month, year) or { 31 }
	if rule.week == 5 {
		for day + 7 <= days {
			day += 7
		}
		if day > days {
			day -= 7
		}
	}
	return day
}

fn tzif_abbreviation(data []u8, start int) string {
	if start < 0 || start >= data.len {
		return ''
	}
	mut end := start
	for end < data.len && data[end] != 0 {
		end++
	}
	return data[start..end].bytestr()
}

fn read_be_i32(data []u8, offset int) int {
	mut value := u32(0)
	for i in 0 .. 4 {
		value = (value << 8) | u32(data[offset + i])
	}
	if value & 0x8000_0000 != 0 {
		return -int((~value) + 1)
	}
	return int(value)
}

fn read_be_i64(data []u8, offset int) i64 {
	mut value := u64(0)
	for i in 0 .. 8 {
		value = (value << 8) | u64(data[offset + i])
	}
	if value & 0x8000_0000_0000_0000 != 0 {
		return -i64((~value) + 1)
	}
	return i64(value)
}
