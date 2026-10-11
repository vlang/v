module progress

import time

fn test_fmt_rate() {
	u := RateUnit{}
	assert fmt_rate(0.0, u) == '  0.0it/s'
	assert fmt_rate(-3.0, u) == '  0.0it/s'
	assert fmt_rate(0.5, u) == '  0.5it/s' // regression: used to print 499.9it/s
	assert fmt_rate(5.0, u) == '  5.0it/s'
	assert fmt_rate(1500.0, u) == '  1.5kit/s'
	assert fmt_rate(2.0e10, u) == ' 20.0Git/s'
	// rounding must not produce "1000.0" with the wrong prefix
	assert fmt_rate(999.96, u) == '  1.0kit/s'
	// beyond the last prefix: clamp, don't index out of range
	assert fmt_rate(1.0e40, u).contains('Qit/s')
	assert fmt_rate(2.0, RateUnit{
		period:        time.minute
		period_string: 'min'
		unit:          'B'
	}) == '120.0B/min'
}

fn test_fmt_time() {
	assert fmt_time(75 * time.second, .mmss) == '01:15'
	assert fmt_time(100 * time.minute, .mmss) == '100:00'
	assert fmt_time(3725 * time.second, .hhmmss) == '01:02:05'
	assert fmt_time(3725 * time.second, .hhmm) == '01:02'
	assert fmt_time(3725 * time.second, .s) == '3725s'
	assert fmt_time(1500 * time.millisecond, .sf) == '1.50s'
	assert fmt_time(-5 * time.second, .mmss) == '00:00'
	assert fmt_time_unknown(.mmss) == '--:--'
	assert fmt_time_unknown(.hhmmss) == '--:--:--'
}

fn test_si_and_iec_prefix_lists() {
	si := UnitPrefixList.si()
	assert si.base == 1000.0
	assert si.sizes[..4] == ['', 'k', 'M', 'G']
	assert UnitPrefixList{}.sizes == si.sizes // the zero value is SI
	iec := UnitPrefixList.iec()
	assert iec.base == 1024.0
	assert iec.sizes == ['', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi', 'Ei', 'Zi', 'Yi']
}

fn test_unit_presets() {
	assert RateUnit.items() == RateUnit{}
	assert RateUnit.items().unit == 'it'
	assert RateUnit.bytes().unit == 'B'
	assert RateUnit.bytes().prefixes.base == 1000.0
	assert RateUnit.bytes_iec().unit == 'B'
	assert RateUnit.bytes_iec().prefixes.base == 1024.0
}

fn test_fmt_rate_bytes_si() {
	u := RateUnit.bytes()
	assert fmt_rate(0.0, u) == '  0.0B/s'
	assert fmt_rate(512.0, u) == '512.0B/s'
	assert fmt_rate(1500.0, u) == '  1.5kB/s'
	assert fmt_rate(2_500_000.0, u) == '  2.5MB/s'
	assert fmt_rate(3.2e9, u) == '  3.2GB/s'
}

fn test_fmt_rate_bytes_iec() {
	u := RateUnit.bytes_iec()
	assert fmt_rate(0.0, u) == '  0.0B/s'
	assert fmt_rate(512.0, u) == '512.0B/s'
	assert fmt_rate(1024.0, u) == '  1.0KiB/s'
	assert fmt_rate(1536.0, u) == '  1.5KiB/s'
	assert fmt_rate(3.5 * 1024 * 1024, u) == '  3.5MiB/s'
	assert fmt_rate(2.0 * 1024 * 1024 * 1024, u) == '  2.0GiB/s'
	// below one: no prefix, and the scaling must agree with that
	assert fmt_rate(0.5, u) == '  0.5B/s'
	// rounding must not show "1024.0B/s": it carries into the next prefix
	assert fmt_rate(1023.96, u) == '  1.0KiB/s'
	assert fmt_rate(1023.9, u) == '1023.9B/s'
	// the last IEC prefix is Yi; larger rates stay there instead of indexing past it
	assert fmt_rate(1.0e40, u).contains('YiB/s')
}

fn test_a_custom_period_and_unit_still_work_with_prefixes() {
	u := RateUnit{
		unit:          'B'
		period:        time.minute
		period_string: 'min'
		prefixes:      UnitPrefixList.iec()
	}
	// 100 bytes per second is 6000 per minute: 5.9 KiB/min
	assert fmt_rate(100.0, u) == '  5.9KiB/min'
}
