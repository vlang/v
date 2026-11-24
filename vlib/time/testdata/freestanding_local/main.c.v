module main

import time

struct Layout {
	prefix u8
	value  time.Time
}

fn require(ok bool, detail string) {
	if !ok { panic('freestanding local time: ' + detail) }
}

fn projected(epoch i64) time.Time { return time.unix(epoch).local() }

fn ny_checks() {
	negative := projected(-1)
	require(negative.year == 1969 && negative.month == 12 && negative.day == 31 && negative.hour == 18 && negative.unix() == -1, 'negative local epoch')
	before_epoch := time.new(time.Time{ year: 1969, month: 12, day: 31, hour: 18, minute: 59, second: 59, is_local: true })
	require(before_epoch.unix() == -1, 'negative local literal inverse')
	normalized := time.unix_nanosecond(0, -1).local()
	require(normalized.unix() == -1 && normalized.nanosecond == 999_999_999, 'negative nanosecond carry')
	first := projected(1_730_611_800)
	second := projected(1_730_615_400)
	require(first.hour == 1 && second.hour == 1 && first.minute == 30, 'fold wall fields')
	require(first.unix() == 1_730_611_800 && second.unix() == 1_730_615_400, 'fold inverse')
	require(first != second && first < second && second - first == time.hour, 'fold comparisons')
	require(first.add(time.hour) == second && first.add_seconds(3600) == second, 'fold elapsed addition')
	require(second.add(-time.hour) == first, 'fold elapsed subtraction')
	require(time.new(second).unix() == second.unix(), 'new preserves resolved fold')
	spread := time.new(time.Time{ ...second, hour: 2 })
	require(spread.unix() == 1_730_619_000 && spread.hour == 2, 'changed spread resolves new offset')
	require(second.as_utc().as_local().unix() == second.unix(), 'as_local/as_utc snapshot roundtrip')
	literal := time.new(time.Time{ year: 2024, month: 11, day: 3, hour: 1, minute: 30, is_local: true })
	require(literal.unix() == first.unix(), 'ambiguous literal chooses earliest')
	require(literal.add(time.hour).unix() == second.unix() && literal.add(time.hour).hour == 1, 'literal elapsed fold')
	spring := projected(1_710_052_200)
	require(spring.hour == 1 && spring.add(time.hour).hour == 3, 'spring elapsed hour')
	plain_spring := time.new(time.Time{ year: 2024, month: 3, day: 10, hour: 1, minute: 30, is_local: true })
	require(plain_spring.add_seconds(3600).hour == 3, 'literal elapsed gap')
	gap := time.new(time.Time{ year: 2024, month: 3, day: 10, hour: 2, minute: 30, is_local: true })
	require(gap.unix() == 1_710_055_800 && gap.local_to_utc().hour == 7, 'gap normalizes forward')
	last := time.unix_nanosecond(1_710_054_000, 0).local().add(-time.nanosecond)
	require(last.hour == 1 && last.minute == 59 && last.second == 59 && last.nanosecond == 999_999_999, 'nanosecond before gap')
	require(last.add(time.nanosecond).hour == 3, 'nanosecond over gap')
	day := projected(1_710_003_600)
	require(day.add_days(1) - day == 24 * time.hour && day.add_days(1).hour == day.hour + 1, 'add_days elapsed duration')
	require(day.add_date(0, 0, 1).unix() - day.unix() == 23 * 3600 && day.add_date(0, 0, 1).hour == day.hour, 'calendar day over DST')
	require(projected(1_710_138_600).add_date(0, 0, -1).hour == 1, 'reverse calendar gap prefers prior offset')
	require(second.add_date(0, 0, 1).add_date(0, 0, -1).unix() == second.unix(), 'calendar fold preserves standard offset')
	future := projected(1_899_356_400)
	require(future.hour == 3, 'POSIX footer future transition')
	stamp := time.unix_nanosecond(1_730_615_400, 123_000_000).local()
	require(stamp.nanosecond == 123_000_000 && stamp.unix_milli() == 1_730_615_400_123, 'nanosecond precision')
	require(stamp.local_to_utc().unix() == stamp.unix(), 'local UTC inverse')
	require(stamp.to_json() == '\"2024-11-03T06:30:00.123Z\"', 'JSON uses absolute instant')
}

fn main() {
	require(sizeof(time.Time) == 80 && __offsetof(Layout, value) == 8 && __offsetof(time.Time, is_local) == 72, 'Time size and member offsets')
	mut snapshots := 0
	$for field in time.Time.fields {
		$if field.name == 'local_offset_known' || field.name == 'local_offset_seconds' {
			snapshots++
			require(!field.is_pub && field.attrs.contains("json: '-'"), 'offset snapshot is private and omitted from JSON metadata')
		}
	}
	require(snapshots == 2, 'both offset fields visible to reflection')
	mode, _ := bf_getenv('TIME_TEST_MODE')
	match mode {
		'ny' { ny_checks() }
		'ny-file' {
			require(projected(1_730_611_800).unix() == 1_730_611_800 && projected(1_730_615_400).hour == 1, 'TZif file fold')
			require(projected(1_710_055_800).hour == 3, 'TZif file gap')
		}
		'south' {
			first := projected(1_712_417_400)
			second := projected(1_712_421_000)
			require(first.hour == 2 && second.hour == 2 && first.unix() == 1_712_417_400 && second.unix() == 1_712_421_000, 'southern fold')
			require(projected(1_728_145_800).hour == 3, 'southern gap')
		}
		'fraction' {
			require(projected(1_704_067_200).hour == 5 && projected(1_704_067_200).minute == 45, 'fractional offset')
		}
		else {
			require(projected(1_704_067_200).hour == 0 && projected(1_704_067_200).unix() == 1_704_067_200, 'UTC configuration/fallback')
		}
	}
	now := time.now()
	utc := time.utc()
	require(now.is_local && !utc.is_local && now.nanosecond >= 0 && now.nanosecond < 1_000_000_000, 'clock kind/nanoseconds')
	require(now.unix() >= utc.unix() - 1 && now.unix() <= utc.unix() + 1, 'local clock epoch')
	require(time.unix_now() >= utc.unix() - 1 && time.ticks() > 0, 'other realtime clocks')
	println('freestanding local time ok')
}
