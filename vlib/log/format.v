// Shared formatting helpers for the built-in handlers.
module log

import time

// format_time renders `t` using one of the predefined log time formats.
fn format_time(t time.Time, f TimeFormat, custom string) string {
	return match f {
		.tf_rfc3339_micro { t.format_rfc3339_micro() }
		.tf_ss_micro { t.format_ss_micro() }
		.tf_default { t.format() }
		.tf_ss { t.format_ss() }
		.tf_ss_milli { t.format_ss_milli() }
		.tf_ss_nano { t.format_ss_nano() }
		.tf_rfc3339 { t.format_rfc3339() }
		.tf_rfc3339_nano { t.format_rfc3339_nano() }
		.tf_hhmm { t.hhmm() }
		.tf_hhmmss { t.hhmmss() }
		.tf_hhmm12 { t.hhmm12() }
		.tf_ymmdd { t.ymmdd() }
		.tf_ddmmy { t.ddmmy() }
		.tf_md { t.md() }
		.tf_custom_format { t.custom_format(custom) }
	}
}

fn append_str(mut buf []u8, s string) {
	buf << s.bytes()
}

fn append_byte(mut buf []u8, s string) {
	buf << s.bytes()
}

fn append_2(mut buf []u8, n int) {
	buf << u8(48 + (n / 10) % 10)
	buf << u8(48 + n % 10)
}

fn append_3(mut buf []u8, n int) {
	buf << u8(48 + (n / 100) % 10)
	buf << u8(48 + (n / 10) % 10)
	buf << u8(48 + n % 10)
}

fn append_4(mut buf []u8, n int) {
	buf << u8(48 + (n / 1000) % 10)
	buf << u8(48 + (n / 100) % 10)
	buf << u8(48 + (n / 10) % 10)
	buf << u8(48 + n % 10)
}

fn append_6(mut buf []u8, n int) {
	buf << u8(48 + (n / 100000) % 10)
	buf << u8(48 + (n / 10000) % 10)
	buf << u8(48 + (n / 1000) % 10)
	buf << u8(48 + (n / 100) % 10)
	buf << u8(48 + (n / 10) % 10)
	buf << u8(48 + n % 10)
}

fn append_ymd(mut buf []u8, t time.Time) {
	append_4(mut buf, t.year)
	buf << u8(45)
	append_2(mut buf, t.month)
	buf << u8(45)
	append_2(mut buf, t.day)
}

fn append_hms(mut buf []u8, t time.Time) {
	append_2(mut buf, t.hour)
	buf << u8(58)
	append_2(mut buf, t.minute)
	buf << u8(58)
	append_2(mut buf, t.second)
}

fn timestamp_direct_supported(f TimeFormat) bool {
	return f in [
		.tf_rfc3339_micro,
		.tf_ss_micro,
		.tf_ss_milli,
		.tf_ss,
		.tf_default,
		.tf_ymmdd,
		.tf_hhmmss,
	]
}

// append_timestamp renders the common formats directly into `buf`, avoiding
// the temporary string that format_time allocates. It returns false for
// formats that still need the generic path.
fn append_timestamp(mut buf []u8, t time.Time, f TimeFormat, local bool) bool {
	// Only the RFC3339 formats carry a Z suffix and therefore need UTC. The
	// legacy formatter renders the other formats from the wall-clock fields as
	// supplied; converting them here would change local_time output.
	u := if local && t.is_local && f == .tf_rfc3339_micro { t.local_to_utc() } else { t }
	match f {
		.tf_rfc3339_micro {
			append_ymd(mut buf, u)
			buf << u8(84)
			append_hms(mut buf, u)
			buf << u8(46)
			append_6(mut buf, u.nanosecond / 1000)
			buf << u8(90)
		}
		.tf_ss_micro {
			append_ymd(mut buf, u)
			buf << u8(32)
			append_hms(mut buf, u)
			buf << u8(46)
			append_6(mut buf, u.nanosecond / 1000)
		}
		.tf_ss_milli {
			append_ymd(mut buf, u)
			buf << u8(32)
			append_hms(mut buf, u)
			buf << u8(46)
			append_3(mut buf, u.nanosecond / 1000000)
		}
		.tf_ss {
			append_ymd(mut buf, u)
			buf << u8(32)
			append_hms(mut buf, u)
		}
		.tf_default {
			append_4(mut buf, u.year)
			buf << u8(45)
			append_2(mut buf, u.month)
			buf << u8(45)
			append_2(mut buf, u.day)
			buf << u8(32)
			append_2(mut buf, u.hour)
			buf << u8(58)
			append_2(mut buf, u.minute)
		}
		.tf_ymmdd {
			append_ymd(mut buf, u)
		}
		.tf_hhmmss {
			append_hms(mut buf, u)
		}
		else {
			return false
		}
	}
	return true
}
