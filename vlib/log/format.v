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
