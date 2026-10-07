module progress

import math
import time

// UnitPrefixList is the set of prefixes used to scale a number, and the factor
// between neighbouring ones. The zero value is the SI list (k, M, G, ... with a
// step of 1000), so a plain `UnitPrefixList{}` is the same as UnitPrefixList.si().
pub struct UnitPrefixList {
pub mut:
	sizes []string = ['', 'k', 'M', 'G', 'T', 'P', 'E', 'Z', 'Y', 'R', 'Q']
	base  f64      = 1000.0
}

// UnitPrefixList.si returns the SI (decimal) prefixes: k, M, G, T, ... where each
// is 1000 times the previous one. This is the default.
pub fn UnitPrefixList.si() UnitPrefixList {
	return UnitPrefixList{}
}

// UnitPrefixList.iec returns the IEC (binary) prefixes: Ki, Mi, Gi, Ti, ... where
// each is 1024 times the previous one.
pub fn UnitPrefixList.iec() UnitPrefixList {
	return UnitPrefixList{
		sizes: ['', 'Ki', 'Mi', 'Gi', 'Ti', 'Pi', 'Ei', 'Zi', 'Yi']
		base:  1024.0
	}
}

// RateUnit says how a rate is displayed: `1.5kit/s` by default. The number is
// scaled with `prefixes`, then `unit` and `/period_string` are appended.
//
// There are presets for the common cases: RateUnit.items() (the default),
// RateUnit.bytes() (kB/s, MB/s, ...) and RateUnit.bytes_iec() (KiB/s, MiB/s, ...).
pub struct RateUnit {
pub mut:
	period        time.Duration = time.second
	period_string string        = 's'
	unit          string        = 'it'
	prefixes      UnitPrefixList
}

// RateUnit.items returns the default unit: items per second (`it/s`, `kit/s`, ...).
pub fn RateUnit.items() RateUnit {
	return RateUnit{}
}

// RateUnit.bytes returns bytes per second with SI prefixes (`B/s`, `kB/s`, `MB/s`,
// `GB/s`, ...), with a step of 1000.
pub fn RateUnit.bytes() RateUnit {
	return RateUnit{
		unit: 'B'
	}
}

// RateUnit.bytes_iec returns bytes per second with IEC prefixes (`B/s`, `KiB/s`,
// `MiB/s`, `GiB/s`, ...), with a step of 1024.
pub fn RateUnit.bytes_iec() RateUnit {
	return RateUnit{
		unit:     'B'
		prefixes: UnitPrefixList.iec()
	}
}

// TimeFormat selects how a duration is displayed.
pub enum TimeFormat {
	mmss   // mm:ss
	hhmm   // hh:mm
	hhmmss // hh:mm:ss
	s      // xs
	sf     // x.xxs
}

// fmt_time formats `d` (negative durations are shown as zero).
fn fmt_time(d time.Duration, f TimeFormat) string {
	ns := math.max(i64(d), i64(0))
	sec := i64(time.second)
	min := i64(time.minute)
	hr := i64(time.hour)
	return match f {
		.s {
			'${ns / sec}s'
		}
		.sf {
			'${time.Duration(ns).seconds():.2f}s'
		}
		.hhmmss {
			'${ns / hr:02}:${(ns % hr) / min:02}:${(ns % min) / sec:02}'
		}
		.hhmm {
			'${ns / hr:02}:${(ns % hr) / min:02}'
		}
		.mmss {
			'${ns / min:02}:${(ns % min) / sec:02}'
		}
	}
}

// fmt_time_unknown is the placeholder shown when a time cannot be estimated yet.
fn fmt_time_unknown(f TimeFormat) string {
	return match f {
		.s { '--s' }
		.sf { '--.--s' }
		.hhmmss { '--:--:--' }
		.hhmm, .mmss { '--:--' }
	}
}

// (The standard library has no SI-prefix formatter, hence the prefix table
// here, nor an `mm:ss` duration formatter; time.Duration.str() prints `1m5s`.)
//
// fmt_rate formats `per_sec` (items per second) in the units described by `u`,
// choosing the prefix so the number stays readable: 0.5 -> `  0.5it/s`,
// 1_500_000 -> `  1.5Mit/s`; with IEC prefixes, 1536 bytes/s -> `  1.5KiB/s`.
fn fmt_rate(per_sec f64, u RateUnit) string {
	sizes := if u.prefixes.sizes.len > 0 { u.prefixes.sizes } else { [''] }
	base := if u.prefixes.base > 1.0 { u.prefixes.base } else { 1000.0 }
	c := per_sec * f64(u.period) / f64(time.second)
	if !(c > 0.0) || !math.is_finite(c) {
		return '  0.0${u.unit}/${u.period_string}'
	}
	last := sizes.len - 1
	// Rates below 1 keep exponent 0 (no prefix) instead of going negative:
	// the exponent must be clamped *before* it is used to scale the value.
	mut e := int(math.clamp(math.floor(math.log_n(c, base)), 0.0, f64(last)))
	mut val := math.round(c / math.pow(base, f64(e)) * 10.0) / 10.0
	// Rounding can carry 999.96 up to 1000.0; show 1.0K instead.
	if val >= base && e < last {
		e++
		val = math.round(c / math.pow(base, f64(e)) * 10.0) / 10.0
	}
	return '${val:5.1f}${sizes[e]}${u.unit}/${u.period_string}'
}

// fmt_rate_unknown is shown before enough samples exist to estimate a rate.
fn fmt_rate_unknown(u RateUnit) string {
	return '  --.-${u.unit}/${u.period_string}'
}
