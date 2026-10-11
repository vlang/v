## Description

V's `time` module, provides utilities for working with time and dates:

- parsing of time values expressed in one of the commonly used standard time/date formats
- formatting of time values
- arithmetic over times/durations
- converting between local time and UTC (timezone support)
- stop watches for accurately measuring time durations
- sleeping for a period of time

`time.ticks()` returns milliseconds since the UNIX epoch on Unix platforms. On Windows it
returns a 64-bit count of milliseconds since system startup, which does not wrap after
49.7 days. The Windows counter is resolved at runtime so it also works with bundled TCC
versions whose import library does not list `GetTickCount64`.

## Examples

You can see the current time. [See](https://play.vlang.io/?query=c121a6dda7):

```v
import time

println(time.now())
```

`time.Time` values can be compared, [see](https://play.vlang.io/?query=133d1a0ce5):

```v
import time

const time_to_test = time.Time{
	year:       1980
	month:      7
	day:        11
	hour:       21
	minute:     23
	second:     42
	nanosecond: 123456789
}

println(time_to_test.format())

assert '1980-07-11 21:23' == time_to_test.format()
assert '1980-07-11 21:23:42' == time_to_test.format_ss()
assert '1980-07-11 21:23:42.123' == time_to_test.format_ss_milli()
assert '1980-07-11 21:23:42.123456' == time_to_test.format_ss_micro()
assert '1980-07-11 21:23:42.123456789' == time_to_test.format_ss_nano()
```

You can also parse strings to produce time.Time values,
[see](https://play.vlang.io/p/b02ca6027f):

```v
import time

s := '2018-01-27 12:48:34'
t := time.parse(s) or { panic('failing format: ${s} | err: ${err}') }
println(t)
println(t.unix())
```

V's time module also has these parse methods:

Month and weekday names in `parse_format` may end the input. For example,
`time.parse_format('May', 'MMMM')!` and `time.parse_format('Jul', 'MMM')!`
return times in May and July, respectively; weekday tokens also accept a terminal name.

`parse_format(s, format)` requires the format to cover the entire input, including literals.
Unmatched trailing text or whitespace returns an error instead of parsing only a prefix.

`parse_format` defaults an omitted month to January. Day-only layouts such as
`time.parse_format('31', 'DD')!` therefore accept January 31; an explicit month still
enforces its actual length.

`Time.custom_format('YYYY')` pads nonnegative years to at least four digits, so year `100`
is written as `0100`. For nonnegative years, `YY` writes the final two year digits
with leading zeros. Negative years retain their existing `YYYY` and `YY` representations.

`parse_format` supports `A` for `AM`/`PM` and `a` for `am`/`pm`. Use these markers with
an hour from `1` to `12`, for example `time.parse_format('02:30:45PM', 'hh:mm:ssA')!`
returns hour `14`, while `12:00:00AM` returns hour `0`. Layouts without a marker retain
their existing 24-hour behavior.

```v ignore
fn parse(s string) !Time
fn parse_iso8601(s string) !Time
fn parse_rfc2822(s string) !Time
fn parse_rfc3339(s string) !Time
```

`time.new(...)` validates the provided fields before calculating the Unix timestamp.
Omitted `month` and `day` values default to `1`, and out-of-range values panic.
Use `t.is_zero()` to check whether a `time.Time` is still its zero value before formatting or
serializing it.

IANA time zone data can be loaded by name and used to convert Unix timestamps
or UTC `Time` values to that location's calendar time. `load_location` searches
`ZONEINFO`, system zoneinfo paths, and V's installed `zoneinfo.zip`. Programs
that need embedded time zone data (no system zoneinfo, portable binaries) can
import `time.tzdata`.

```v
import time
import time.tzdata as _

shanghai := time.load_location('Asia/Shanghai')!
local := time.unix(1_704_067_200).in(shanghai)!
assert local.format_ss() == '2024-01-01 08:00:00'
assert local.unix() == 1_704_067_200
assert (local.zone()!).offset == 28_800
```

IANA-zoned values keep `unix` as the absolute UTC epoch instant. Calendar
fields (`year`, `hour`, ...) are wall time in that location. Prefer
`t.location()` / `t.zone()` over the older `is_local` flag: `is_local` only
marks system-local wall time with a fixed process offset and is left `false`
on IANA-zoned `Time` values.

`parse_rfc3339` and `parse_iso8601` keep a non-zero numeric UTC offset in the
same way: the calendar fields stay as written, and `t.zone()` returns a fixed
zone with that offset (in seconds east of UTC). Inputs ending in `Z`, `+00:00`
or `-00:00` give plain UTC values without a location.

```v
import time

t := time.parse_rfc3339('2024-07-15T18:30:45-05:00')!
assert t.format_ss() == '2024-07-15 18:30:45'
assert (t.zone()!).offset == -18_000
assert t.format_rfc3339() == '2024-07-15T23:30:45.000Z'
```

The bundled `vlib/time/tzdata/zoneinfo.zip` is a store-only (uncompressed) zip
of IANA zoneinfo files for offline use via `import time.tzdata`. Refresh it
from a full IANA tzdb source archive with packrat data enabled, so named zones
retain their pre-1970 histories. For example, from an extracted tzdb source
archive:

```sh
make PACKRATDATA=backzone PACKRATLIST= ZFLAGS='-b slim' \
  DESTDIR=/tmp/tzdb-full TZDIR=/zoneinfo posix_only
cd /tmp/tzdb-full/zoneinfo
find . -type f -print | LC_ALL=C sort | sed 's#^./##' | \
  zip -0 -X /path/to/v/vlib/time/tzdata/zoneinfo.zip -@
```

`time.parse_duration(s)` parses a string such as `1h30m`, `-1.5s` or `300ms` into a
`time.Duration`. The format is the one of Go's `time.ParseDuration`: an optional sign,
followed by one or more decimal numbers, each with an optional fraction and a unit. The
units are `ns`, `us` (or `µs`), `ms`, `s`, `m` and `h`; only `0` can be written without one.
A string that is not a duration, a number without a unit, an unknown unit such as `d`, and
a duration that does not fit in the `i64` range of nanoseconds are reported as errors:

```v
import time

timeout := time.parse_duration('1h30m')!
assert timeout == 90 * time.minute
assert time.parse_duration('-1.5s')! == -1500 * time.millisecond
assert time.parse_duration('2h45m30.5s')!.seconds() == 9930.5

time.parse_duration('5') or { assert err.msg() == 'missing unit in duration: "5"' }
time.parse_duration('1d') or { assert err.msg() == 'unknown unit "d" in duration: "1d"' }
```

`Duration.str()` is meant for display. For a minute or more it writes forms such as
`1:30:00`, which are not duration strings, so `parse_duration` does not read them back.

Another very useful feature of the `time` module is the stop watch,
for when you want to measure short time periods, elapsed while you
executed other tasks. [See](https://play.vlang.io/?query=f6c008bc34):

```v
import time

fn do_something() {
	time.sleep(510 * time.millisecond)
}

fn main() {
	sw := time.new_stopwatch()
	do_something()
	println('Note: do_something() took: ${sw.elapsed().milliseconds()} ms')
}
```

A wait that needs to participate in a `select` uses `sync.new_timer`, which sends the
time on a channel: see the `sync` module.
