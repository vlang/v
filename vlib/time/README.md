## Description:

V's `time` module, provides utilities for working with time and dates:
- parsing of time values expressed in one of the commonly used standard time/date formats
- formatting of time values
- arithmetic over times/durations
- converting between local time and UTC (timezone support)
- stop watches for accurately measuring time durations
- sleeping for a period of time

## Examples:

You can see the current time. [See](https://play.vlang.io/?query=c121a6dda7):
```v
import time

println(time.now())
```

`time.Time` values can be compared, [see](https://play.vlang.io/?query=133d1a0ce5):
```v
import time

const time_to_test = time.Time{
	year: 1980
	month: 7
	day: 11
	hour: 21
	minute: 23
	second: 42
	microsecond: 123456
	unix: 332198622
}

println(time_to_test.format())

assert '1980-07-11 21:23' == time_to_test.format()
assert '1980-07-11 21:23:42' == time_to_test.format_ss()
assert '1980-07-11 21:23:42.123' == time_to_test.format_ss_milli()
assert '1980-07-11 21:23:42.123456' == time_to_test.format_ss_micro()
```

You can also parse strings to produce time.Time values,
[see](https://play.vlang.io/p/b02ca6027f):
```v
import time

s := '2018-01-27 12:48:34'
t := time.parse(s) or { panic('failing format: ${s} | err: ${err}') }
println(t)
println(t.unix)
```

V's time module also has these parse methods:
```v ignore
fn parse(s string) !Time
fn parse_iso8601(s string) !Time
fn parse_rfc2822(s string) !Time
fn parse_rfc3339(s string) !Time
```

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
