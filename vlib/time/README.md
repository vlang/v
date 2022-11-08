# time

A time library

## Features
TODO

## Description:

`time` provides utilities for working with time and dates:
- parsing of time values expressed in one of the commonly used standard time/date formats
- formatting of time values
- arithmetic over times/durations
- converting between local time and UTC (timezone support)
- stop watches for accurately measuring time durations
- sleeping for a period of time

**Example:**
time can be comparable

see: [V Playground](https://play.vlang.io/p/c121a6dda7)
```v
import time

println(time.now())
```
### format

**v doc:**
```v ignore
fn (t Time) format() string
fn (t Time) format_ss() string
fn (t Time) format_ss_milli() string
fn (t Time) format_ss_micro() string
```

**Example:**

```v
assert time_to_test.format() =='1980-07-11 21:23'
assert time_to_test.format_ss() =='1980-07-11 21:23:42'
assert time_to_test.format_ss_milli() =='1980-07-11 21:23:42.123'
assert time_to_test.format_ss_micro() =='1980-07-11 21:23:42.123456'
```

### parse

**v doc:**
```v ignore
fn parse(s string) !Time
fn parse_iso8601(s string) !Time
fn parse_rfc2822(s string) !Time
fn parse_rfc3339(s string) !Time
```

**Example:**

see: [V Playground](https://play.vlang.io/p/b02ca6027f)
```v
import time

s := '2018-01-27 12:48:34'
t := time.parse(s) or {
    panic('> failing format: $s | err: $err')
}
println(t)
println(t.unix)
```
### stopwatch

**Example:**

```v

```
### unix

**Example:**

```v

```
### chrono
// days_from_civil - return the number of days since the
// Unix epoch 1970-01-01. A detailed description of the algorithm here
// is in: http://howardhinnant.github.io/date_algorithms.html
// Note that it will return negative values for days before 1970-01-01.