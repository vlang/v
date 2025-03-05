module time

// now returns the current local time.
pub fn now() Time {
	mut res := Time{}
	#let date = new Date()
	#res.year.val = date.getFullYear()
	#res.month.val = date.getMonth()
	#res.day.val = date.getDay()
	#res.hour.val = date.getHours()
	#res.minute.val = date.getMinutes()
	#res.second.val = date.getSeconds()
	#res.microsecond.val = date.getMilliseconds() * 1000
	#res.unix.val = (date.getTime() / 1000).toFixed(0)

	return res
}

// utc returns the current UTC time.
pub fn utc() Time {
	mut res := Time{}
	#let date = new Date()
	#res.year.val = date.getUTCFullYear()
	#res.month.val = date.getUTCMonth()
	#res.day.val = date.getUTCDay()
	#res.hour.val = date.getUTCHours()
	#res.minute.val = date.getUTCMinutes()
	#res.second.val = date.getUTCSeconds()
	#res.microsecond.val = date.getUTCMilliseconds() * 1000
	#res.unix.val = (date.getTime() / 1000).toFixed(0)

	return res
}

// local returns the local time.
pub fn (t Time) local() Time {
	// TODO: Does this actually correct? JS clock is always set to timezone or no?
	// if it is not we should try to use Intl for getting local time.
	return t
}

// sleep suspends the execution for a given duration (in nanoseconds).
pub fn sleep(dur Duration) {
	#let now = new Date().getTime()
	#let toWait = BigInt(dur.val) / BigInt(time__millisecond)
	#while (new Date().getTime() < now + Number(toWait)) {}
}

fn time_with_unix(t Time) Time {
	if t.unix != 0 {
		return t
	}
	mut res := Time{}
	#res.year.val = t.year.val
	#res.month.val = t.month.val
	#res.day.val = t.day.val
	#res.hour.val = t.hour.val
	#res.minute.val = t.minute.val
	#res.second.val = t.second.val
	#res.microsecond.val = t.microsecond.val
	#res.unix.val = t.unix.val

	return res
}

// ticks returns the number of milliseconds since the UNIX epoch.
// // On Windows ticks returns the number of milliseconds elapsed since system start.
pub fn ticks() i64 {
	t := i64(0)
	#t.val = BigInt(new Date().getTime())

	return t
}
