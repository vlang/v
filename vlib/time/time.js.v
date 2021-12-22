module time

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

/// Returns local time
pub fn (t Time) local() Time {
	// TODO: Does this actually correct? JS clock is always set to timezone or no?
	// if it is not we should try to use Intl for getting local time.
	return t
}

pub fn sleep(dur Duration) {
	#let now = new Date().getTime()
	#let toWait = BigInt(dur.val) / BigInt(time__millisecond)
	#while (new Date().getTime() < now + Number(toWait)) {}
}

pub fn ticks() i64 {
	t := i64(0)
	#t.val = BigInt(new Date().getTime())

	return t
}
