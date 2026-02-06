module time

// parse returns the time from a date string.
//
// TODO(playX): JS Date expects iso8061 format of strings and other formats
// are implementation dependent, we probably want to implement parsing in JS.
pub fn parse(s string) Time {
	mut res := Time{}
	#let date = new Date(s.str)
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

// parse_iso8601 parses the ISO 8601 time format yyyy-MM-ddTHH:mm:ss.dddddd+dd:dd as local time.
// The fraction part is difference in milli seconds, and the last part is offset from UTC time.
// Both can be +/- HH:mm .
// See https://en.wikipedia.org/wiki/ISO_8601 .
// Remarks: not all of ISO 8601 is supported; checks and support for leapseconds should be added.
pub fn parse_iso8601(s string) !Time {
	return parse(s)
}
