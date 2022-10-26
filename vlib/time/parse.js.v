module time

// parse returns time from a date string.
//
// TODO(playX): JS Date expects iso8061 format of strings and other formats
// are implementation dependant, we probably want to implement parsing in JS.
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

pub fn parse_iso8601(s string) !Time {
	return parse(s)
}
