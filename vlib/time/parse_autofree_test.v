// vtest build: !msvc-windows
// vtest vflags: -autofree
module time

fn test_parse_format_regression_21369_autofree() {
	for _ in 0 .. 1000 {
		t := parse_format('04 Oct 2023', 'DD MMM YYYY') or {
			panic('parse_format should succeed under -autofree')
		}
		assert t.year == 2023
		assert t.month == 10
		assert t.day == 4
		assert t.hour == 0
		assert t.minute == 0
		assert t.second == 0
	}
}
