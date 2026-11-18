import time

// `Weekday` shares its short name with the imported `time.Weekday` enum.
struct Weekday {
	s string
}

struct Wrap {
	day  Weekday
	tday time.Weekday
}

fn test_local_struct_shadowing_imported_enum_uses_struct_str() {
	d := Weekday{
		s: 'mon'
	}
	expected := "Weekday{\n    s: 'mon'\n}"
	assert '${d}' == expected
	assert d.str() == expected
	assert '${[d]}' == "[Weekday{\n    s: 'mon'\n}]"
	opt := ?Weekday(d)
	assert '${opt}' == "Option(Weekday{\n    s: 'mon'\n})"
	w := Wrap{
		day:  d
		tday: .tuesday
	}
	assert '${w}' == "Wrap{\n    day: Weekday{\n        s: 'mon'\n    }\n    tday: tuesday\n}"
	assert '${time.Weekday.friday}' == 'friday'
}
