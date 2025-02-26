type Any = ?int | ?string

struct TestConfig {
	timeout ?int
}

fn test_main() {
	mut r := TestConfig{}
	mut m := map[string]?Any{}
	m['timeout'] = r.timeout
	assert m.str() == "{'timeout': Option(Any(Option(none)))}"
}
