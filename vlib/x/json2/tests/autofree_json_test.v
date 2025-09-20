// vtest build: !sanitize-memory-gcc && !sanitize-address-gcc && !tests-sanitize-address-clang
// vtest vflags: -autofree
import x.json2

struct Config {
	bbb bool
}

fn test_compilation_with_autofree() {
	cfg := Config{}
	s := json2.encode(cfg, prettify: true)
	assert s == '{\n    "bbb": false\n}'
}
