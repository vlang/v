import json

struct Window {
pub mut:
	width  f64
	height f64
}

fn make_default_window_settings() Window {
	return Window{
		width: 1280
		height: 720
	}
}

struct Settings {
pub mut:
	window Window = make_default_window_settings()
}

fn test_encoding_works() {
	mut settings := Settings{}
	dump(settings)
	encoded := json.encode(settings)
	println(encoded)
	assert encoded == '{"window":{"width":1280,"height":720}}'
}
