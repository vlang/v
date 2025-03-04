module main

type Float = f32

struct Note {
mut:
	seen string
}

fn (mut n Note) bad_call(value ?Float) {
	if val := value {
		n.seen += ', ${val}'
	}
}

fn (mut n Note) good_call(value ?f32) {
	if val := value {
		n.seen += ', ${val}'
	}
}

fn test_main() {
	mut good := Note{}
	mut bad := Note{}
	mut c := 0
	for {
		good.good_call(f32(c))
		bad.bad_call(Float(c))
		c += 1
		if c >= 5 {
			break
		}
	}
	assert '${good.seen}' == '${bad.seen}'
}
