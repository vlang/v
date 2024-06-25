struct Parse {
mut:
	stack []&Element
}

struct Balise {}

struct RawText {
	s string
}

type Element = Balise | RawText

fn (mut p Parse) process_open_tag() string {
	mut last := &p.stack[0]
	if mut last is RawText {
		println(last)
		return last.s
	} else {
		return ''
	}
}

fn test_sumtype_with_reference() {
	mut parse := Parse{
		stack: [&RawText{'raw'}]
	}
	assert parse.process_open_tag() == 'raw'
}
