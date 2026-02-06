const default_logger = Example{}

struct Example {
	structs []Another = [Another{}]
}

pub struct Another {
	function fn (string) = fn (value string) {
		println('${value}')
	}
}

pub fn (e Example) useless() string {
	return 'ok'
}

fn test_anon_fn_redefinition() {
	e1 := Example{}
	assert e1.useless() == 'ok'
	e2 := Example{}
	assert e2.useless() == 'ok'
	e3 := Example{}
	assert e3.useless() == 'ok'
}
