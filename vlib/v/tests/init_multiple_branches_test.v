// fixes https://github.com/vlang/v/issues/11564, test is copied from the code by https://github.com/ken0x0a and formatted
import x.json2

struct SA {}

struct SB {}

struct SC {}

type Sum = SA | SB | SC

struct App {
	s map[string]Sum
	t bool
}

fn test_init_multiple_branches() {
	mut m := map[string]json2.Any{}
	app := App{
		t: m['t'] or { 0 }.bool()
		s: if a := m['a'] {
			println('a => ${a}')
			b := return_optional(a)? // Fails only if the expr in this line has or_block (or_block kind (.propagation or .block) doesn't matter)
			b
		} else {
			map[string]Sum{}
		}
	}
	println('app => ${app}')
}

fn return_optional(input json2.Any) ?map[string]Sum {
	return map[string]Sum{}
}
