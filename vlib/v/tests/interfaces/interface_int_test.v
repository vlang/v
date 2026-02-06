module main

interface Value {}

fn test_main() {
	mut a := []string{len: 10}
	mut v := []Value{}
	for i := 0; i < a.len; i++ {
		v << i
	}
	assert v[0] == Value(0)
}
