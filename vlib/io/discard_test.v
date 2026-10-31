module io

fn test_discard() {
	mut d := Discard{}
	n := d.write('hello'.bytes())!
	assert n == 5
}
