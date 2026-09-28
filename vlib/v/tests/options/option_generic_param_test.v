// vtest vflags: -w
import json2

type Elem = int | ?int

const empty = ?int(none)
const array = [Elem(1), Elem(empty), 3]

fn test_main() {
	dump(array)
	assert dump(json2.encode(array)) == '[1,{},3]'
}
