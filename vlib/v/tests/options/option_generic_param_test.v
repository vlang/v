import json
import x.json2

type Elem = int | ?int

const empty = ?int(none)
const array = [Elem(1), Elem(empty), 3]

fn test_main() {
	dump(array)
	e := json.encode(array)
	assert dump(e) == '[1,{},3]'
	assert dump(json2.encode(array)) == '[1,,3]'
}
