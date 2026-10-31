// vtest vflags: -w
import json2

fn q_and_a(mut db_json map[string][]string) {
	x := json2.encode(db_json, escape_unicode: true)
	assert x == '{}'
}

fn test_main() {
	mut db_json := json2.decode[map[string][]string{}]('{}')!
	assert db_json == {}
	q_and_a(mut db_json)
}
