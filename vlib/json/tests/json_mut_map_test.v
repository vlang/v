import json

fn q_and_a(mut db_json map[string][]string) {
	x := json.encode(db_json)
	assert x == '{}'
}

fn test_main() {
	mut db_json := json.decode(map[string][]string, '{}')!
	assert db_json == {}
	q_and_a(mut db_json)
}
