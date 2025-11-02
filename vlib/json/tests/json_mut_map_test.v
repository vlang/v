import json

fn q_and_a(mut db_json map[string][]string) {
	json.encode(db_json)
}

fn test_main() {
	mut db_json := json.decode(map[string][]string, '{}')!
	q_and_a(mut db_json)
}
