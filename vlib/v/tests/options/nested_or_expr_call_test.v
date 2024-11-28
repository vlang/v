fn get_name() !string {
	return error('failed')
}

fn test_nested_or_expr_call() {
	uid_map := map[int]string{}
	uid := 2
	username := if uid <= 0 {
		'unknown'
	} else {
		uid_map[uid] or {
			name := get_name() or { 'unknown' }
			name
		}
	}
	assert username == 'unknown'
	println('${uid} is ${username}')
}
