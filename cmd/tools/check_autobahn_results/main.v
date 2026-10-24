module main

import os
import strings

fn count_field(content string, field string, value string) int {
	mut n := 0
	mut pos := 0
	needle := '"' + field + '": "' + value + '"'
	for {
		idx := content.index_after(needle, pos) or { -1 }
		if idx < 0 { break }
		n++
		pos = idx + needle.len
	}
	return n
}


fn main() {
	if os.args.len < 2 {
		eprintln('usage: check_autobahn_results <reports_dir>')
		exit(1)
	}
	root := os.args[1]
	clients_path := os.join_path(root, 'clients', 'index.json')
	servers_path := os.join_path(root, 'servers', 'index.json')

	clients_content := os.read_file(clients_path) or {
		eprintln('cannot read ${clients_path}: ${err}')
		exit(1)
	}
	servers_content := os.read_file(servers_path) or {
		eprintln('cannot read ${servers_path}: ${err}')
		exit(1)
	}

	// count tests: number of "behavior" keys
	mut ct := 0
	mut pos := 0
	for {
		idx := clients_content.index_after('"behavior"', pos) or { -1 }
		if idx < 0 { break }
		ct++
		pos = idx + 10
	}
	mut st := 0
	pos = 0
	for {
		idx := servers_content.index_after('"behavior"', pos) or { -1 }
		if idx < 0 { break }
		st++
		pos = idx + 10
	}

	ce := count_field(clients_content, 'behavior', 'FAILED') + count_field(clients_content, 'behaviorClose', 'FAILED')
	se := count_field(servers_content, 'behavior', 'FAILED') + count_field(servers_content, 'behaviorClose', 'FAILED')

	println('CLIENT ERRORS ${ce} of ${ct}, SERVER ERRORS ${se} of ${st}')
	if ce > 0 || se > 0 {
		exit(1)
	}
}
