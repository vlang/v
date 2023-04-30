module main

import net.http
import os
import clipboard
import json

struct Response {
	hash  string
	error string
}

fn main() {
	mut cb := clipboard.new()

	if os.args.len < 3 {
		eprintln('Please provide a file')
		exit(1)
	}

	if os.file_ext(os.args[2]) != '.v' {
		eprintln('Must be a V source file.')
		exit(1)
	}

	if !os.is_file(os.args[2]) {
		eprintln('File not found.')
		exit(1)
	}

	to_send := os.args[2]

	content := os.read_file(to_send) or {
		eprintln(err)
		exit(1)
	}

	share := http.post_form('https://play.vlang.io/share', {
		'code': content
	})!

	response := json.decode(Response, share.body)!
	url := 'https://play.vlang.io/p/${response.hash}'

	cb.copy(url)
	println(url)
	println('Copied URL to clipboard.')
}
