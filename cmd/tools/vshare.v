module main

import net.http
import os
import vshare

fn main() {
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
	}) or {
		eprintln('Failed to share code: ${err.msg()}')
		exit(1)
	}

	url := vshare.share_url_from_response(share.status_code, share.body) or {
		eprintln(err.msg())
		exit(1)
	}

	println(url)
	if vshare.copy_to_clipboard(url) {
		println('Copied URL to clipboard.')
	} else {
		println('Clipboard copy unavailable. Copy the URL manually.')
	}
}
