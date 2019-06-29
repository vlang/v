// Copyright (c) 2019 Alexander Medvednikov. All rights reserved.
// Use of this source code is governed by an MIT license
// that can be found in the LICENSE file.

import http

fn main() {
	html := http.get('https://news.ycombinator.com')
	mut pos := 0
	for {
		pos = html.index_after('https://', pos + 1)
		if pos == -1 {
			break
		}
		end := html.index_after('"', pos)
		println(html.substr(pos, end))
	}
}

