import net.http

fn main() {
	html := http.get_text('https://news.ycombinator.com')
	mut pos := 0
	for {
		pos = html.index_after('https://', pos + 1) or { -1 }
		if pos == -1 {
			break
		}
		end := html.index_after('"', pos) or { html.len }
		println(html[pos..end])
	}
}
