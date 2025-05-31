import net.http

fn main() {
	html := http.get_text('https://news.ycombinator.com')
	mut pos := 0
	for {
		pos = html.index_after('https://', pos + 1) or { break }
		end := html.index_after('"', pos) or { break }
		println(html[pos..end])
	}
}
