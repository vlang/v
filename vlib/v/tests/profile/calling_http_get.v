import net.http

fn main() {
	if resp := http.get('http://127.0.0.1:56713/unknown_page') {
		println(resp.text)
	} else {
		println(err)
	}
	assert true
}
