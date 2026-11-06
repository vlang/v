import time
import net.http

fn main() {
	config := http.FetchConfig{
		url:       'https://time.akamai.com/'
		use_tls13: true
	}
	resp := http.fetch(config) or {
		eprintln('Failed to fetch data from the server. Error: ${err}')
		return
	}

	t := time.unix(resp.body.int())
	println(t.format())
}
