import time
import net.http

fn main() {
	resp := http.get('https://vlang.io/utc_now') or {
		eprintln('Failed to fetch data from the server. Error: $err')
		return
	}

	t := time.unix(resp.body.int())
	println(t.format())
}
