import net.http
import sync
import time

fn vlang_time(mut wg sync.WaitGroup) ?string {
	start := time.ticks()
	data := http.get('https://vlang.io/utc_now') ?
	finish := time.ticks()
	println('Finish getting time ${finish - start} ms')
	println(data.text)
	wg.done()
	return data.text
}

fn remote_ip(mut wg sync.WaitGroup) ?string {
	start := time.ticks()
	data := http.get('https://api.ipify.org') ?
	finish := time.ticks()
	println('Finish getting ip ${finish - start} ms')
	println(data.text)
	wg.done()
	return data.text
}

fn main() {
	mut wg := sync.new_waitgroup()
	wg.add(2)
	// Run tasks async
	go vlang_time(mut wg)
	go remote_ip(mut wg)
	wg.wait()
}
