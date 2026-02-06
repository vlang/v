import net.http
import sync
import time

fn vlang_time(mut wg sync.WaitGroup) !string {
	start := time.ticks()
	data := http.get('https://vlang.io/utc_now')!
	finish := time.ticks()
	println('Finish getting time ${finish - start} ms')
	println(data.body)
	wg.done()
	return data.body
}

fn remote_ip(mut wg sync.WaitGroup) !string {
	start := time.ticks()
	data := http.get('https://api.ipify.org')!
	finish := time.ticks()
	println('Finish getting ip ${finish - start} ms')
	println(data.body)
	wg.done()
	return data.body
}

fn main() {
	mut wg := sync.new_waitgroup()
	wg.add(2)
	// Run tasks async
	spawn vlang_time(mut wg)
	spawn remote_ip(mut wg)
	wg.wait()
}
