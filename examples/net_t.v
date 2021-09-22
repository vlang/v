import net.http
import sync
import time

fn send_request(mut wg sync.WaitGroup) ?string {
	start := time.ticks()
	data := http.get('https://google.com') ?
	finish := time.ticks()
	println('Finish getting time ${finish - start} ms')
	wg.done()
	return data.text
}

fn main() {
	mut wg := sync.new_waitgroup()
	for i := 0; i < 50; i++ {
		wg.add(1)
		go send_request(mut wg)
	}
	wg.wait()
}
