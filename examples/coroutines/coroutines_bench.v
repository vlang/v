// vtest build: false // This should be build with: `v -use-coroutines coroutine_benchs.v`
// Note: the Photon wrapper is not yet trivial enough to build/install on the CI.
import coroutines
import time
import net.http
import sync

const run_time = 10 * time.second

fn request(mut mu sync.Mutex, count &int) {
	for {
		http.get('http://vlang.io/utc_now') or { panic(err) }
		mu.lock()
		unsafe {
			(*count)++
		}
		mu.unlock()
	}
}

fn main() {
	mut mu := sync.new_mutex()
	mut count := 0

	for _ in 0 .. 8 {
		go request(mut mu, &count)
	}
	$if is_coroutine ? {
		println('IS COROUTINE=true')
		coroutines.sleep(run_time)
	} $else {
		println('IS COROUTINE=false')
		time.sleep(run_time)
	}
	println('${count} requests made.')
}
