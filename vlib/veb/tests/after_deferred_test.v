// vtest build: !windows // fasthttp.Server.run is not implemented on windows yet
import net.http
import os
import time
import veb

const exit_after = time.second * 20
const port = 13010
const localserver = 'localhost:${port}'

pub struct Context {
	veb.Context
}

pub struct App {
mut:
	started chan bool
}

fn delayed_answer(mut ctx veb.Context, arg u64) {
	ctx.text('late:${arg}')
}

pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

@['/delay']
pub fn (mut app App) delay(mut ctx Context) veb.Result {
	return ctx.after(400, delayed_answer, 7)
}

@['/fast']
pub fn (mut app App) fast(mut ctx Context) veb.Result {
	return ctx.text('fast')
}

fn testsuite_begin() {
	os.chdir(os.dir(@FILE))!
	mut app := &App{}

	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 5)
	_ := <-app.started

	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
}

fn test_deferred_response_arrives_no_sooner_than_ms() {
	start := time.ticks()
	resp := http.get('http://${localserver}/delay')!
	elapsed := time.ticks() - start
	assert elapsed >= 350, 'deferred response arrived after ${elapsed}ms, want >= 350ms'
	assert resp.body == 'late:7', 'unexpected deferred body: ${resp.body}'
}

fn test_deferred_handler_does_not_hold_the_worker() {
	ch := chan string{cap: 1}
	spawn fn (c chan string) {
		r := http.get('http://${localserver}/delay') or {
			c <- 'error: ${err}'
			return
		}
		c <- r.body
	}(ch)
	// Let the deferred handler arm its timer, then a fast route must still
	// answer at once: the worker was freed on return.
	time.sleep(100 * time.millisecond)
	fast_start := time.ticks()
	fast := http.get('http://${localserver}/fast')!
	fast_elapsed := time.ticks() - fast_start
	assert fast.body == 'fast', 'unexpected fast body: ${fast.body}'
	assert fast_elapsed < 300, 'worker was held for ${fast_elapsed}ms by the deferred handler'
	body := <-ch
	assert body == 'late:7', 'unexpected deferred body: ${body}'
}
