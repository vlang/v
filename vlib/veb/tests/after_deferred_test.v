// vtest build: !windows // fasthttp.Server.run is not implemented on windows yet
import net.http
import os
import sync
import time
import veb

const exit_after = time.second * 20
const port = 13010
const localserver = 'localhost:${port}'
const deferred_delay_ms = 40

struct DeferredEvent {
	thread_id u64
	time_ns   u64
}

struct DeferredFixture {
	events  chan DeferredEvent = chan DeferredEvent{cap: 2}
	release chan bool          = chan bool{cap: 1}
}

const deferred_fixture = &DeferredFixture{}

fn record_deferred_event() {
	deferred_fixture.events <- DeferredEvent{
		thread_id: sync.thread_id()
		time_ns:   time.sys_mono_now()
	}
}

pub struct Context {
	veb.Context
}

pub struct App {
mut:
	started chan bool
}

fn delayed_answer(mut ctx veb.Context, arg u64) {
	record_deferred_event()
	_ := <-deferred_fixture.release
	ctx.text('late:${arg}')
}

// before_accept_loop signals that the test server is ready to accept requests.
pub fn (mut app App) before_accept_loop() {
	app.started <- true
}

// delay schedules a response while releasing the request worker.
@['/delay']
pub fn (mut app App) delay(mut ctx Context) veb.Result {
	record_deferred_event()
	return ctx.after(deferred_delay_ms, delayed_answer, 7)
}

// fast responds immediately to test that a deferred request leaves the worker available.
@['/fast']
pub fn (mut app App) fast(mut ctx Context) veb.Result {
	return ctx.text('fast')
}

fn testsuite_begin() {
	spawn fn () {
		time.sleep(exit_after)
		assert true == false, 'timeout reached!'
		exit(1)
	}()
	os.chdir(os.dir(@FILE))!
	mut app := &App{}
	spawn veb.run_at[App, Context](mut app, port: port, timeout_in_seconds: 5)
	_ := <-app.started
}

fn test_deferred_response_runs_later_without_holding_the_worker() {
	request := spawn http.get('http://${localserver}/delay')
	handler := <-deferred_fixture.events
	callback := <-deferred_fixture.events

	// The callback is waiting for release, so the fast response must finish first.
	fast := http.get('http://${localserver}/fast')!
	deferred_fixture.release <- true
	response := request.wait()!

	assert callback.thread_id != handler.thread_id, 'callback ran on the request worker'
	elapsed := time.Duration(callback.time_ns - handler.time_ns)
	assert elapsed >= deferred_delay_ms * time.millisecond, 'callback ran after only ${elapsed}'
	assert fast.body == 'fast', 'unexpected fast body: ${fast.body}'
	assert response.body == 'late:7', 'unexpected deferred body: ${response.body}'
}
