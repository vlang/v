module benchmark

pub struct Benchmark {
	//TODO Change this to an embedded struct when it works properly
pub mut:
	concurrent ConcurrentBenchmark
	instance   ?ConcurrentInstance
}

pub fn new_benchmark() Benchmark {
	return Benchmark{
		concurrent: new_concurrent_benchmark()
		instance: none
	}
}

pub fn (b mut Benchmark) set_total_expected_steps(n int) {
	b.concurrent.set_total_expected_steps(n)
}

pub fn (b mut Benchmark) stop() {
	b.concurrent.stop()
}

pub fn (b mut Benchmark) step() {
	b.instance = b.concurrent.step()
}

pub fn (b mut Benchmark) fail() {
	mut instance := b.instance or {
		panic("could not fail when there's no step")
	}
	instance.fail()
}

pub fn (b mut Benchmark) ok() {
	mut instance := b.instance or {
		panic("could not ok when there's no step")
	}
	instance.ok()
}

pub fn (b mut Benchmark) fail_many(n int) {
	mut instance := b.instance or {
		panic("could not fail when there's no step")
	}
	instance.fail_many(n)
}

pub fn (b mut Benchmark) ok_many(n int) {
	mut instance := b.instance or {
		panic("could not ok when there's no step")
	}
	instance.ok_many(n)
}

pub fn (b mut Benchmark) neither_fail_nor_ok() {
	mut instance := b.instance or {
		panic("could not set status when there's no step")
	}
	instance.neither_fail_nor_ok()
}

pub fn (b &Benchmark) step_message_with_label(label string, msg string) string {
	mut instance := b.instance or {
		panic("could not print step message when there's no step")
	}
	return instance.step_message_with_label(label, msg)
}

pub fn (b &Benchmark) step_message(msg string) string {
	mut instance := b.instance or {
		panic("could not print step message when there's no step")
	}
	return instance.step_message(msg)
}

pub fn (b &Benchmark) step_message_ok(msg string) string {
	mut instance := b.instance or {
		panic("could not print step message when there's no step")
	}
	return instance.step_message_ok(msg)
}

pub fn (b &Benchmark) step_message_fail(msg string) string {
	mut instance := b.instance or {
		panic("could not print step message when there's no step")
	}
	return instance.step_message_fail(msg)
}

pub fn (b &Benchmark) total_message(msg string) string {
	return b.concurrent.total_message(msg)
}

pub fn (b &Benchmark) total_duration() i64 {
	return b.concurrent.total_duration()
}

// //////////////////////////////////////////////////////////////////
fn (b &Benchmark) tdiff_in_ms(s string, sticks i64, eticks i64) string {
	return b.concurrent.tdiff_in_ms(s, sticks, eticks)
}
