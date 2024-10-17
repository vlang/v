module os

// sleep_ms suspends the execution for a given duration (in milliseconds), without having to `import time` for a single time.sleep/1 call.
fn sleep_ms(dur i64) {
	#let now = new Date().getTime()
	#let toWait = BigInt(dur)
	#while (new Date().getTime() < now + Number(toWait)) {}
}
