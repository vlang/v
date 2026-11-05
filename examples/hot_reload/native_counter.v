module main

// Native-backend hot reloading experiment (macOS ARM64 only).
//
// Run with: v -b arm64 run native_counter.v, then modify `next_value`
// below while it is running to see the change take effect.
//
// Unlike message.v (which needs calls and string literals and is only
// supported by the C shared-library workflow), this example stays
// self-contained so the native reloader can extract it: the hot
// function below uses only arithmetic on its argument.
import time

@[live]
fn next_value(n int) int {
	return n * 2 + 1
}

fn main() {
	mut n := 1
	for {
		n = next_value(n)
		println(n)
		time.sleep(500 * time.millisecond)
	}
}
