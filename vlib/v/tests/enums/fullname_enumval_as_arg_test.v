import os

fn test_main() {
	wrap_reset := '\e[?7l'
	os.signal_opt(os.Signal.int, fn [wrap_reset] (sig os.Signal) {
		print(wrap_reset)
		exit(0)
	}) or {}
	assert true
}
