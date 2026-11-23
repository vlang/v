import os
import time

const invalid_ticker_probe_arg = '--vtest-invalid-ticker'

fn testsuite_begin() {
	if os.args.len != 3 || os.args[1] != invalid_ticker_probe_arg {
		return
	}
	mut ticker := time.new_ticker(time.Duration(os.args[2].i64()))
	ticker.stop()
	exit(0)
}

fn test_ticker_rejects_nonpositive_durations() {
	executable := os.executable()
	for duration in ['0', '-1', '-9223372036854775808'] {
		result := os.exec([executable, invalid_ticker_probe_arg, duration])
		assert result.exit_code != 0, 'new_ticker accepted duration ${duration}'
		assert result.output.contains('non-positive interval for new_ticker'), result.output
	}
}
