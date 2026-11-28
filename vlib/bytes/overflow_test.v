module bytes

import os

const bytes_overflow_probe_arg = '--vtest-bytes-overflow'

fn testsuite_begin() {
	if os.args.len == 2 && os.args[1] == bytes_overflow_probe_arg {
		println(repeat([u8(1), 2], 2147483647))
		exit(0)
	}
}

fn test_repeat_rejects_length_overflow() {
	result := os.exec([os.executable(), bytes_overflow_probe_arg])
	assert result.exit_code != 0
	assert result.output.contains('bytes.repeat: length overflow'), result.output
}
