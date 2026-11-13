import os

fn test_ticker_rejects_nonpositive_durations() {
	root := os.join_path(os.vtmp_dir(), 'ticker_invalid_${os.getpid()}')
	os.mkdir_all(root) or { panic(err) }
	defer {
		os.rmdir_all(root) or {}
	}
	source := os.join_path(root, 'ticker_invalid.v')
	executable := os.join_path(root, 'ticker_invalid' + if os.user_os() == 'windows' {
		'.exe'
	} else {
		''
	})
	os.write_file(source, 'import os
import time

mut ticker := time.new_ticker(time.Duration(os.args[1].i64()))
ticker.stop()
') or { panic(err) }
	build := os.exec([@VEXE, '-o', executable, source])
	assert build.exit_code == 0, build.output
	for duration in ['0', '-1', '-9223372036854775808'] {
		result := os.exec([executable, duration])
		assert result.exit_code != 0, 'new_ticker accepted duration ${duration}'
		assert result.output.contains('non-positive interval for new_ticker'), result.output
	}
}
