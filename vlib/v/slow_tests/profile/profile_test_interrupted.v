import time

fn format_iteration(i int) string {
	return '${i:04} iteration'
}

unbuffer_stdout()
for i in 1 .. 9999 {
	time.sleep(10 * time.millisecond)
	println(format_iteration(i))
}
