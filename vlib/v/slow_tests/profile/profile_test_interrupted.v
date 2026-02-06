import time

unbuffer_stdout()
for i in 1 .. 9999 {
	time.sleep(10 * time.millisecond)
	println('${i:04} iteration')
}
