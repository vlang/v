import os

println('Press Ctrl+D(Linux) or Ctrl+Z(Windows) at line begin to exit')
mut i := 0
for {
	i += 1
	mut line := os.get_raw_line()
	if line.len == 0 {
		break
	}
	println('${i}: ${line}')
}
