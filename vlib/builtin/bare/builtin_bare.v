module builtin

pub fn print(s string) {
	write(1, s.str, u64(s.len))
}

pub fn println(s string) {
	print(s)
	write(1, "\n".str, 1)
}

// replaces panic when -debug arg is passed
fn panic_debug(line_no int, file,  mod, fn_name, s string) {
	println('================ V panic ================')
	print('   module: ')
	println('mod')
	print(' function: ')
	print(fn_name)
	println('()')
	println('     file: ')
	println(file)
	//println('     line: ${line_no}')
	print('  message: ')
	println(s)
	println('=========================================')
	exit(1)
}

pub fn panic(s string) {
	print('V panic: ')
	println(s)
	exit(1)
}
