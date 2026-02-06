module builtin

// called by the generated main/init
fn init() {
}

pub fn isnil(p voidptr) bool {
	return p == 0
}

pub fn print(s string) {
	sys_write(1, s.str, u64(s.len))
}

pub fn println(s string) {
	print(s)
	print('\n')
}

pub fn panic(s string) {
	eprint('V panic: ')
	eprintln(s)
	sys_exit(1)
}

// replaces panic when -debug arg is passed
fn panic_debug(line_no int, file string, mod string, fn_name string, s string) {
	eprintln('================ V panic ================')
	eprint('   module: ')
	eprintln('mod')
	eprint(' function: ')
	eprint(fn_name)
	eprintln('()')
	eprintln('     file: ')
	eprintln(file)
	// println('     line: ${line_no}')
	eprint('  message: ')
	eprintln(s)
	eprintln('=========================================')
	sys_exit(1)
}

pub fn eprint(s string) {
	if isnil(s.str) {
		panic('eprint(NIL)')
	}
	sys_write(2, s.str, u64(s.len))
}

pub fn eprint_ln(s string) {
	eprint(s)
	eprint('\n')
}

pub fn eprintln(s string) {
	if isnil(s.str) {
		panic('eprintln(NIL)')
	}
	eprint_ln(s)
}
