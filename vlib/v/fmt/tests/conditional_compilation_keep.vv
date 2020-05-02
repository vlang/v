// __global g_my_global_variable int
fn main() {
	$if tinyc {
		println('This will be compiled only when the compiler is tcc')
	}
	$if !windows {
		println('This will get compiled on non-windows platforms.')
	}
	//
	$if !linux {
		println('Only non linux platforms will get this')
	} $else {
		println('This part is for linux only.')
	}
	//
	$if network ? {
		println('This will be run, only when the program is compiled with `-d network`')
	}
}
