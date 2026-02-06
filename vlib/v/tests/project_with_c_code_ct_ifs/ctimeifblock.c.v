$if linux {
	#include "@VMODROOT/a_linux.h"
}

$if !linux {
	#include "@VMODROOT/a_nonlinux.h"
}

fn main() {
	C.printf(c'a: %s\n', C.version)
}
