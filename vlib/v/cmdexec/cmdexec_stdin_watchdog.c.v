module cmdexec

#include <stdlib.h>

fn C._Exit(code int)

fn stdin_eof_watchdog_exit(code int) {
	C._Exit(code)
}
