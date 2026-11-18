module os

$if !windows {
	#include <signal.h>
}

// C.sigaction is declared here for the V checker; the C compiler uses <signal.h>.
pub type FN_SA_Handler = fn (sig int)

pub struct C.sigaction {
mut:
	sa_mask      int
	sa_sigaction int
	sa_flags     int
	sa_handler   FN_SA_Handler
}
