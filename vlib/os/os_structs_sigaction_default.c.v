module os

pub type FN_SA_Handler = fn (sig int)

struct C.sigaction {
mut:
	sa_mask      int
	sa_sigaction int
	sa_flags     int
	sa_handler   FN_SA_Handler
}
