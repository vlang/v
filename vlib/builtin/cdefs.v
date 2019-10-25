module builtin

fn C.backtrace(a voidptr, b int) int

fn C.memcpy(byteptr, byteptr, int)
fn C.memmove(byteptr, byteptr, int)
//fn C.malloc(int) byteptr
fn C.realloc(a byteptr, b int) byteptr

fn C.qsort(voidptr, int, int, voidptr)

fn C.sprintf(a ...voidptr) byteptr
fn C.strlen(s byteptr) int



fn C.backtrace_symbols_fd(voidptr, int, int)



// Windows
fn C._setmode(int, int)
fn C._fileno(int) int


