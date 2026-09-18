// === Linux syscalls (shared) ===

fn C.mmap(addr voidptr, length usize, prot int, flags int, fd int, offset i64) voidptr
fn C.munmap(addr voidptr, length usize) int
fn C.close(fd int) int
fn C.read(fd int, buf voidptr, count usize) isize
fn C.pipe(fds &i32) i32
fn C.strcmp(s1 &char, s2 &char) int
fn C.strncmp(s1 &char, s2 &char, n usize) int
fn C.strlen(s &char) usize
fn C.strtok(str &char, delim &char) &char
fn C.strtol(str &char, endptr &&char, base int) i64
fn C.memset(s voidptr, c int, n usize) voidptr
fn C.atof(s &char) f64
