[wasm_import_namespace: 'wasi_snapshot_preview1']
module builtin

struct CIOVec {
	buf &u8
	len usize
}

type Errno = u16
type FileDesc = int

fn WASM.fd_write(fd FileDesc, iovs &CIOVec, iovs_len usize, retptr &usize) Errno
[noreturn]
fn WASM.proc_exit(rval int)
