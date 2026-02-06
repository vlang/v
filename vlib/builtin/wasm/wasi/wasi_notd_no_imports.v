module builtin

pub struct CIOVec {
pub:
	buf &u8
	len usize
}

type Errno = u16
type FileDesc = int

@[wasm_import_namespace: 'wasi_snapshot_preview1']
fn WASM.args_sizes_get(argc &u32, argc_buf_size &u32) Errno

@[wasm_import_namespace: 'wasi_snapshot_preview1']
fn WASM.args_get(argv &&u8, argv_buf &u8) Errno

@[wasm_import_namespace: 'wasi_snapshot_preview1']
fn WASM.fd_write(fd FileDesc, iovs &CIOVec, iovs_len usize, retptr &usize) Errno

@[wasm_import_namespace: 'wasi_snapshot_preview1']
fn WASM.fd_read(fd FileDesc, iovs &CIOVec, iovs_len usize, nread &usize) Errno

@[wasm_import_namespace: 'wasi_snapshot_preview1']
fn WASM.fd_sync(fd FileDesc) Errno

@[wasm_import_namespace: 'wasi_snapshot_preview1']
fn WASM.random_get(buf &u8, buf_len usize) Errno

@[wasm_import_namespace: 'wasi_snapshot_preview1']
@[noreturn]
fn WASM.proc_exit(rval int)
