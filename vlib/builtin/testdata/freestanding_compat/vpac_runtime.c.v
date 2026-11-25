module main

#include "@DIR/vpac_runtime_probe.h"
#flag @DIR/vpac_runtime_probe.c
#flag -Wl,--wrap=connect
#flag -Wl,--export-dynamic

fn C.v_vpac_runtime_probe(&char) i32
fn C.getuid() u32
fn C.setuid(u32) i32

fn raw_credential_probe(argument u64) {
	valid := C.setuid(C.getuid()) == -1 && C.errno == C.ENOTSUP
	unsafe { *(&i32(argument)) = if valid { i32(1) } else { i32(-1) } }
}

fn main() {
	result := C.v_vpac_runtime_probe(c'/tmp')
	if result != 0 {
		eprintln('native runtime regression failed at C line ${result}')
		exit(1)
	}
	mut raw_result := i32(0)
	mut worker, code := vlibc_thread_spawn(raw_credential_probe, u64(&raw_result))
	if int(code) != 0 { exit(1) }
	if int(vlibc_thread_join(mut worker)) != 0 || raw_result != 1 || C.setuid(C.getuid()) != 0 {
		exit(1)
	}
}
