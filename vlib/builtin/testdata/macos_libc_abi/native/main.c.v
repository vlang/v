module main

#flag @DIR/probe.c

fn C.v_macos_libc_native_mode() i32
fn C.v_macos_libc_native_probe(mode i32) i32

@[noreturn]
fn C._Exit(code i32)

fn abort_worker(mode i32) {
	C.v_macos_libc_native_probe(mode)
	C._Exit(93)
}

fn main() {
	// V assertions disappear in -prod. Keep both the BF seed and every probe
	// check as ordinary control flow in this debug/production native gate.
	if bf_close(-1) != 9 { C._Exit(93) }
	mode := C.v_macos_libc_native_mode()
	if mode == 119 { // w: handler must execute on this worker, using its TLS.
		t := spawn abort_worker(104)
		t.wait()
		C._Exit(93)
	} else if mode == 99 { // c: simultaneous abort callers, no global lock.
		first := spawn abort_worker(mode)
		second := spawn abort_worker(mode)
		first.wait()
		second.wait()
		C._Exit(93)
	} else {
		result := C.v_macos_libc_native_probe(mode)
		if result != 0 { C._Exit(result) }
		if bf_last_errno() != 9 { C._Exit(93) }
	}
}
