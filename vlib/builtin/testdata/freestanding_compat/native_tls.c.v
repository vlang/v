module main

import net.http
import os
import time

#include "@DIR/native_tls_probe.h"
#flag @DIR/native_tls_probe.c
#flag -lcurl
#flag -lvlibc_mbedcrypto
#flag -larchive
#flag -lgpgme
#flag -lgpg-error
#flag -lseccomp

fn C.v_native_libraries_prepare() i32
fn C.v_native_libraries_fetch(&char, &char) i32
fn C.v_native_libraries_formats() i32
fn C.v_native_libraries_verify(&char, &char) i32
fn C.v_native_libraries_sandbox() i32
fn C.v_native_libraries_reject_certificate(&char, &char) i32
fn C.v_native_libraries_finish()

fn require_native(code i32) {
	if code != 0 {
		eprintln('native library regression failed at C line ${code}')
		exit(1)
	}
}

fn native_fetch(url string, certificate string) i32 {
	return C.v_native_libraries_fetch(&char(url.str), &char(certificate.str))
}

fn v_fetch(url string, certificate string) {
	response := http.fetch(url: url, verify: certificate, validate: true) or {
		eprintln(err)
		exit(1)
	}
	if response.status_code != 200 || response.body != 'native HTTPS body\n' { exit(1) }
}

fn main() {
	if os.args.len == 2 && os.args[1] == 'sandbox' {
		require_native(C.v_native_libraries_sandbox())
		println('native seccomp enforcement passed')
		return
	}
	if os.args.len != 6 { exit(2) }
	require_native(C.v_native_libraries_prepare())
	defer { C.v_native_libraries_finish() }
	gate := os.getenv('V_NATIVE_PROBE_GATE')
	if gate != '' {
		println('native library fixture ready')
		flush_stdout()
		for !os.exists(gate) { time.sleep(5 * time.millisecond) }
	}
	url := os.args[1]
	certificate := os.args[2]
	mut native_workers := []thread i32{}
	mut v_workers := []thread{}
	for _ in 0 .. 2 {
		native_workers << spawn native_fetch(url, certificate)
		v_workers << spawn v_fetch(url, certificate)
	}
	for worker in native_workers { require_native(worker.wait()) }
	v_workers.wait()
	require_native(C.v_native_libraries_reject_certificate(&char(url.str), &char(os.args[5].str)))
	if _ := http.fetch(url: url, verify: os.args[5], validate: true) {
		eprintln('V HTTPS accepted an untrusted certificate')
		exit(1)
	}
	require_native(C.v_native_libraries_formats())
	require_native(C.v_native_libraries_verify(&char(os.args[3].str), &char(os.args[4].str)))
	println('independent native/V TLS, compression, GPG verification and seccomp APIs passed')
}
