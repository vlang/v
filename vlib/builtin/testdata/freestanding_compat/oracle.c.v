module main

import os

#include "@DIR/probe.h"
#flag @DIR/format_probe.c

fn C.v_freestanding_format_oracle(path &char) i32
fn C.v_freestanding_format_contracts() i32

fn main() {
	assert os.args.len == 2
	$if freestanding {
		contracts := C.v_freestanding_format_contracts()
		if contracts != 0 {
			eprintln('format contract failed at C line ${contracts}')
		}
		assert contracts == 0
	}
	status := C.v_freestanding_format_oracle(&char(os.args[1].str))
	if status != 0 {
		eprintln('format oracle failed at C line ${status}')
	}
	assert status == 0
}
