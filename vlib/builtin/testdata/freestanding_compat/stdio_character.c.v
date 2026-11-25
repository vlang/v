module main

import os

#flag @DIR/stdio_character_probe.c

fn C.v_stdio_character_prepare(&char) int
fn C.v_stdio_character_contender() int
fn C.v_stdio_character_release() int
fn C.v_stdio_character_finish(&char) int

fn contend() int { return C.v_stdio_character_contender() }

fn require(code int) {
	if code != 0 {
		eprintln('stdio character probe failed at C line ${code}')
		exit(1)
	}
}

fn main() {
	if os.args.len != 2 { exit(2) }
	path := os.args[1]
	require(C.v_stdio_character_prepare(&char(path.str)))
	worker := spawn contend()
	require(C.v_stdio_character_release())
	result := worker.wait()
	require(result)
	require(C.v_stdio_character_finish(&char(path.str)))
	println('stdio character ownership passed')
}
