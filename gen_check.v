import os
import wit

fn main() {
	src := os.read_file(os.args[1]) or { println('READERR: ${err}'); return }
	doc := wit.parse(src) or { println('PARSEERR: ${err}'); return }
	out := doc.to_v()
	os.write_file(os.args[2], out) or { println('WRITEERR: ${err}'); return }
	println('GEN_OK ${out.len} bytes')
}