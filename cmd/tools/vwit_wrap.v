import os
import wit
import wasm

fn main() {
	if os.args.len < 5 {
		println('usage: v run cmd/tools/vwit_wrap.v <core.wasm> <input.wit> <world> <out.component.wasm>')
		return
	}
	core := os.read_file(os.args[1]) or { panic(err) }
	src := os.read_file(os.args[2]) or { panic(err) }
	doc := wit.parse(src) or { panic(err) }
	out := wit.wrap_component(core.bytes(), doc, os.args[3])
	os.write_file_array(os.args[4], out) or { panic(err) }
	println('wrote ${os.args[4]} (${out.len} bytes)')
	_ = wasm.is_component
}
