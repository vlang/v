import wit
import os

fn main() {
	if os.args.len < 2 {
		println('usage: conform_test <file.wit>')
		return
	}
	src := os.read_file(os.args[1]) or { println('READERR: ${err}'); return }
	doc := wit.parse(src) or { println('PARSEERR: ${err}'); return }
	println('PARSE_OK interfaces=${doc.interfaces.len} worlds=${doc.worlds.len}')
	printed := doc.str()
	println('PRINT_OK len=${printed.len}')
	generated := doc.to_v()
	println('GEN_OK len=${generated.len}')
}