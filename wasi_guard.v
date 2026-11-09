module main

fn main() {
	$if wasm32 {
		println('wasm')
	} $else {
		println('not wasm')
	}
}