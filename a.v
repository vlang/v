fn main() {
	$if !wasm {
		println("teste")
	} $else {
		println("no")
	}
}