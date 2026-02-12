module atomics

$if prod && (gcc || clang) {
	#flag -Wl,--undefined=panicUnaligned
}

@[export: 'panicUnaligned']
fn panic_unaligned() {
	panic('unaligned atomic operation')
}
